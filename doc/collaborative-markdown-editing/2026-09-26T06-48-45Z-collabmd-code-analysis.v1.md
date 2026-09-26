2026-09-26T06-48-45Z-collabmd-code-analysis.v1.md
project: tsk
# CollabMD code analysis

Repository: https://github.com/andes90/collabmd
Commit read: `39d299a` (2026-09-25), default branch `master`.
Local clone: `/tmp/claude-0/-home-user-dotfiles/03bb94cf-f466-5710-82dd-fe31304de24a/scratchpad/repos/collabmd`
Permalink base: `https://github.com/andes90/collabmd/blob/39d299ae12298a60b5148679b6b0f1c0660b6ddc/`

## Short answer

CollabMD stores comments in JSON sidecar files next to the vault, under `.collabmd/comments/<path>.json`. It stores Yjs document state under `.collabmd/yjs/<path>.bin`. On start-up it adds `.collabmd/` to `.git/info/exclude`. Comments and Yjs state therefore never land in git. Version history is plain git history of the markdown files. Commits are manual: a user stages and commits from the browser, and the server runs the `git` CLI. There is no suggestion mode.

## 1. Tech stack

| Item | Value | Evidence |
|---|---|---|
| Language | JavaScript (ES modules), no TypeScript | `package.json:4` (`"type": "module"`) |
| Runtime | Node.js >= 26 (pinned 26.7.0) | `package.json:64-66`, `.tool-versions` |
| Server framework | None. Raw `node:http` handlers plus `ws` | `src/server/infrastructure/http/create-request-handler.js`, `src/server/infrastructure/websocket/attach-collaboration-gateway.js:1` |
| Client framework | None for the main app (vanilla JS, layered as domain/application/infrastructure/presentation). React 19 only for the embedded Excalidraw editor | `src/client/`, `package.json` devDependencies |
| Build | Vite 8 | `package.json:125`, `vite.config.mjs` |
| Tests | `node --test`, Vitest browser mode (Chromium), Playwright e2e | `package.json:44-54` |
| Package manager | npm (`package-lock.json`) | repository root |
| Distribution | npm (`npx collabmd`), Homebrew tap, Docker image | `.github/workflows/npm-publish.yml`, `homebrew-tap-release.yml`, `docker-publish.yml`, `Dockerfile` |

## 2. Key libraries

| Concern | Library | Evidence |
|---|---|---|
| Editor | CodeMirror 6 (`@codemirror/*`), source-mode markdown; optional Vim bindings | `src/client/infrastructure/editor-session.js:1-44`, `:700` |
| Editor to CRDT binding | `y-codemirror.next` (`yCollab`) | `src/client/infrastructure/editor-session.js:44` |
| CRDT | Yjs 13 | `package.json:83` |
| Sync protocol | `y-protocols/sync` and `y-protocols/awareness` on the server; `y-websocket` provider on the client | `src/server/domain/collaboration/collaboration-room.js:1-5`, `src/client/infrastructure/editor-collaboration-client.js:1,59` |
| Transport | WebSocket (`ws` on the server) | `package.json:81`, `attach-collaboration-gateway.js:1` |
| Markdown renderer | markdown-it 15, KaTeX, highlight.js, Mermaid; PlantUML and Structurizr through server endpoints | `src/client/application/preview-render-compiler.js:1`, `package.json` |
| Persistence | Plain `fs/promises` for files and sidecars; `node:sqlite` (`DatabaseSync`) only for hosted-mode metadata and MCP agent connections | `src/server/infrastructure/persistence/sidecar-store.js`, `hosted-metadata-store.js:123-124`, `agent-connection-store.js:40-41` |
| Git | `git` CLI through `child_process.execFile`; no git library | `src/server/infrastructure/git/git-service.js:1,16` |
| File watching | `node:fs` `watch(..., { recursive: true })`; no chokidar | `src/server/infrastructure/workspace/file-system-sync-service.js:1,74` |
| Auth | `openid-client` (Google OIDC), signed session cookie for password mode | `package.json:79`, `src/server/auth/` |

The UI is a file tree, a CodeMirror source pane and a rendered preview pane (editor, split and preview views). It is Obsidian-like in structure (vault, wiki-links, backlinks, quick switcher). It is not a WYSIWYG block editor in the Notion style.

## 3. Comment design

### Data model

`src/domain/comment-threads.js` defines the model. Each file's Yjs document holds a `Y.Array` named `comments`. Each entry is a `Y.Map` thread (`createCommentThreadSharedType`, lines 310-352) with:

- `id`, `createdAt`, `createdByName`, `createdByColor`, `createdByPeerId`
- `anchorKind`: `line`, `text` or `diagram-element` (line 8)
- text anchors: `anchorStart`, `anchorEnd` (Yjs relative positions as JSON), `anchorStartLine`, `anchorEndLine`, `anchorQuote` (up to 280 characters, line 5)
- diagram anchors: `elementId`, `anchorPoint`, `anchorSnapshot` (lines 273-288)
- `messages`: a `Y.Array` of `{ id, body, createdAt, peerId, userName, userColor, reactions }` (lines 203-218); body limit 2000 characters (line 3)
- `resolvedAt`, `resolvedBy*` fields
- reactions as separate map keys `reaction:[messageId, emoji, userId]` so concurrent reactions do not conflict (lines 135-137, 228-261)

### How anchors attach

`EditorViewAdapter.getCurrentSelectionCommentAnchor` (`src/client/infrastructure/editor-view-adapter.js:900-935`) builds the anchor. A collapsed cursor produces a `line` anchor over the whole line. A selection produces a `text` anchor over the exact character range, so a single word is supported.

`CommentThreadStore.createCommentThread` (`src/client/infrastructure/comment-thread-store.js:138-190`) converts the start and end indices into `Y.createRelativePositionFromTypeIndex(ytext, index)` and stores them as JSON (lines 159-169).

### How anchors survive edits

`resolveCommentThread` (`comment-thread-store.js:293-339`) converts each relative position back to an absolute index with `Y.createAbsolutePositionFromRelativePosition` (lines 341-357). Yjs relative positions reference CRDT item IDs, so they track concurrent and local edits inside a live Yjs document.

If a relative position does not resolve, the code falls back to the stored `anchorStartLine` / `anchorEndLine` (lines 306-311). These line numbers are written at creation and are not updated afterwards. `anchorQuote` is not used to re-anchor in the editor. The preview uses it only to highlight a unique text match in rendered HTML (`findUniqueQuoteRange`, `src/client/presentation/comment-ui/comment-ui-shared.js:333-350`).

The relative positions are only valid against the Yjs history that created them. That history lives in `.collabmd/yjs/<path>.bin`. Two paths break it:

1. An external change to a file (editor outside CollabMD, `git pull`, `git checkout`) triggers `reconcileCollaborationSnapshots`, which deletes the Yjs snapshot for each changed path (`src/server/application/workspace-reconciliation.js:245`, `src/server/infrastructure/persistence/vault-file-store.js:1040-1052`).
2. If no room for that file is open, the next open hydrates from the markdown text and the comment JSON (`collaboration-room.js:358-387`). The text is inserted as new Yjs items, so the stored relative positions reference items that no longer exist. Anchors fall back to creation-time line numbers.

If the room is open during the external change, `applyExternalContent` applies a single prefix/suffix replacement to the `Y.Text` (`collaboration-room.js:756-778`, diff at `:166-195`). Items outside the changed span survive, so anchors outside the span still resolve. Anchors inside the span collapse. Concurrent unsaved local edits are merged with a three-way prefix/suffix check; overlapping changes take the disk version (`reconcileTextContent`, `:197-223`).

### Where comments persist

- `SidecarStore` writes `{ threads, version: 1 }` to `.collabmd/comments/<vault-relative-path>.json` (`src/server/infrastructure/persistence/sidecar-store.js:7, 42-44, 132-153`).
- The room persists comments, the Yjs snapshot and the markdown together, 500 ms after the last change (`collaboration-room.js:586-598`, `:600-655`). `VaultFileStore.persistCollaborationState` stages all three to temp files and renames them with backup and rollback (`vault-file-store.js:568-640`).
- Rename and delete of a markdown file rename or delete its sidecars (`sidecar-store.js:202-210`, `vault-file-store.js:1023-1038`).

### Whether comments land in git

No. `ensureCollabMetadataGitExclude` appends `.collabmd/` to the repository's `info/exclude` (`src/server/infrastructure/git/local-exclude.js:8-37`). It runs at vault start-up when the vault is a git repository (`src/server/vault-registry.js:104-108`) and after a remote clone or update (`src/server/startup/git-remote-bootstrap.js:331, 346, 356`). The file watcher and ripgrep search also skip dot-directories (`src/server/infrastructure/persistence/path-utils.js:11-13`, `src/server/domain/ripgrep-search-service.js:127`).

A user can remove the exclude line and commit `.collabmd/comments/` by hand. The JSON format is stable enough for that. The anchors in it are Yjs relative positions, which do not resolve on another machine without the matching `.bin` snapshot.

### Resolution and comment history

Resolve deletes the thread from the `Y.Array` (`comment-thread-store.js:256-271`, transaction origin `comment-thread-resolve`). `serializeCommentThread` also drops any thread with `resolvedAt` set (`comment-threads.js:354-357`). The sidecar holds only open threads. There is no record of resolved threads and no comment history.

Chat is a separate `Y.Array` named `chat-messages` in the lobby room, capped in length (`src/client/infrastructure/lobby-presence.js:67, 165-167`).

## 4. Suggestion mode

It does not exist. A search for suggestion, track changes and redline across `src/`, `docs/`, `README.md` and `CONTEXT.md` returns only autocomplete and filter suggestion lists (`src/client/presentation/bases-preview-controller.js`, `canvas-picker.js`, `src/client/domain/wiki-link-completions.js`). Every edit goes straight into the shared `Y.Text` and to disk. There is no proposed-change state, accept or reject action, or per-author change marking.

## 5. History and versioning

- Versions are git commits of the markdown files. CollabMD keeps no version store of its own. The Yjs snapshot holds only the current CRDT state; it is deleted on external change and is not a history.
- Git access is the `git` CLI via `execFile` (`git-service.js:1, 16, 58-80`).
- Commit is manual. The browser calls `POST /api/git/stage`, `stage-all`, `unstage`, `commit`, `push`, `pull` and `reset-file` (`src/server/infrastructure/http/create-git-api-command-handler.js:44-80`). `commitStaged` runs `git commit -m` (`git-service.js:182-208`). There is no auto-commit.
- With OIDC auth, the signed-in Google name and email become `GIT_AUTHOR_*` and `GIT_COMMITTER_*` (`git-service.js:18-30`, README line 102).
- Pull is `git fetch` then `git merge --no-edit --autostash <upstream>` (`git-service.js:228-295`). Dirty files that overlap upstream changes are copied to `.collabmd/pull-backups/` first (`backupAndClearOverlappingEntries`, `:457`; `src/server/infrastructure/persistence/pull-backup-store.js:7`).
- Read side: `GET /api/git/history`, `file-history` (`git log --follow`), `commit`, `diff`, `file-snapshot` (`git show <hash>:<path>`) (`create-git-api-query-handler.js:41-121`, `src/server/infrastructure/git/history-service.js:175, 246-251, 468`).
- File watching: recursive `fs.watch` on the vault, 180 ms debounce, incremental state diff with fallback to a full scan (`file-system-sync-service.js:49-243`). Changes go through `WorkspaceReconciliation.reconcileVaultChangeObservation` (`workspace-reconciliation.js:227-260`), which renames or deletes sidecars, deletes Yjs snapshots, and calls `RoomRegistry.reconcileWorkspaceChange`. That calls `room.reloadFromDisk()` for each open room (`src/server/domain/collaboration/room-registry.js:121-141`). Clients receive a highlight range for the changed span.

## 6. Architecture

- Single Node process. It serves the Vite-built client, an HTTP API (`/api/*`), a WebSocket gateway (`/ws/*`) and an MCP endpoint for AI agents (`src/server/create-app-server.js`, `src/server/infrastructure/mcp/create-agent-mcp-handler.js`).
- One Yjs room per open file, held in memory in `RoomRegistry`; a workspace room carries the file tree; a lobby room carries presence and chat.
- The filesystem is the source of truth. The server writes plain markdown back to disk 500 ms after edits.
- Deployment: `npx`, Homebrew, Docker or docker-compose. Optional Cloudflare Quick Tunnel for sharing (`scripts/cloudflare-tunnel.mjs`). A hosted workspace mode clones a GitHub repository into the vault and keeps team metadata in SQLite (`docs/dev/adr/0001-0003`).
- Single instance only. Room state is in-process and not shared across replicas (README line 110).
- Auth strategies: `none`, `password` (shared host password plus signed cookie), `oidc` (Google only) (`src/server/auth/auth-constants.js:3-11`). Without OIDC, user identity is a self-chosen display name and colour. There are no per-document permissions beyond hosted-mode team roles.
- Multi-user: concurrent editing via Yjs, awareness cursors, follow mode, room chat.

## 7. Maturity

| Measure | Value |
|---|---|
| Licence | MIT (`LICENSE`, `package.json:67`) |
| Created | 2026-03-04 |
| Commits | 668; 363 in March 2026, 34 April, 32 May, 2 June, 16 July, 122 August, 99 September |
| Contributors | One main author (667 of 668 commits under two emails); one outside commit |
| Version | 0.1.54; 55 tags |
| Stars / forks / open issues | 277 / 20 / 12 (GitHub API, 2026-09-26) |
| Source size | about 71,500 lines of JS in `src/`, 12,200 lines of CSS |
| Tests | 167 test files, about 51,000 lines: 102 node unit, 13 node integration, 28 browser, 15 Playwright e2e. Comments have dedicated tests (`tests/node/comment-threads.test.js`, `comment-thread-store.test.js`, `comments-feature.test.js`) |
| CI | Publish workflows only (npm, Docker, Homebrew, landing page). No test workflow in `.github/workflows/` |

## 8. Fit against Jim's requirements

| Requirement | Fit | Gap |
|---|---|---|
| Collaborative markdown editor with a Notion/Obsidian-like UI | Partial. Real-time Yjs editing on a folder of plain markdown, Obsidian-style vault features | CodeMirror source pane plus preview. No WYSIWYG or block editing in the Notion style |
| Comment mode and suggestion mode anchored to a word or line | Comments: yes, on a line or an arbitrary character range. Suggestions: no | No suggestion mode. Anchors are Yjs relative positions that break when the Yjs snapshot is discarded; the fallback is creation-time line numbers, with no quote-based re-anchoring |
| Version and comment history stored alongside the markdown in git | Markdown history: yes, via manual commits through the git CLI | Comments are in `.collabmd/comments/*.json`, excluded from git through `.git/info/exclude`. Resolved threads are deleted. No comment history. No auto-commit |

Changes needed to close the gaps:

1. Store comments in a git-tracked location (remove the exclude entry, or move the sidecar into a tracked directory) and commit them with the markdown.
2. Replace or supplement Yjs relative positions with a portable anchor: quote plus prefix and suffix context (W3C Web Annotation `TextQuoteSelector` style), re-resolved against text after external changes.
3. Keep resolved threads with `resolvedAt` in the sidecar instead of deleting them.
4. Add a suggestion model, for example CriticMarkup in the markdown text or a suggestion record type in the sidecar with accept and reject actions.
5. Add an auto-commit policy (for example on idle or on session end) if history must be captured without manual commits.
6. Replace the source editor with a WYSIWYG editor (Milkdown, TipTap or ProseMirror with a Yjs binding) for the Notion-like requirement. This is the largest change, because the comment anchoring code binds to CodeMirror and `Y.Text`.
