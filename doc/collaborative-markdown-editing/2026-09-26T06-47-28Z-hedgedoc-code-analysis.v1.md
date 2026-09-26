2026-09-26T06-47-28Z-hedgedoc-code-analysis.v1.md
project: tsk
# HedgeDoc code analysis

Repository: https://github.com/hedgedoc/hedgedoc

Clones used (paths relative to `/tmp/claude-0/-home-user-dotfiles/03bb94cf-f466-5710-82dd-fe31304de24a/scratchpad/repos/`):

- `hedgedoc/`: branch `main` (HedgeDoc 2.x rewrite), head `5dd94d5`, 2026-09-22.
- `hedgedoc-1.x/`: branch `master` (HedgeDoc 1.x), head `005787c`, 2026-09-05.

All line references below point into these clones. GitHub links use the same paths on the named branch, for example `https://github.com/hedgedoc/hedgedoc/blob/main/backend/src/revisions/revisions.service.ts#L377`.

## Summary

- HedgeDoc stores notes in a relational database. Each save writes a full-content snapshot plus a unified diff to a `revision` table. There is no git storage, no git export and no git import in either 1.x or 2.x.
- Real-time sync in 2.x uses Yjs (CRDT) over a custom WebSocket message protocol. 1.x uses operational transformation (ot.js) over socket.io.
- HedgeDoc has no comments and no suggestion mode in either version. HackMD (closed source) has both, per its public docs.
- The prior research claim is confirmed for storage: the database is the source of truth. One correction: HedgeDoc has no git push/pull at all. 1.x exports to GitHub Gist, GitLab Snippets and Dropbox. The GitHub push/pull sync is a HackMD feature.
- A git-backed note store needs a new persistence layer. The code has a storage interface for media uploads only. Note and revision persistence is hard-wired to Knex queries.

## 1. Tech stack

### 2.x (`main`)

| Area | Choice | Evidence |
|---|---|---|
| Language | TypeScript throughout | `package.json` devDependencies `typescript 5.9.3` |
| Monorepo layout | pnpm workspaces: `backend`, `frontend`, `commons`, `database`, `html-to-react`, `markdown-it-plugins`, `docs`, `dev-reverse-proxy` | `pnpm-workspace.yaml` lines 5 to 13 |
| Package manager | pnpm 11.24.0 (enforced via `devEngines`) | `package.json` lines 19 to 25 |
| Build orchestration | Turborepo 2.10 | `package.json` scripts, `turbo.json` |
| Lint and format | oxlint, oxfmt | `package.json` devDependencies |
| Backend framework | NestJS 11 on Fastify, WebSockets via `@nestjs/platform-ws` (`ws`) | `backend/package.json` lines 39 to 42, 72; `backend/src/app-init.ts` line 126 |
| Frontend framework | Next.js 14, React 18, Redux Toolkit, React Bootstrap, i18next | `frontend/package.json` lines 53, 96 to 108 |
| Database access | Knex 3.1 query builder with hand-written migrations | `backend/package.json` lines 23, 59; `backend/src/database/migrations/20250312211152_initial.js` |
| Databases | PostgreSQL, MariaDB/MySQL, SQLite | `backend/src/config/database-type.enum.ts` lines 7 to 11; drivers `pg`, `mysql2`, `better-sqlite3` in `backend/package.json` lines 51, 64, 68 |
| Tests | Jest (backend and frontend unit), Cypress (frontend end-to-end) | `backend/package.json` line 93; `frontend/package.json` lines 19 to 24 |

Note: the tagged `v2.0.0-alpha.3` release (2024-09-18) uses TypeORM (`git show v2.0.0-alpha.3:backend/package.json`, line 68). `main` has since replaced TypeORM with Knex. The initial Knex migration is dated 2025-03-12.

### 1.x (`master`)

JavaScript, Node.js, Express 4.22, Sequelize 5.22 ORM, socket.io 4.8, CodeMirror 5 fork, markdown-it 14. Evidence: `hedgedoc-1.x/package.json` lines 3, 40, 43, 58, 99, 100, 142. Supports the same SQL database family through Sequelize.

## 2. Key libraries

| Function | 2.x | 1.x |
|---|---|---|
| Editor component | CodeMirror 6 via `@uiw/react-codemirror` (`frontend/package.json` lines 34 to 42, 55) | CodeMirror 5 fork `@hedgedoc/codemirror-5` (`hedgedoc-1.x/package.json` line 142) |
| Real-time sync model | Yjs 13.6 CRDT, patched (`pnpm-workspace.yaml` line 38; `commons/src/y-doc-sync/realtime-doc.ts` line 7) | Operational transformation, vendored ot.js (`hedgedoc-1.x/lib/ot/`) |
| Editor to CRDT binding | Custom CodeMirror view plugin, not `y-codemirror` (`frontend/src/components/editor-page/editor-pane/codemirror-extensions/document-sync/y-text-sync-view-plugin.ts` lines 13 to 30) | ot.js client (`hedgedoc-1.x/lib/ot/client.js`) |
| Transport | Plain WebSocket with a custom JSON message protocol, not `y-websocket` or `y-protocols` (`commons/src/message-transporters/message.ts` lines 8 to 25) | socket.io |
| Markdown renderer | markdown-it 13 with many plugins, rendered in a sandboxed iframe (`frontend/package.json` lines 81 to 94; `frontend/src/components/editor-page/renderer-pane/renderer-pane.tsx` lines 9 to 29) | markdown-it 14 |
| Diffs for revisions | `diff` (jsdiff) `createPatch` (`backend/src/revisions/revisions.service.ts` line 24) | `diff-match-patch` (HackMD fork) |
| ORM / DB layer | Knex | Sequelize |

The Yjs document holds one `Y.Text` named by `MARKDOWN_CONTENT_CHANNEL_NAME` (`commons/src/y-doc-sync/realtime-doc.ts` line 50). The document has no other shared types, so there is no structured side-channel for annotations.

## 3. Comments

HedgeDoc has no comment feature in 2.x or 1.x.

Evidence:

- The 2.x schema has no comment table. Tables created in `backend/src/database/migrations/20250312211152_initial.js`: user, group, note, alias, api_token, identity, group_user, revision, revision_tag, authorship_info, note_user_permission, note_group_permission, media_upload, media_upload_note, user_pinned_note, visited_note, session (lines 51 to 458).
- `database/src/types/` has no comment type.
- The WebSocket protocol has no comment messages (`commons/src/message-transporters/message.ts` lines 8 to 25).
- A case-insensitive search for `comment` across `backend/src`, `database/src` and `commons/src` matches only an LDAP code comment.
- 1.x models are `author`, `note`, `revision`, `user` only (`hedgedoc-1.x/lib/models/`).

HackMD (closed source, from public docs): users select text in a note and add a comment, or add a comment without a text selection. The Sharing menu controls who may comment. Sources: https://hackmd.io/s/how-to-use-comments and https://hackmd.io/@hackmd-blog/new-commenting-experience. The docs do not describe the anchoring data model or storage.

## 4. Suggestion mode

HedgeDoc has no suggestion mode in 2.x or 1.x. No table, DTO, message type or UI component refers to suggestions. Edits apply directly to the shared `Y.Text` (2.x) or the OT document (1.x).

HackMD has "Suggest edit": the user selects text and submits a proposed change, per https://hackmd.io/@hackmd-blog/new-commenting-experience. The implementation is not public.

## 5. History and versioning

### 2.x revision model

Each revision stores both a full snapshot and a patch. `database/src/types/revision.ts` lines 57 to 84:

- `uuid`, `note_id`
- `patch`: unified diff from the previous revision
- `content`: full note text at this revision
- `yjs_state_vector`: binary Yjs state (nullable)
- `note_type`, `title`, `description`, `created_at`

Table definition: `backend/src/database/migrations/20250312211152_initial.js` lines 213 to 237 (`patch` and `content` are `text`, `yjs_state_vector` is `binary`, cascade delete on note).

Related tables:

- `revision_tag` (`database/src/types/revision-tag.ts`): tags per revision.
- `authorship_info` (`database/src/types/authorship-info.ts` lines 111 to 136): author id with start and end character offsets per revision. Production code reads this table (`backend/src/revisions/revisions.service.ts` lines 103 to 110, 280 to 297) but no production code inserts rows. Only the seed file writes it (`backend/src/database/seeds/03_note.ts` line 155). Per-range authorship is therefore not populated in 2.x yet.

When revisions are written:

- `RevisionsService.createRevision` computes `createPatch(primaryAlias, oldContent, newContent)` and inserts a row. It skips the insert when content is unchanged (`backend/src/revisions/revisions.service.ts` lines 330 to 400, patch at line 377).
- The realtime layer calls `saveRealtimeNote` on a timer and before the in-memory note is destroyed (`backend/src/realtime/realtime-note/realtime-note.service.ts` lines 47 to 62, 84 to 118).
- The timer interval is `HD_NOTE_PERSIST_INTERVAL`, default 10 minutes (`backend/src/config/note.config.ts` lines 73 to 78). Revisions are therefore coarse snapshots, not per-keystroke history.
- A REST content update closes the realtime session and writes a revision (`backend/src/notes/note.service.ts` lines 245 to 249).

Retention: a daily cron job deletes revisions older than `HD_NOTE_REVISION_RETENTION_DAYS` (default 0, which disables deletion) and rewrites the oldest kept revision's patch as a diff from empty (`backend/src/revisions/revisions.service.ts` lines 431 to 580; config lines 66 to 72). `purgeRevisions` deletes all history except the latest (lines 181 to 210).

On load, the backend restores the Yjs document from the latest revision's `yjs_state_vector`, or from `content` when that is null (`realtime-note.service.ts` lines 84 to 90).

### 1.x revision model

`hedgedoc-1.x/lib/models/revision.js` lines 56 to 100: `patch`, `lastContent`, `content`, `length`, `authorship` (JSON text). Patches use diff-match-patch. `saveAllNotesRevision` (line 174) runs on a timer (line 165). The `Note` model also holds the current content and authorship (`hedgedoc-1.x/lib/models/note.js`).

### Git export and import

Neither version has git integration.

- 2.x: a search for `git`, `github`, `gitlab` and `gist` in `backend/src` returns only issue-tracker URLs in error messages (`backend/src/permissions/permissions.guard.ts` lines 64, 89).
- 1.x: exports to GitHub Gist through OAuth (`hedgedoc-1.x/lib/web/note/actions.js` lines 34 to 46; route in `lib/web/note/controller.js` line 116), GitLab Snippets and Dropbox (`hedgedoc-1.x/public/views/hedgedoc/header.ejs` lines 37 to 53). A Gist is a git repository on GitHub's side, but HedgeDoc treats it as a one-shot export target. There is no pull, no sync and no revision history transfer.

The "git push/pull mirror" in the prior research describes HackMD's GitHub sync. It does not describe HedgeDoc.

## 6. Storage layer

The note store cannot be pointed at a filesystem or git repository.

- Notes and revisions live only in SQL via Knex. `Note` holds metadata only; content lives in `revision` (`database/src/types/note.ts` lines 7 to 11).
- There is no repository or storage interface for notes. `RevisionsService` and `NoteService` issue Knex queries directly. `TableRevision` appears 45 times across 4 non-test files (`revisions.service.ts`, `explore.service.ts`, `knex.types.ts`, seeds).
- Media uploads do have a pluggable backend: `MediaBackend` interface (`backend/src/media/media-backend.interface.ts` line 8) with filesystem, S3, Azure, WebDAV and Imgur implementations (`backend/src/media/backends/`). This pattern shows how the project structures storage plugins, but it covers binary uploads only.

Effort for a git-backed note store:

1. Extract a `NoteContentStore` interface from `RevisionsService` covering: create revision, get latest, get by id, list metadata, purge, retention cleanup. The seams are clear because all content access goes through `RevisionsService`.
2. Implement it with a git library (for example `isomorphic-git` or shelling out to `git`). Map one note to one file, one revision to one commit, revision tags to git tags or trailers.
3. Keep SQL for users, permissions, aliases, sessions, and an index of note-to-path. Queries in `explore.service.ts` that join `revision` for title and description need a cache table or a rewrite.
4. Store the Yjs state separately (a sidecar binary or a DB column), or rebuild it from markdown on load. Rebuilding loses nothing needed for correctness, because the backend already falls back to `content` when the state is null.
5. Handle external edits: a commit made outside HedgeDoc while a realtime session is open needs a diff applied into the `Y.Text`. HedgeDoc has no code path for this today.

Estimated scope: medium. Steps 1 to 4 touch a small number of backend files. Step 5 and conflict handling for concurrent external commits are new design work. Commits happen at persist-interval granularity (default 10 minutes), so git history would be coarse unless the interval is shortened.

## 7. Maturity

- Licence: AGPL-3.0 (`LICENSE`; `backend/package.json` line 8; `frontend/package.json` line 5). Server modifications offered over a network must be published.
- Activity: 50 commits on `main` between 2026-08-30 and 2026-09-22 (depth-50 clone). Top authors in that window: renovate bot 20, Philip Molares 18, Erik Michelson 8. A small core team drives development.
- 2.x release state: latest tag `v2.0.0-alpha.3` (2024-09-18). No 2.x beta or stable release exists two years later. `main` has diverged significantly since alpha.3 (TypeORM replaced by Knex). The README states that 2.x does not implement all features yet.
- 1.x: latest release `1.12.0` (2026-08-21). The README states 1.x is maintenance-only and accepts no feature requests.
- Tests: 142 `*.spec.ts` files across the 2.x monorepo; 30 Cypress files in `frontend/cypress`. CI workflows in `.github/workflows/` run build, unit tests with Codecov upload, end-to-end tests, lint, REUSE licence checks, and static analysis (`test-and-build.yml` lines 43, 52, 65, 72).

## 8. Fit against Jim's requirements

| Requirement | HedgeDoc 2.x | Gap |
|---|---|---|
| Collaborative markdown editor with a Notion/Obsidian-like UI | Real-time collaboration works (Yjs). UI is a split CodeMirror source pane plus rendered preview. No block editor, no WYSIWYG, no live-preview inline rendering. | Large. A Notion-like UI needs a different editor (for example ProseMirror/Tiptap or CodeMirror with live-preview decorations). The markdown-it render pipeline and Yjs sync are reusable. |
| Comment mode and suggestion mode anchored to a word or line | Absent. No schema, protocol or UI. | Full build. Anchors need Yjs relative positions (`Y.RelativePosition`) so they survive concurrent edits; the single `Y.Text` document has room for a second shared type (`Y.Map` or `Y.Array`) holding threads. Suggestions need a pending-change model on top of that. |
| Version and comment history stored alongside the markdown in git | Absent. History is SQL rows (snapshot plus patch). No git integration of any kind. | Full build of a storage adapter (section 6), plus a serialisation format for comments in the repository (for example a sidecar JSON or YAML file per note), plus external-edit reconciliation. |

Conclusion: HedgeDoc supplies a tested real-time markdown sync core (Yjs over WebSocket, NestJS backend, markdown-it renderer) under AGPL-3.0. It supplies none of the three distinguishing requirements. Using it means forking a pre-release codebase with a small team and rebuilding the editor UI, adding comments and suggestions, and replacing revision persistence. The reusable parts are the patterns (Yjs server adapter in `commons/src/y-doc-sync/`, persist timer, revision snapshot plus patch) more than the application itself.
