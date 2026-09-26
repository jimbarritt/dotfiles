2026-09-26T06-46-37Z-agent-comments-code-analysis.v1.md
project: tsk

# Agent Comments: code analysis

Repository: https://github.com/andrewroxby/agent-comments (confirmed by clone and GitHub search).
Commit analysed: `fe44746` (2026-08-09).
Local clone: `/tmp/claude-0/-home-user-dotfiles/03bb94cf-f466-5710-82dd-fe31304de24a/scratchpad/repos/agent-comments`.

All paths below are relative to the repository root.

## Summary

Agent Comments is an Obsidian plugin in TypeScript. It stores all review state inline in the note as plain CriticMarkup. It adds no IDs, no timestamps and no metadata block. A thread is a run of adjacent `{>>...<<}` comments. The author is an optional `Name:` prefix inside the comment body. Agents integrate through the file itself: the plugin pipes a prompt to a local CLI (`claude -p`, `codex exec`, `opencode run`) and the agent edits the file, or the plugin appends the agent's stdout as a reply. There is no MCP server, no standalone CLI and no real-time collaboration. History is whatever git records. Resolving a thread deletes it from the file.

## 1. Tech stack

- Language: TypeScript. Entry point `main.ts` (602 lines), modules under `src/`.
- Build: esbuild bundles `main.ts` to `main.js` in CommonJS, target es2022 (`esbuild.config.mjs:7-31`). Obsidian, Electron, all `@codemirror/*` packages and Node builtins are external (`esbuild.config.mjs:12-24`).
- Type check: `tsc -noEmit -skipLibCheck` (`package.json` `scripts.build`, `scripts.typecheck`).
- Package manager: npm (`package-lock.json` is tracked).
- Tests: vitest (`package.json` `scripts.test`).
- Obsidian API usage (`main.ts`):
  - `Plugin` subclass, `onload` at `main.ts:62`.
  - `registerView` for the `Open | Terminal` sidebar (`main.ts:73`).
  - `registerEditorExtension` for CodeMirror 6 decorations in Live Preview (`main.ts:75-81`).
  - `registerMarkdownPostProcessor` for reading mode (`main.ts:83-87`).
  - Seven `addCommand` entries (`main.ts:97-165`): open review, open terminal, add comment to selection, send selection/note/filepath/all items to terminal, finalize.
  - `loadData`/`saveData` hold settings only (`main.ts:174-183`).
  - File writes: CodeMirror `dispatch` on the open editor, else `app.vault.process` (`src/source-sync.ts:51-99`).
- Manifest: `isDesktopOnly: false`, `minAppVersion: 1.5.0` (`manifest.json`). Node-backed modules load lazily so the review UI runs on mobile (`main.ts:48-53`).

## 2. Key libraries

Runtime dependencies (`package.json`):

- `@xterm/xterm` 6, `@xterm/addon-fit`, `@xterm/addon-webgl`: the embedded terminal.

Dev dependencies (`package.json`):

- `@codemirror/state` 6.5.0 and `@codemirror/view` 6.38.6. Obsidian supplies these at runtime.
- `obsidian` 1.8.7 type definitions, `esbuild`, `typescript`, `vitest`, `builtin-modules`.

CodeMirror 6 usage:

- `src/editor/decorations.ts` builds decorations in a `StateField`, not a `ViewPlugin`, because replace decorations that span line breaks throw in the ViewPlugin form (`src/editor/decorations.ts:1-4`).
- `ThreadChipWidget extends WidgetType` renders a comment thread as an inline chip (`src/editor/decorations.ts:19`).
- It reads Obsidian's `editorLivePreviewField` to switch behaviour between source and Live Preview (`src/editor/decorations.ts:8`).

Parser:

- No third-party parser. `src/parser.ts` uses five regular expressions, one per CriticMarkup form (`src/parser.ts:99-103`).
- It detects fenced, indented and inline code regions and ignores markup inside them (`src/parser.ts:111-193`).
- Overlapping or nested matches are dropped. Nested comments or highlights inside a suggestion are reported as `malformed` (`src/parser.ts:259-278`, `323-344`).

Terminal:

- `src/terminal.ts` embeds a Python script that uses `pty.fork()` as a PTY bridge (`src/terminal.ts:9`, `39`). Desktop only, macOS first (`README.md:95-97`).

Attribution: the parser, operations, decorations, reading-mode rendering and finalize flow derive from the MIT-licensed Track Changes plugin by `philphilphil` (`NOTICE.md`, `TRACK_CHANGES_LICENSE`, `README.md:131-133`).

## 3. Comment design

### Syntax

Standard CriticMarkup, five forms (`src/parser.ts:1-8`, `docs/AGENT_PROTOCOL.md:5-13`):

```text
{>>comment<<}
{++addition++}
{--deletion--}
{~~old~>new~~}
{==highlight==}
```

The plugin adds no new delimiters. It adds conventions on top of the standard syntax.

### Author

- An author is a prefix inside the comment body: `Name:` or `@Name:`. Regex: `/^\s*@?([A-Za-z][\w.-]{0,29})\s*:\s*/` (`src/authors.ts:18`).
- An unprefixed comment belongs to the local user, shown as "You" (`src/authors.ts:3-5`).
- A prefix equal to the configured self-handle (`userDisplayName`, default `user`) maps to the local user (`src/authors.ts:20-40`, `src/settings/types.ts:88`).
- Colours: a fixed hue table for known agent names, a hash for other names (`src/authors.ts:48-73`).

Consequence: any comment that starts with a word and a colon parses as authored by that word. `{>>Note: check this<<}` gets author "Note".

### Threads

- Inline thread: consecutive `{>>...<<}` blocks separated only by spaces or tabs form one thread. The first is the root; the rest are replies (`src/parser.ts:10-12`, `280-318`).
- Own-line thread (added in `fe44746`, 2026-08-09): a line that contains only comment nodes is a thread line. It attaches to the non-blank line directly above. The Nth thread line pairs with the Nth "bare" annotation on that line (an annotation without its own inline thread). Extra thread lines comment on the whole prose line. A thread line under a blank line, or at the top of the file, is a document-level comment (`src/parser.ts:350-503`).
- Annotations key by their end line, so multi-line anchors pair with the thread line below their closing marker (`src/parser.ts:435-445`).
- The commit message for `fe44746` states the own-line format mirrors a Neovim plugin, "scholia.nvim" (also referenced as "critic.nvim" in `src/authors.ts:7`). Neither repository is public under the `andrewroxby` account.

Example of what the plugin writes for a new comment on a selection (`src/review-model.ts:97-140`):

```text
The build uses {==esbuild==} for bundling.
{>>Why not Vite?<<}{>>Claude: Obsidian plugins ship one CJS file; esbuild is the template default.<<}
```

In table rows, or when an earlier bare anchor on the same line has no thread yet, it falls back to the inline form `{==sel==}{>>comment<<}` (`src/review-model.ts:113-133`).

### IDs and metadata

- No comment IDs in the file. Review item IDs are computed at parse time from node kind and character offset: `reviewItemId(node.kind, node.from)` (`src/review-model.ts:371`). They change whenever text before them changes.
- No timestamps. No resolved flag. No reply-to pointer. Thread membership is positional adjacency only.
- Stale-offset safety: every `SourceEdit` carries an `expected` substring and optional `before` context; `rebaseEdit` re-locates or refuses an edit if the document moved (`src/operations.ts:1-126`).

### Anchoring

- Anchor to a word or range: wrap it in `{==...==}` and attach a thread (`docs/WORKFLOW.md:7-15`).
- Anchor to a suggestion: place the thread directly after `{++..++}`, `{--..--}` or `{~~..~>..~~}`. The UI links them as one proposal card (`docs/AGENT_PROTOCOL.md:35-57`).
- Anchor to a line: a surplus own-line thread under a prose line (`src/parser.ts:360-365`).
- Anchor to the document: a thread line under a blank line or at file start.
- Nesting is forbidden. A comment on text that also has a suggestion must stay flat: `{~~X~>Y~~}{>>...<<}` (`docs/AGENT_PROTOCOL.md:37-49`).

### Persistence

- Inline in the Markdown file only. No sidecar file and no review database (`README.md:40`, `docs/ARCHITECTURE.md:7`).
- Plugin data (`data.json` via `saveData`) holds settings only: agents, display name, zoom, finalize defaults (`src/settings/types.ts:42-52`).

### Resolve and finalize

- Resolve deletes the thread markup and unwraps the highlight, keeping the anchored text (`src/review-model.ts:258-276`, `docs/WORKFLOW.md:182-184`).
- Finalize strips every comment and accepts or rejects remaining suggestions per settings. Defaults: accept additions, reject deletions, reject substitutions, strip highlights (`src/operations.ts:430-448`).

## 4. Suggestion mode

- Suggestions exist as CriticMarkup additions, deletions and substitutions.
- The UI can accept, reject or edit a suggestion, from the sidebar card or from an inline popover in Live Preview (`src/ui/overlays.ts:143-200`, `src/operations.ts:128-198`).
- "Edit" rewrites the proposed text inside the markup; it does not apply it (`src/operations.ts:170-186`, `src/edit-suggestion-modal.ts`).
- There is no track-changes typing mode. No CodeMirror `transactionFilter` or `changeFilter` intercepts user typing (search of `main.ts` and `src/` returns none). The selection popover offers only "Add comment" and "Send to Terminal" (`src/ui/overlays.ts:113-123`).
- Suggestions are created by agents, or by the user typing CriticMarkup by hand.

## 5. Agent integration

Two paths, both local commands. No MCP server. No standalone CLI.

1. Background reply (one-shot):
   - `runReplyAgent` spawns the configured command in the vault root, writes the prompt to stdin and reads stdout, with a timeout (`src/agent/process.ts:109-166`).
   - Presets: `codex exec`, `claude -p --permission-mode acceptEdits`, `opencode run` (`src/settings/types.ts:54-76`).
   - The prompt contains the CriticMarkup protocol, the target thread, a review index and the full note (`src/agent/prompts.ts:14-73`).
   - The agent either edits the file directly, or prints a reply. The plugin wraps stdout as `{>>AgentName: ...<<}` and appends it to the thread (`src/review-model.ts:302-312`, `main.ts:444-471`). Cleanup strips ANSI codes, one enclosing code fence, and neutralises `<<}` (`src/review-model.ts:326-342`).
   - The spawn sets `AGENT_COMMENTS_REPLY=1` for hook suppression (`src/agent/process.ts:119`).
2. Embedded terminal:
   - xterm.js plus a Python PTY bridge in the sidebar, multiple tabs (`src/terminal.ts`, `src/panel/view.ts:425`).
   - Commands paste a prompt into the running agent: current note, selection, selected items, all open items, or the filepath only (`src/agent/prompts.ts:75-160`, `main.ts:116-155`).

Conventions doc for agents that run outside the plugin:

- `docs/AGENT_PROTOCOL.md` states the rules.
- `docs/APPEND_TO_AGENTS.md` is a block to paste into a vault `AGENTS.md` or `CLAUDE.md`. Claude Code running in a plain terminal follows these rules and edits the file with its normal file tools.
- The own-line thread rules exist in the prompt constant (`src/agent/prompts.ts:34-38`) but not yet in `docs/AGENT_PROTOCOL.md` or `docs/APPEND_TO_AGENTS.md`. The docs describe only the inline form.

Concurrency with agents: if the file changed on disk and in the editor during a run, the plugin shows a notice and does not overwrite (`src/source-sync.ts:102-120`). `docs/WORKFLOW.md:180` states that two terminal agents editing one note at once can race.

## 6. History

- Nothing beyond the file. No history store, no event log, no resolved-thread archive.
- Resolve and finalize delete comment text from the file. Recovery depends on git or Obsidian file recovery.
- No git integration in the code. A search of `main.ts` and `src/` for `git` returns nothing.
- `docs/ROADMAP.md:10` lists "Optional resolved-history store" as a later item.

## 7. Collaboration

- No real-time multi-user support. No CRDT, no OT, no `@codemirror/collab` usage (it appears only in the esbuild external list, `esbuild.config.mjs:17`).
- No user identity beyond a display name string in settings.
- Sharing depends on whatever syncs the vault: git, Obsidian Sync, iCloud. The docs name git and mobile sync (`docs/ENGINEERING_NOTES.md:7`).
- Multiple agents coordinate through the same file sequentially.

## 8. Maturity

- Created 2026-06-10. 17 commits, all by `andrewroxby`. Last commit 2026-08-09. One merged PR (#1).
- 0 stars, 0 forks, 0 open issues (GitHub search, 2026-09-26).
- Version 0.1.0 (`manifest.json`, `package.json`). Not in the Obsidian community store; install manually or through BRAT (`README.md:21-36`).
- Licence: MIT (`LICENSE`). Includes Track Changes MIT licence (`TRACK_CHANGES_LICENSE`).
- `README.md:9-11`: "personal, experimental project". The author states no time to maintain it; issues and PRs may go unanswered.
- Tests: 57 vitest cases. `tests/criticmarkup.test.ts` (552 lines, 52 cases) covers parser, operations, linked proposals and reply cleanup. `tests/settings.test.ts` (5 cases) covers settings migration. UI, terminal and Obsidian sync are manual-tested only (`docs/ENGINEERING_NOTES.md:40-42`).
- No CI configuration (no `.github/` directory).
- Size: about 6,900 lines of TypeScript source plus 620 lines of tests, 1,419 lines of CSS, about 16,000 tracked lines in total including `package-lock.json`. Largest file: `src/panel/view.ts` (1,192 lines).
- Platform: terminal and background agents are macOS first and desktop only (`README.md:91-97`).

## 9. Fit against Jim's requirements

### Requirement 1: collaborative editor with a Notion/Obsidian-like UI

- Partial. The UI is Obsidian itself, with a review sidebar, inline chips and popovers.
- Not collaborative in real time. One writer at a time; sync is external.
- Tied to Obsidian. It is a plugin, not a standalone editor or web app.

### Requirement 2: comment mode and suggestion mode, anchored to a word or line

- Comment mode: yes. Select text, "Add comment", thread with replies. Anchors to a word/range (`{==...==}`), a line (own-line thread) or the document.
- Suggestion mode: partial. Suggestions can be reviewed (accept, reject, edit) and discussed. The user cannot type in a suggesting mode; no keystroke-to-CriticMarkup conversion exists. Agents or hand-written markup create suggestions.
- Anchors are textual, not ID-based. Duplicate anchor text is a known weak point in reading mode (`docs/ROADMAP.md:14`).

### Requirement 3: version and comment history alongside the markdown in git

- Comments are in the file, so git tracks them.
- Resolving deletes the thread. The only record of a resolved discussion is git history.
- No timestamps and no stable IDs, so git blame is the only source of "who and when" beyond the name prefix.
- No git features in the plugin: no commit on resolve, no history view, no diff view.

### Gaps

1. No real-time collaboration and no identity model.
2. No typing-based suggestion mode.
3. No stable comment IDs or timestamps; thread structure depends on adjacency and line position.
4. No history of resolved comments outside git.
5. Obsidian only. No web editor.
6. Nesting is forbidden, so overlapping comments and suggestions need manual flattening.
7. Unmaintained by the author's own statement.

### Reusable parts

- `src/parser.ts`, `src/operations.ts` and `src/review-model.ts` have no Obsidian dependency and are unit-tested. They are portable to a standalone CodeMirror 6 editor.
- `docs/APPEND_TO_AGENTS.md` is a ready-made agent convention for CriticMarkup review.
- The prompt-to-CLI pattern (`src/agent/process.ts`, `src/agent/prompts.ts`) works with Claude Code with no extra protocol.

### Raw file outside Obsidian

The file stays valid Markdown text but carries visible markup. Example of a note under review:

```text
# Release notes

The plugin uses {==esbuild==} for bundling.
{>>Why not Vite?<<}{>>Claude: Obsidian plugins ship one CJS file; esbuild is the template default.<<}

It supports {~~three~>five~~} agent presets.{>>Codex: Count in settings/types.ts is three.<<}{>>user reply<<}

{>>Claude: Add a section on mobile support.<<}
```

- GitHub, VS Code and standard Markdown renderers do not render CriticMarkup. They show the braces and arrows as literal text, or partially interpret `~~` as strikethrough.
- Pandoc and MultiMarkdown process CriticMarkup through extensions or filters.
- Diffs are readable: a new reply is an in-line insertion on one line.
- The file needs "Finalize" before publication, or it ships with markup in it.
