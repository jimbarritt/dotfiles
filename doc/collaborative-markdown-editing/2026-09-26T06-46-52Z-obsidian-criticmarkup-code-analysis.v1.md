2026-09-26T06-46-52Z-obsidian-criticmarkup-code-analysis.v1.md
project: tsk
# obsidian-criticmarkup (Commentator): code analysis

## Source

- Repository: https://github.com/Fevol/obsidian-criticmarkup (URL confirmed by clone).
- Commit analysed: `ffe0df4` (2026-08-07), version 0.2.7.
- Plugin id `commentator`, display name "Commentator" (`manifest.json:2-3`).
- Parser repository: https://github.com/Fevol/criticmarkup-parser (npm package `@fevol/lang-criticmarkup` 2.1.0).
- Git submodules: `src/database` (https://github.com/Fevol/obsidian-database-library) and `src/ui/components` (https://github.com/Fevol/obsidian-svelte-component-library) (`.gitmodules`).
- Local clone: `/tmp/claude-0/-home-user-dotfiles/03bb94cf-f466-5710-82dd-fe31304de24a/scratchpad/repos/obsidian-criticmarkup`.

All paths below are relative to the repository root unless stated.

## 1. Tech stack

| Area | Finding | Evidence |
|---|---|---|
| Language | TypeScript, with Svelte 5 for the side view and settings UI | `tsconfig.json`, `package.json` devDependencies `svelte ^5.55.2`, `src/ui/pages/**/*.svelte` |
| Build | esbuild via a Bun script; `tsc -noEmit` type check first | `package.json` scripts `build`, `scripts/build/esbuild.config.ts` |
| Worker bundling | Custom esbuild plugin inlines the indexer Web Worker | `scripts/build/inline-worker-plugin.ts` |
| Package manager | Bun (`bun.lock`, `bunfig.toml`) | README "Developing" step 1 |
| Styles | SCSS via `esbuild-sass-plugin` | `src/assets/*.scss` |
| Lint and format | ESLint 8, dprint | `.eslintrc.cjs`, `dprint.json` |
| CI | GitHub Actions builds on tag push and attests `main.js` provenance | `.github/workflows/releases.yml` |

Obsidian API usage (`src/main.ts`):

- `registerEditorExtension` for all CodeMirror 6 extensions (`main.ts:225`, list built in `loadEditorExtensions`, `main.ts:101-190`).
- `registerMarkdownPostProcessor` for Reading view rendering (`main.ts:241`, `src/editor/renderers/post-process/renderer.ts:34`).
- `registerView` for the vault-wide annotations view (`main.ts:216`).
- `loadData` / `saveData` for settings only (`main.ts:340-344`).
- `vault.on("modify" | "delete" | "rename" | "create")` for the index (`src/database/database.ts:152-174`).
- `vault.read`, `vault.modify`, `vault.cachedRead` for bulk accept/reject (`src/editor/base/edit-logic/alter-suggestion.ts:36-40`).
- Internal fields `editorEditorField`, `editorInfoField` and the `obsidian-typings` package for undocumented API (`src/editor/uix/extensions/editing-modes/suggestion-mode.ts:1-2`).
- `monkey-around` patches `Menu`, `MenuItem`, `MarkdownView` and `app.plugins` prototypes (`src/patches.ts:9,35,71,140`).

## 2. Key libraries

| Library | Use | Evidence |
|---|---|---|
| `@fevol/lang-criticmarkup` | Lezer LR grammar for CriticMarkup plus metadata. Not regex. | `criticmarkup-parser/src/criticmarkup.grammar` (61 lines) |
| `@codemirror/state`, `@codemirror/view` | StateFields, transaction filters, decorations, gutters, widgets | 37 and 17 import sites in `src/` |
| `@codemirror/language` (fork `github:fevol-forks/cm-language`) | `DocInput` for incremental re-parse | `src/editor/base/edit-util/range-state.ts:1,60` |
| `@lezer/common` | Syntax tree cursor | `src/editor/base/edit-util/range-parser.ts:1` |
| `@codemirror/search` | Text search in the annotations view filter | `src/ui/pages/annotations-view/filter-ranges.ts:5` |
| `@flatten-js/interval-tree` | Range lookup by position | `src/editor/base/ranges/grouped_range.ts:2,11` |
| `diff-match-patch` | Diff clipboard text against selection into CriticMarkup | `src/editor/base/edit-logic/text-diff.ts:1-65` |
| `localforage` (+ getItems/setItems) | IndexedDB persistence of the vault index | `src/database/database.ts:1-5,124-127` |
| `monkey-around` | Prototype patches | `src/patches.ts:2` |
| `svelty-picker` | Date range picker in filter modal | `src/ui/modals/DateRangeModal.svelte` |

`package.json` pins `@lezer/common 1.2.3`, `@codemirror/state 6.5.0`, `@codemirror/view 6.38.6` through `overrides`. These pins match the CodeMirror versions bundled by Obsidian.

Grammar structure (`criticmarkup-parser/src/criticmarkup.grammar`):

- Five node types: `Addition`, `Deletion`, `Substitution`, `Comment`, `Highlight`.
- Each node has an optional `MDSep*` token `@@` that splits a metadata prefix from the content.
- It also accepts HTML-escaped brackets (`{&gt;&gt;`, `{<mark>`, `</del>}`). These appear after Markdown rendering.
- CodeMirror parsing is incremental: `rangeParser` StateField re-parses with tree fragments on each transaction (`range-state.ts:22-60`).
- The range list is rebuilt from the syntax tree in `cursorGenerateRanges` (`range-parser.ts:27-59`).

## 3. Comment design

### Syntax

Standard CriticMarkup: `{>>comment<<}`. Highlight: `{==text==}`.

Extended syntax with metadata, a JSON object followed by `@@` directly after the opening bracket:

```
{=={"author":"jim","time":1758868012}@@the anchored words==}{>>{"author":"jim","time":1758868012}@@Why this word?<<}{>>{"author":"ana","time":1758868100}@@Agreed.<<}
```

- The metadata text is `JSON.parse`d (`src/editor/base/ranges/base_range.ts:37-58`).
- Short keys map to long keys: `a` author, `t` time, `d` done, `s` style, `c` color (`base_range.ts:6-12`).
- `MetadataFields` has `author`, `time`, `done`, `style`, `color` and any extra key (`base_range.ts:14-21`).
- Writers emit `JSON.stringify(fields) + "@@"` (`src/editor/base/edit-util/range-create.ts:36-39`; `base_range.ts:143-158`).
- `time` is Unix seconds from `Math.floor(Date.now() / 1000)` (`src/editor/base/edit-util/metadata.ts:31`).
- `author` is a free-text string from plugin settings (`metadata.ts:29`, `src/types.ts:268`). There is no identity check.
- Metadata is off by default: `enable_metadata`, `add_metadata`, `add_author_metadata`, `add_timestamp_metadata` all default to `false` and `author` defaults to `""` (`src/constants.ts:75-89`).
- Invalid JSON before `@@` gives empty fields. No error is shown (`base_range.ts:56-58`, TODO comment). A comment at `base_range.ts:45` also flags injection as an open concern.

### Anchoring

- A comment has no explicit anchor id. The anchor is positional adjacency.
- `cursorGenerateRanges` attaches a comment to the preceding range when `previous.to === comment.from` (`range-parser.ts:46-51`; `right_adjacent` at `base_range.ts:178-180`).
- The preceding range is any CriticMarkup range: a highlight (word or phrase anchor) or a suggestion (comment on an edit).
- A comment with no adjacent range stands alone at a character position. It has no span.
- The "Add comment" command inserts an empty `{>><<}` at the cursor. When the cursor is inside an existing range, it inserts after the end of that range's thread instead (`src/editor/uix/commands.ts:124-134`; `src/editor/base/edit-logic/add-comment.ts:11-24`).
- No command wraps a selection in a highlight and attaches a comment in one step. The README lists "Add comments to selection" as not done (`README.md:94`).
- Line-level anchoring does not exist. A user highlights the line text instead.

### Threading and replies

- Replies are further `{>>...<<}` blocks placed directly after the first comment with no gap.
- `CommentRange.add_reply` attaches each reply to the thread's base range (`src/editor/base/ranges/types/comment_range.ts:37-45`).
- The thread is flat: base range, then replies in document order (`comment_range.ts:12-14`; `base_range.ts:66-71`). There is no nested reply tree.
- A reply is created with `addCommentToView(view, range)`, which inserts at `range.full_range_back` (`add-comment.ts:13`). UI entry points: comment widget (`src/editor/renderers/live-preview/comment-widget.ts:320`), annotation gutter (`src/editor/renderers/gutters/annotations-gutter/marker.ts:216,244,270`), annotations view context menu (`src/ui/pages/annotations-view/context-menu.ts:78`).
- Resolved state uses the `done` metadata key. There is no separate resolved record.
- Any whitespace or character between two comments breaks the thread.

### Persistence

| Data | Location | Evidence |
|---|---|---|
| Comments, suggestions, metadata | Inline in the `.md` file | `range-create.ts:39-42` |
| Settings (author name, toggles) | Plugin `data.json` via `saveData` | `main.ts:340-344` |
| Vault-wide index | IndexedDB, named `commentator/cache/<appId>` | `main.ts:72-89`, `database.ts:124-127` |
| Bulk-operation undo buffer | In memory only, lost on reload | `main.ts:90-93`, `src/editor/uix/workspace.ts:11-34` |

The file is the single source of truth. The index is a derived cache.

## 4. Suggestion mode

### Edit capture

- Three edit modes: `OFF`, `CORRECTED`, `SUGGEST` (`src/types.ts:13-17`). Default is `CORRECTED` (`constants.ts:16`).
- `SUGGEST` installs `EditorState.transactionFilter` (`suggestion-mode.ts:90-91`).
- The filter runs on transactions with user event `input`, `paste` or `delete`. Other transactions pass through unchanged (`suggestion-mode.ts:117-127`). Image paste and drag-and-drop carry no user event and so escape suggestion mode (comments at `suggestion-mode.ts:120-125`).
- Each changed range becomes `ADDITION` (insert only), `DELETION` (delete only) or `SUBSTITUTION` (both) (`suggestion-mode.ts:141-143`).
- Deleting text inside the user's own `{++...++}` with compatible metadata deletes it for real (`MarkAction.REGULAR`, `suggestion-mode.ts:155-170`).
- `mark_ranges` merges the new edit with adjacent or enclosing ranges (`src/editor/base/edit-logic/mark.ts`, 508 lines). Merge rules: deletion then addition becomes substitution (`mark.ts:57-67`).
- Different `author` values force a split into separate ranges (`metadata.ts:39-41`, `MetadataMergeAction.SPLIT`).
- The filter returns a replacement transaction with `filter: false` and a recomputed cursor (`suggestion-mode.ts:196-202`).
- `CORRECTED` mode (`src/editor/uix/extensions/editing-modes/edit-mode.ts`) keeps edits plain but repairs cursor movement and bracket integrity.
- A second path: "Generate text diff from clipboard" diffs the selection against clipboard text with diff-match-patch and writes CriticMarkup (`commands.ts:178-201`, `text-diff.ts:50-65`).

### Accept and reject

- `AdditionRange.accept()` returns unwrapped text. `reject()` returns empty text (`src/editor/base/ranges/types/addition_range.ts:10-15`). Deletion and substitution mirror this.
- `acceptSuggestions` / `rejectSuggestions` build CodeMirror change specs for the whole document or an interval. By default they also delete attached comments (`alter-suggestion.ts:9-27`).
- Entry points: command palette (`commands.ts:64,76,87,106`), editor context menu on selection (`src/editor/uix/context-menu.ts:33,49`), gutter markers per line, annotations view.
- Vault-wide operations call `applyToFile`, which reads, rewrites and saves each file (`alter-suggestion.ts:29-41`). The previous file contents go into an in-memory stack for one-level undo (`workspace.ts:11-60`).
- Preview modes render the document as if all changes were accepted or rejected, without writing (`src/types.ts:4-10`).
- "Save clean copy" writes a new file with all markup unwrapped (`commands.ts:203-220`).

### Vault-wide index

- `Database<CriticMarkupRange[]>` keyed by file path, value `{data, mtime}` (`database.ts:11,14`).
- On load it compares stored `mtime` with `file.stat.mtime` and re-indexes stale files (`database.ts:202-203,245`).
- Re-indexing runs in Web Workers, default 2 (`constants.ts:69`; `database.ts:217-224`; `src/index-worker.ts:6-10`). The worker parses file text with the same Lezer parser (`range-parser.ts:62-65`).
- On `modify`, it reuses the open editor's parsed ranges when available (`main.ts:79-83`).
- Storage: IndexedDB through localforage (`database.ts:124-127`). Schema version `DATABASE_VERSION = 4` (`constants.ts:11`). A version change drops and rebuilds the cache (`main.ts:256,276`).
- The index is per device and per vault. It is not written to the vault and is not in git.
- The annotations view (`src/ui/pages/annotations-view/AnnotationsView.svelte`) reads this index and filters by type, text, author and date (`filter-ranges.ts:25-35,101-105`).

## 5. History

- No git integration. A search for `git`, `commit` or `diff` history APIs in `src/` finds nothing relevant.
- The only history mechanism is `plugin.file_history`: an in-memory stack of file snapshots taken before vault-wide accept/reject (`main.ts:90-93`, `workspace.ts:8-60`). It exists for one "Undo" action in the annotations view (`AnnotationsView.svelte:488`). It does not persist.
- Accepting or rejecting a suggestion removes the markup, the comment thread and its metadata from the file. After that, the record exists only in earlier git commits (if the user commits).
- `time` metadata records when a range was created or updated. It is not a version log.

## 6. Collaboration

- No real-time multi-user support. The code has no CRDT, OT, websocket or network code.
- Multi-user support is limited to the `author` string on each range and author-based range splitting.
- Sharing happens through whatever syncs the vault: Obsidian Sync, git, or a file share. Concurrent edits to one file produce normal text merge conflicts. Inline markup and adjacency-based threading make such conflicts touch the prose lines themselves.
- The `author` value is set per device in settings. Any user can write any name.

## 7. Maturity

| Measure | Value | Evidence |
|---|---|---|
| Licence | MIT, "kometenstaub and Fevol" | `LICENSE` |
| Age | First commit 2022-04-18 | `git log --reverse` |
| Commits | 453 total. 432 by Fevol, 19 by kometenstaub, 2 other | `git shortlog -sn` |
| Activity | Bursts: 2023-03, 2023-08, 2024-03, 2025-05, 2025-07. Since then: 6 commits 2025-12, 15 in 2026-04, 4 in 2026-08 | `git log` by month |
| Latest release | Tag `0.2.7` (2026-08-06) | `git tag`, `package.json:3` |
| Size | About 11,480 lines of TS and Svelte in `src/` including submodules | `wc -l` |
| Tests | One Jest file, `tests/cursor_movement.test.ts` (204 lines), cursor movement only. Parser repo has Lezer grammar tests (`criticmarkup-parser/test/*.txt`). No tests for suggestion mode, merging or accept/reject | `jest.config.js`, `tests/` |
| Obsidian version | `minAppVersion` 1.7.5 (`manifest.json:5`); beta manifest 1.5.0 | `manifest.json`, `manifest-beta.json`, `versions.json` |
| Mobile | `isDesktopOnly: false` | `manifest.json:9` |
| Status | README says beta, not for a main vault, "non-zero risk of text being removed" in suggestion mode | `README.md:12-13` |
| Distribution | Not listed in `obsidianmd/obsidian-releases/community-plugins.json` under id `commentator` (checked 2026-09-26). Install is from GitHub releases or BRAT | community-plugins.json |

Code quality notes: many `TODO`, `FIXME` and "bodge" comments (for example `main.ts:98,114`, `add-comment.ts:28`). Heavy use of undocumented Obsidian internals and prototype patches. These raise upgrade risk on new Obsidian releases.

The community plugin list holds three newer CriticMarkup plugins: "Review Critic" (rohrbachd/obsidian-review-critics), "Track Changes" (philphilphil/obsidian-track-changes) and "Review Comments" (shotashirai1719/obsidian-review-comments, "Notion-style review comments stored as CriticMarkup"). This analysis does not cover them.

## 8. Fit against Jim's requirements

| Requirement | Fit | Detail |
|---|---|---|
| Notion/Obsidian-like UI | Good inside Obsidian | Live Preview hides markup and styles ranges (`src/editor/renderers/live-preview/markup-renderer.ts:10-11,132-139`). Right-side annotation gutter shows comment threads. Diff gutter marks changed lines. Side view lists all annotations across the vault. Runs only as an Obsidian plugin. |
| Comment and suggestion mode anchored to a word or line | Partial | Suggestion mode works while typing. Comments anchor by adjacency to a preceding highlight or suggestion. No one-step "comment on selection". No line anchor. No comment-only mode toggle (`README.md:66`). Threads are flat and break on any intervening character. |
| Version and comment history in git alongside the markdown | Partial | Everything lives inline in the `.md`, so git tracks it with no extra files. No git integration. Accepting or resolving deletes the record from the working file, so history exists only in git commits. Metadata is off by default, so author and time need explicit settings. |

### Gaps

1. No real-time collaboration. Git or a sync service is the only sharing layer.
2. Author identity is a free-text setting, not tied to git or any account.
3. Comment anchoring depends on exact byte adjacency. Plain-text edits outside Obsidian break threads without warning.
4. No persistent history of resolved comments or accepted suggestions inside the tool.
5. The vault index is local IndexedDB. Other clones rebuild it and it is not reviewable in git.
6. Beta status with a stated data-loss risk. Test coverage is thin.
7. Heavy dependence on Obsidian internals. Reuse outside Obsidian needs a rewrite of the UI layer. The Lezer grammar, range classes and `mark.ts` merge logic are the most portable parts (all CodeMirror 6 based).

### The raw file outside Obsidian

- GitHub renders `.md` with no CriticMarkup support. All markup shows as literal text: `{++`, `++}`, `{>>`, `<<}`, `~>`, and the JSON metadata such as `{"author":"jim","time":1758868012}@@`.
- Markdown inside a range still renders, so emphasis or links in a suggestion display normally but sit between literal brackets.
- A suggestion that spans block boundaries (for example a deleted paragraph) leaves `{--` and `--}` in separate rendered blocks.
- With metadata on, each range grows by about 40 characters of JSON. Prose diffs in git and on GitHub show markup and metadata changes mixed with text changes on the same lines.
- Other tools that implement CriticMarkup (MultiMarkdown, Pandoc filters) parse the brackets but not the `@@` metadata extension. They show the JSON as part of the comment or suggestion text.
