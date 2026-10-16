# M-DOT-NVIM-03: task notes

Imported from `doc/planning/plan.md` in the dotfiles repo, heading "Delta: Neovim Ergonomics". The bullets are as written in the plan.

### T-01: Prose wrap for markdown and text
- ✓ DONE — `wrap` never applied to markdown despite a `FileType` autocmd existing
  - Cause: `init.lua` requires `config.autocmds` after `config.lazy`, so for a file named on the command line the event has already fired — the same race as `doc/nvim-lsp-filetype-race.md`
  - `text` was never handled at all
  - Fixed with `config/nvim/after/ftplugin/markdown.lua` and `after/ftplugin/text.lua`; the runtime's ftplugin mechanism is registered before `init.lua` runs, so it is immune to the ordering
  - Dead autocmd removed from `config/nvim/lua/config/autocmds.lua`
  - Verified headlessly: `.md`/`.txt` wrap, `.go` does not, no leak between buffers in one window

### T-02: File tree filters and cheatsheet
- ✓ DONE — nvim-tree `filters` changed to `dotfiles = false` with `^\.git$` and `^\.DS_Store$` added to `filters.custom`; `git_ignored` left on
  - Originally raised as untracked Go files being hidden; testing showed nvim-tree was never hiding them, and the user confirmed the directory had been misread
  - Kept anyway as the stated preference: see gitignored files' names but not their contents, see dotfiles, see untracked and staged files
- ✓ DONE — Three FILE TREE sections added to `config/key-help/nvim`, taken from a dump of the live buffer keymaps so they reflect the `on_attach` overrides

### T-03: Window close key behaviour
- ✓ DONE — `<C-w>q` remapped to `<C-w>c` in `config/nvim/lua/config/keymaps.lua`
  - `<C-w>q` is `:quit` scoped to a window, so on the last window it exits nvim; `:close` raises `E444` instead
  - Cheatsheet WINDOWS section updated, plus `:bd`

### T-04: Dockerfile treesitter injection bleed
- ✓ DONE — Dockerfile instructions rendering as plain text from partway down the file
  - Cause: nvim-treesitter's dockerfile `injections.scm` sets `injection.combined`, merging every `RUN` body into one bash tree parsed as a contiguous span, so bash `@variable.parameter` captures cover the `COPY`/`FROM`/`RUN` keywords in between and outrank `@keyword`
  - Fixed with `config/nvim/after/queries/dockerfile/injections.scm` — a full replacement (no `;; extends`, since appending cannot remove a directive) with `injection.combined` dropped
  - `dockerfile` added to `ensure_installed` in `treesitter.lua`
  - Written up in `doc/nvim-dockerfile-injection-bleed.md`, indexed, and cross-linked from `doc/additional-customisation-of-nvim-treesitter.md`

### T-05: Mason ENOTCONN error
- TODO — `Error executing callback: ENOTCONN` from `mason-core/async/init.lua:121` seen interactively; not reproduced
  - Ruled out: registry is current (downloaded same day), network fine, all 11 `ensure_installed` servers present, `mason.log` has no entry, forced `mason-registry.update()` succeeds
  - Line 121 is `error(err, 0)` in `exports.scope`; `ENOTCONN` is libuv for writing to an unconnected stream, consistent with a spawn race rather than a network failure
  - Offered but not applied: `log_level = vim.log.levels.DEBUG` in `mason.setup()` to capture the next occurrence
