# M-DOT-NVIM-02: task notes

Imported from `doc/planning/plan.md` in the dotfiles repo, heading "Delta: Neovim LSP Reliability". The bullets are as written in the plan.

### T-01: Fix intermittent LSP attach
- ✓ DONE — Diagnosed intermittent "no active clients" affecting every server, not just gopls
  - Timing measurement was the discriminator: attach completed in 0.1s or never within 45s, with no intermediate values, ruling out slow server startup or indexing
  - Cause: LSP clients start off `FileType`; when a file is passed on the command line that event can fire before the plugin config finishes, and nothing retries
  - Baseline failure rate ~40%; a restart appeared to fix it only by winning the race
- ✓ DONE — `vim.lsp.enable(vim.tbl_keys(servers))` added to `config/nvim/lua/plugins/lsp.lua` — the config previously never called `enable()`, relying on mason-lspconfig, which as a lazy.nvim dependency loads first and produced the `"<server> does not have a configuration"` warnings in `:LspLog`. Measured separately: did not fix the flakiness
- ✓ DONE — Re-fire `FileType` for already-loaded buffers at the end of setup — this is the fix; 8/8 clean runs against a ~40% baseline
- ✓ DONE — Written up in `doc/nvim-lsp-filetype-race.md`, indexed in `doc/INDEX.md`, including a general diagnostic table for this class of problem
