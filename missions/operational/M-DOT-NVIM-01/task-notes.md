# M-DOT-NVIM-01: task notes

Imported from `doc/planning/plan.md` in the dotfiles repo, heading "Delta: Go Development Environment". The bullets are as written in the plan.

### T-01: Go toolchain and nvim LSP
- ✓ DONE — `go = "latest"` added to `config/mise/config.toml` (installed go 1.26.5)
- ✓ DONE — `$GOPATH/bin` added to PATH in `home/zshrc` (mise provides the toolchain; `go install` binaries land in GOBIN and were otherwise unreachable)
- ✓ DONE — `gopls` added to `config/nvim/lua/plugins/mason.lua` `ensure_installed` (installed v0.23.0)
- ✓ DONE — `gopls` server entry in `config/nvim/lua/plugins/lsp.lua` — gofumpt, staticcheck, inlay hints, `unusedparams`/`unusedwrite`/`nilness`/`shadow` analyses
- ✓ DONE — `go`/`gomod`/`gosum`/`gowork` treesitter parsers
- ✓ DONE — Go-only `BufWritePre` autocmd: `source.organizeImports` code action then format. Deliberate exception to the repo's manual-`<leader>f` convention because unused/missing imports are compile errors in Go
- ✓ DONE — Verified end-to-end headlessly: gopls attaches, unused import removed, missing import added, indentation reformatted

### T-02: Delve/DAP debugging
- TODO — Install `delve` (`go install github.com/go-delve/delve/cmd/dlv@latest`, or via mise if a backend exists)
- TODO — Add `nvim-dap` — no DAP setup exists in this config at all, so this is new plumbing rather than an entry in an existing table
- TODO — Add `leoluz/nvim-dap-go` for Go-specific adapter config and test-debugging helpers
- TODO — Add `rcarriga/nvim-dap-ui` + decide keymaps (breakpoint toggle, step over/into/out, REPL) and where they live relative to the shared `on_attach` in `lsp.lua`
- TODO — Consider whether DAP should be generalised for other languages (rust, python) or kept Go-only for now
