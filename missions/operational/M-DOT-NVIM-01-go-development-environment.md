# Mission: Go development environment

| Field | Value |
|---|---|
| ID | M-DOT-NVIM-01 |
| Status | IN PROGRESS |
| Blocked by | none |

## Objective

- Go toolchain from mise, with gopls in Neovim
- Go code can be debugged in Neovim with Delve through DAP

## Purpose

Develop Go in Neovim.

## Decisions

- Go-only `BufWritePre` autocmd runs organise imports and then format. This is a deliberate exception to the manual `<leader>f` convention, because unused and missing imports are compile errors in Go. Jim, 2026-08-02

## Open questions

- Generalise DAP for other languages such as rust and python, or keep it Go-only?
- Which DAP keymaps, and where do they live relative to the shared `on_attach` in `lsp.lua`?

## Plan

| ID | Task | Status |
|---|---|---|
| T-01 | Go toolchain and nvim LSP | DONE |
| T-02 | Delve/DAP debugging | TODO |

## Intel

- Task notes, as imported from the plan: `missions/operational/M-DOT-NVIM-01/task-notes.md`
