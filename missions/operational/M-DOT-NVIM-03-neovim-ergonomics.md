# Mission: Neovim ergonomics

| Field | Value |
|---|---|
| ID | M-DOT-NVIM-03 |
| Status | IN PROGRESS |
| Blocked by | none |

## Objective

- Markdown and text files wrap
- nvim-tree shows dotfiles and untracked files, and the cheatsheet lists its keys
- Closing the last window does not exit Neovim
- Dockerfile keywords highlight correctly
- The Mason `ENOTCONN` error is explained

## Purpose

Fix day-to-day Neovim friction.

## Decisions

- Filetype settings go in `config/nvim/after/ftplugin/`, which is immune to the `init.lua` ordering race. Jim, 2026-08-18
- nvim-tree shows dotfiles and gitignored file names. Jim, 2026-08-18
- `<C-w>q` is remapped to `<C-w>c`. Jim, 2026-08-18
- The Dockerfile injections query is a full replacement in `after/queries/`, with `injection.combined` dropped. Jim, 2026-08-18

## Open questions

- What causes the Mason `ENOTCONN` error?
- Set `log_level = vim.log.levels.DEBUG` in `mason.setup()` to capture the next occurrence?

## Plan

| ID | Task | Status |
|---|---|---|
| T-01 | Prose wrap for markdown and text | DONE |
| T-02 | File tree filters and cheatsheet | DONE |
| T-03 | Window close key behaviour | DONE |
| T-04 | Dockerfile treesitter injection bleed | DONE |
| T-05 | Mason ENOTCONN error | TODO |

## Intel

- Task notes, as imported from the plan: `missions/operational/M-DOT-NVIM-03/task-notes.md`
