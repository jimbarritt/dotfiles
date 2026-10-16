# Mission: Neovim LSP reliability

| Field | Value |
|---|---|
| ID | M-DOT-NVIM-02 |
| Status | DONE |
| Blocked by | none |

## Objective

- Every LSP server attaches on every Neovim start

## Purpose

LSP clients intermittently never attached, about 40% of starts, for every server.

## Decisions

- Re-fire `FileType` for already-loaded buffers at the end of LSP setup. This is the fix: 8 of 8 clean runs. Jim, 2026-08-02

## Plan

| ID | Task | Status |
|---|---|---|
| T-01 | Fix intermittent LSP attach | DONE |

## Intel

- Task notes, as imported from the plan: `missions/operational/M-DOT-NVIM-02/task-notes.md`
