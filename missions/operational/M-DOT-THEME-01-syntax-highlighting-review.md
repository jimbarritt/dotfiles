# Mission: Syntax highlighting review

| Field | Value |
|---|---|
| ID | M-DOT-THEME-01 |
| Status | IN PROGRESS |
| Blocked by | none |

## Objective

- The `green-tinted` dark theme highlights only definitions, strings, constants and comments
- The `green-tinted-light` theme is reviewed against Tonsky's article and changed where Jim decides

## Purpose

Jim finds the dark theme hard to read: about 20 slots each have their own colour, so nothing stands out. Reference: <https://tonsky.me/blog/syntax-highlighting/> and `doc/syntax-highlighting-tonsky.md` in the dotfiles repo.

## Decisions

- Dark theme first, as T-02. It takes over the dark theme part of T-01. Jim
- `mapping.lua` is shared with the light themes, which must render the same after the dark theme change. Jim
- Dark result: comments yellow, definitions light blue, constants purple, strings green, all else default text, punctuation dim. Jim
- `doc/themes/colorscheme-tweaker.html` is the tool to review and change the palettes: a colour picker per palette slot, Lua, Kotlin, Go and Markdown samples coloured to match `mapping.lua`, and an export of the palette as a Lua file. Jim, 2026-10-12

## Open questions

- `rainbow-delimiters.nvim` keeps brackets bright green in every theme. Change it?
- Light theme: keep keywords and function calls as anchor layers, or drop them to default text?
- Light theme: try background tints on tokens?

## Plan

| ID | Task | Status |
|---|---|---|
| T-01 | Review Tonsky's syntax highlighting article against the themes | TODO |
| T-02 | Apply Tonsky's principles to the dark theme | DONE |

## Intel

- Task notes, as imported from the plan: `missions/operational/M-DOT-THEME-01/task-notes.md`
- Dark theme brief for T-02: `missions/operational/M-DOT-THEME-01/tonsky-dark-theme-brief.md`
- Colorscheme tweaker page, in the dotfiles repo: `doc/themes/colorscheme-tweaker.html` (commit `b99a0cf`)
