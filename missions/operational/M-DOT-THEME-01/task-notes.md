# M-DOT-THEME-01: task notes

Imported from `doc/planning/plan.md` in the dotfiles repo, heading "Delta: Syntax Highlighting Review". The bullets are as written in the plan.

### T-01: Review Tonsky's syntax highlighting article against the themes
- TODO — Review <https://tonsky.me/blog/syntax-highlighting/> and decide what to apply to `green-tinted` and `green-tinted-light`
  - Reference notes and the list of differences: `doc/syntax-highlighting-tonsky.md`
  - Keywords and function calls: keep as anchor and navigation layers, or drop to default text
  - Comments: highlight explanatory comments, grey only commented-out code
  - Colour count: compare the slot count with the "as many as you can remember" rule
  - Light theme: try background tints on tokens
  - Bold and italic: remove any use

### T-02: Apply Tonsky's principles to the dark theme
- ✓ DONE — Rework the `green-dark` palette so the `green-tinted` Neovim theme highlights only definitions, strings, constants and comments
  - Why: Jim finds the dark theme hard to read. About 20 slots each have their own colour, so nothing stands out
  - Brief with current state, target, constraints and verification: [tonsky-dark-theme-brief.md](tonsky-dark-theme-brief.md)
  - Keywords, calls, variables, properties and types go to default text; punctuation goes dim; comments become bright
  - Constraint: `mapping.lua` is shared with the light themes, which must render the same after the change
  - Takes over the dark theme part of Task 1
  - Result: comments yellow, definitions light blue, constants purple, strings green, all else default text, punctuation dim. Details in `doc/syntax-highlighting-tonsky.md`
  - Verified with `nvim --headless` highlight dumps: light theme dumps match before and after
  - Open: `rainbow-delimiters.nvim` keeps brackets bright green in every theme
