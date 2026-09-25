# Syntax Highlighting: Tonsky's Principles

Reference notes on Nikita Prokopov (tonsky), "I am sorry, but everyone is getting syntax
highlighting wrong", 15 October 2025. Found via Martin Fowler's blog.

Source: <https://tonsky.me/blog/syntax-highlighting/>

## Core claim

Most colour themes highlight too much. When every token has a colour, no token
stands out. Highlighting exists to make a small set of things findable at a
glance.

## Rules from the article

| Topic | Rule |
|-------|------|
| Colour count | Use only as many colours as you can remember. Alabaster uses four. |
| Highlight | Strings, constants and numbers, top-level definitions, comments. |
| Do not highlight | Keywords (`if`, `class`, `function`), variable use, function calls. These make up about 75% of code. |
| Punctuation | Dim it. Names carry more meaning than brackets. |
| Comments | Highlight explanatory comments. Grey out only commented-out code. |
| Bold and italic | Avoid. They add a second highlighting dimension and do not replace colour. |
| Light themes | Dark text colours on white lose distinction. Use background tints on tokens to recover it. |
| Dark themes | Brighter colours on dark backgrounds give more contrast, so dark themes look more vibrant. |
| Uniform lightness | Do not tune all colours to equal lightness. The eye separates lightness before hue, so equal lightness makes colours hard to tell apart. |

## Alabaster colour assignment

| Colour | Token |
|--------|-------|
| Green | Strings |
| Purple | Constants |
| Yellow | Comments |
| Light blue | Top-level definitions |

Everything else stays default text colour.

## Comparison with the green-tinted themes

The `green-tinted` and `green-tinted-light` themes share the core idea: a
brightness hierarchy to reduce cognitive load, with variables, operators and
punctuation at the dimmest layer. See
[green-tinted-light-theme.md](green-tinted-light-theme.md) and
[colorscheme-slot-system-design.md](colorscheme-slot-system-design.md).

Points of difference to review:

- **Keywords.** The green-tinted themes treat keywords as structural anchors at
  the brightest layer. Tonsky leaves keywords unhighlighted.
- **Function calls.** The green-tinted themes give function calls a navigation
  layer colour. Tonsky leaves calls unhighlighted and highlights only top-level
  definitions.
- **Comments.** Check whether comments are dimmed. Tonsky highlights them and
  greys only commented-out code.
- **Slot count.** The slot system has about 15 slots. Tonsky caps colours at a
  number you can remember.
- **Light theme background tints.** Not used today. A candidate for
  `green-tinted-light` and the diff colours.
- **Bold and italic.** Check the palettes for any use.

The review task is in `doc/planning/plan.md` under Delta: Syntax Highlighting
Review.

## Applied to the dark theme (`green-tinted`)

Done in Delta: Syntax Highlighting Review, Task 2. The light themes are
unchanged: their highlight dumps match before and after.

### Colours

| Role | Slots | Before | After | OKLab L |
|------|-------|--------|-------|---------|
| Comments | `comment` | `#4a6860` very dim | `#f0d860` yellow | 0.88 |
| Default text | `text` | `#c8d8d0` | `#c8d8d0` | 0.87 |
| Definitions | `entity`, `type_definition` | `#d8eb8b` yellow-green | `#88c8ff` light blue | 0.81 |
| Constants and numbers | `constant`, `number` | `#d8b888`, `#98b8a8` | `#c898e8` purple | 0.75 |
| Strings | `string`, `string_escape` | `#88a888`, `#98b898` | `#60b060` green | 0.69 |
| Punctuation | `operator`, `punctuation`, `bracket_top`, `string_interpolation` | `#5a7a6a`, unused, `#7fd87f`, `#7fd87f` | `#5f7f6f` | 0.57 |

Removed from `green-dark.lua`, so they fall back to `text`: `keyword`,
`keyword_control`, `fn_call`, `constructor`, `variable`, `variable_local`,
`param`, `property`, `type`.

Comments sit just above default text in lightness and have the most
chroma, so they stand out most. The four highlight colours are spread from
L 0.69 to 0.88, not tuned to equal lightness.

### Mapping and applicator changes

The mapping is shared, so each change keeps the light palettes the same:

- `apply.lua` now resolves `slot → mapping._fallback[slot] → text → fg`.
- New slot `entity_ref` takes type, module and package references out of
  `entity` (`@module`, `@lsp.type.class`, `@lsp.type.struct`, and similar).
  It falls back to `entity`. `green-dark` sets it to `text`.
- New slot `type_definition` takes `@type.definition` out of `type`. It
  falls back to `type`. `green-dark` sets it to the definition colour.
- `Delimiter`, `@punctuation.delimiter` and `@punctuation.special` move from
  `text` to `punctuation`. The light palettes resolve `punctuation` to the
  text colour, so they render the same.
- UI colours that read syntax slots now read a dedicated field first:
  `Folded` reads `folded`, and `:terminal` ANSI blue and magenta read
  `terminal_blue` and `terminal_magenta`. `green-dark` sets these to the old
  values, so the UI does not change.

### Not covered

- `rainbow-delimiters.nvim` sets bracket colours in
  `config/nvim/lua/plugins/rainbow-delimiters.lua`, a bright-to-dim green
  gradient that starts at `#7fd87f`. It overrides `@punctuation.bracket`, so
  brackets stay bright. It applies to every theme, so the dark-only change
  leaves it alone.
- Commented-out code is not greyed. Tree-sitter does not separate it from
  explanatory comments.
- LSP semantic tokens are stopped in `lua/plugins/lsp.lua`, so the
  `@lsp.*` groups do not take effect today. Declaration and reference
  split for Kotlin classes therefore depends on Tree-sitter captures. If
  Tree-sitter captures a Kotlin class name in a declaration as `@type`, not
  `@type.definition`, the declaration shows as default text. Check with
  `:InspectLine`.
