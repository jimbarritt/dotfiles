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
