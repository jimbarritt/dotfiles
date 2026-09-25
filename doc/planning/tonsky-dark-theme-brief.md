# Brief: Apply Tonsky's Principles to the Dark Theme

Plan task: Delta: Syntax Highlighting Review / Task 2.

## Goal

Make the `green-tinted` dark Neovim theme more readable by applying the rules
in [../syntax-highlighting-tonsky.md](../syntax-highlighting-tonsky.md). Read
that doc first. Source article: <https://tonsky.me/blog/syntax-highlighting/>.

Jim looked at the dark theme again and found it hard to read. Too many token
types have their own colour, so nothing stands out.

## Scope

- In scope: the dark theme only. Entry point `config/nvim/colors/green-tinted.lua`,
  palette `config/nvim/lua/colorscheme/palettes/green-dark.lua`.
- Out of scope: `green-tinted-light`, `github-light`, Ghostty, tmux, Claude Code
  themes, diff colours. Task 1 covers the light theme later.

## How the theme is built

- `colors/green-tinted.lua` loads the `green-dark` palette and calls
  `colorscheme.apply`.
- `lua/colorscheme/mapping.lua` maps slot names to highlight groups. All three
  palettes share it.
- `lua/colorscheme/apply.lua` resolves each slot with the fallback
  `palette[slot] → palette.text → palette.fg`. Remove a slot from a palette and
  its groups take the default text colour.
- `lua/colorscheme/diagnose.lua` provides `:InspectLine`, which lists the
  highlight group and colour of each token on the cursor line.
- Design background: `doc/colorscheme-slot-system-design.md`,
  `doc/green-tinted-light-theme.md`.

## Constraint: the mapping is shared

A change to `mapping.lua` changes every theme. Prefer palette-only changes in
`green-dark.lua`. If a mapping change is needed, make the new slot fall back
to the current colour, so the light palettes render the same as before. Check
this with the highlight dump in Verification.

## Current state of `green-dark`

About 20 syntax slots, each with its own colour:

| Slot | Colour | Tonsky rule |
|------|--------|-------------|
| `keyword` `#5ab85a`, `keyword_control` `#88ff88` | bright greens | Do not highlight keywords |
| `fn_call` `#88d8c8`, `constructor` `#d8eb8b` | cyan, yellow-green | Do not highlight calls |
| `variable` `#789878`, `variable_local` `#6a8a6a`, `param` `#a8d8b8`, `property` `#98c8b8` | four sage greens | Do not highlight variable use |
| `type` `#88c8ff` | blue | Not in Tonsky's highlighted set |
| `entity` `#d8eb8b` | yellow-green | Highlight definitions: keep |
| `string` `#88a888`, `string_escape` `#98b898` | dull greens | Highlight: keep, make distinct |
| `constant` `#d8b888`, `number` `#98b8a8` | tan, grey-green | Highlight: merge into one colour |
| `comment` `#4a6860` | very dim | Highlight comments: make them readable |
| `bracket_top` `#7fd87f`, `string_interpolation` `#7fd87f` | bright green | Dim punctuation |
| `operator` `#5a7a6a`, `punctuation` `#4a6a5a` | dim | Dim punctuation: keep |

The `entity` slot also holds type references (`@lsp.type.class`,
`@lsp.type.struct`, `@lsp.type.interface`, `@module`, enum members), not only
declarations.

## Target

1. **Default text** for keywords, function calls, constructors, variables,
   parameters, properties and types. Remove those slots from `green-dark.lua`
   so they fall back to `text`, or set them to the `text` colour.
2. **Four highlight colours**, one per role:
   - Definitions: `entity`.
   - Strings: `string` and `string_escape`, one colour.
   - Constants and numbers: `constant` and `number`, one colour.
   - Comments: `comment`, bright and distinct. Tonsky uses yellow.
3. **Dim punctuation**: `operator`, `punctuation`, `bracket_top` and
   `string_interpolation` all go dim. `bracket_top` is bright green today.
4. **Different lightness per colour.** Do not tune the four colours to equal
   lightness. The eye separates lightness before hue.
5. **No bold or italic on code tokens.** No code group sets them today. Keep
   it that way.
6. **Definitions only, not references.** If type references still show in the
   `entity` colour after step 1, split them out of `entity` in `mapping.lua`
   into a slot that falls back to `entity`. `green-dark` sets that slot to
   `text`. The light palettes do not define it, so they render the same.

Keep the dark green background, UI, diagnostics and git colours as they are.

## Verification

- Dump every highlight group for each theme before and after the change:
  ```
  nvim --headless -c 'colorscheme green-tinted-light' \
    -c 'redir! > /tmp/hi-light.txt | silent hi | redir END' -c 'qa!'
  ```
  Do the same for `green-tinted` and `github-light`. The light dumps must match
  before and after. The dark dump shows the intended changes.
- If `nvim` is not installed in the session, install it. If that fails, list
  the checks for Jim to run on his machine.
- Jim reviews the result by eye on a Lua, Kotlin and Go file, using
  `:InspectLine` on any token that looks wrong.

## When done

- Update `doc/syntax-highlighting-tonsky.md` with what changed in the dark
  theme.
- Update the colour hierarchy table in `doc/green-tinted-light-theme.md` for
  the dark column.
- Mark Task 2 done in `doc/planning/plan.md`.
- Commit and push to `main`, per CLAUDE.md.
