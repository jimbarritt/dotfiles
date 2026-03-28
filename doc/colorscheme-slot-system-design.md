# Colorscheme Slot System — Architecture Design

## Problem

`green-tinted.lua` (307 lines) and `green-tinted-light.lua` (324 lines) share identical
mapping logic (which Neovim highlight groups map to which semantic slots) but differ only
in the palette values. Adding a new theme variant means duplicating all ~200 `hi()` calls.
Fixing a mapping bug (e.g. `@function.method.call` should map to `fn_call` not `fn_decl`)
requires editing every theme file.

GitHub solves this with PrettyLights: ~15 color slots, a single mapping from TextMate scopes
to those slots, and separate palette JSON files per theme variant. We want the same separation.

## Design

Three files, three concerns:

```
colors/
  green-tinted.lua          -- entry point: palette + require applicator
  green-tinted-light.lua    -- entry point: palette + require applicator

lua/colorscheme/
  palettes/
    green-dark.lua           -- returns palette table
    green-light.lua          -- returns palette table (current green-tinted-light colors)
    github-light.lua         -- returns palette table (pure GitHub Primer colors)
  mapping.lua                -- slot name → list of highlight groups (declarative)
  apply.lua                  -- reads palette + mapping, calls nvim_set_hl
```

### Layer 1: Palette

A palette is a plain Lua table. Slot names are shared across all palettes.
The slot taxonomy is derived from your existing `colors` tables, which already
align closely with GitHub's PrettyLights tokens:

```lua
-- lua/colorscheme/palettes/github-light.lua
return {
  name = "github-light",
  background = "light",  -- sets vim.o.background

  -- Canvas
  bg           = "#ffffff",
  bg_float     = "#f6f8fa",
  bg_highlight = "#eaeef2",
  fg           = "#1f2328",
  fg_dim       = "#656d76",

  -- Syntax slots (GitHub PrettyLights mapping)
  comment      = "#59636e",
  keyword      = "#cf222e",      -- prettylights: keyword
  entity       = "#8250df",      -- prettylights: entity (fn decl, class name)
  type         = "#0550ae",      -- prettylights: constant (types, numbers, constants)
  string       = "#0a3069",      -- prettylights: string
  variable     = "#953800",      -- prettylights: variable (params, builtins)
  tag          = "#116329",      -- prettylights: entity.tag (HTML tags, regexp)
  text         = "#1f2328",      -- default text (identifiers, operators, punctuation)

  -- UI
  cursor       = "#0550ae",
  selection    = "#ddf4ff",
  line_number  = "#8c959f",
  search       = "#fff8c5",
  search_current = "#ffdf5d",

  -- Diagnostics
  error        = "#cf222e",
  warning      = "#9a6700",
  info         = "#0550ae",
  hint         = "#0550ae",
}
```

Note: only **8 syntax slots** (comment, keyword, entity, type, string, variable,
tag, text). This matches GitHub's actual granularity. Your green-tinted palettes
use more slots (fn_decl, fn_call, param, property, bracket_top, etc.) — that's
fine, the palette can have more slots than GitHub uses. The mapping layer handles
either shape.

For the green-dark palette, slots like `fn_decl`, `fn_call`, `param` would remain
as separate values. For the github-light palette, many of those collapse to `text`
because GitHub doesn't differentiate them.

### Layer 2: Mapping

A single declarative table mapping slot names to lists of Neovim highlight groups.
No imperative code, no `nvim_set_hl` calls. Just data.

```lua
-- lua/colorscheme/mapping.lua
--
-- Each entry: slot_name = { list of highlight groups }
-- The slot_name must exist in the palette.
-- If a slot_name doesn't exist in a given palette, it falls back
-- to 'text' (for syntax) or 'fg' (for UI).
--
-- Format per group: { group_name, opts_overrides }
-- opts_overrides can add bg, gui, sp on top of the slot's fg color.
-- A bare string means { group_name, {} } (fg only, no extras).

return {
  -- ── Syntax ──────────────────────────────────────────────

  comment = {
    "Comment",
    "@comment",
    "@comment.documentation",
    "SpecialComment",
    "@lsp.type.comment",
  },

  keyword = {
    "Statement", "Conditional", "Repeat", "Label", "Keyword", "Exception",
    "PreProc", "Include", "Define", "Macro", "PreCondit",
    "@keyword", "@keyword.function", "@keyword.return", "@keyword.operator",
    "@keyword.import", "@keyword.repeat", "@keyword.conditional",
    "@keyword.function.lua", "@keyword.lua",
    "@keyword.kotlin",
    "@lsp.type.keyword",
    "@lsp.type.keyword.kotlin", "@lsp.keyword.kotlin",
  },

  entity = {
    "Function",
    "@function", "@function.builtin", "@function.method",
    "@module",
    "@lsp.type.method",
    "@lsp.typemod.function.declaration",
    "@lsp.typemod.function.declaration.kotlin",
    "@lsp.typemod.class.declaration",
    "@lsp.typemod.class.declaration.kotlin",
    "@function.kotlin",
    -- Markup headings also use entity color
    { "@markup.heading",   { gui = "bold" } },
    { "@markup.heading.1", { gui = "bold" } },
    { "@markup.heading.2", { gui = "bold" } },
    { "@markup.heading.3", { gui = "bold" } },
    { "@markup.heading.4", { gui = "bold" } },
    { "@markup.heading.5", { gui = "bold" } },
    { "@markup.heading.6", { gui = "bold" } },
    { "markdownH1",        { gui = "bold" } },
    { "markdownH2",        { gui = "bold" } },
    { "markdownH3",        { gui = "bold" } },
    "NvimTreeRootFolder", "NvimTreeSpecialFile",
  },

  -- fn_call: falls back to 'text' in github-light, separate color in green-dark
  fn_call = {
    "@function.call", "@function.method.call",
    "@function.builtin",  -- could also be entity; debatable
  },

  type = {
    "Type", "StorageClass", "Structure", "Typedef",
    "Constant", "Boolean",
    "@type", "@type.builtin", "@type.definition",
    "@attribute", "@constant", "@constant.builtin",
    "@namespace",
    "@number", "@float", "Number", "Float",
    "@lsp.type.type", "@lsp.type.class", "@lsp.type.enum",
    "@lsp.type.interface", "@lsp.type.namespace",
    "@lsp.mod.readonly",
    "Directory", "NvimTreeFolderName",
    "@type.kotlin",
    "jsonBoolean",
  },

  string = {
    "String", "Character",
    "@string", "@character",
    "@string.special",
    "@markup.raw", "@markup.raw.block",
    "markdownCode",
    "jsonString",
  },

  string_escape = {
    "SpecialChar",
    "@string.escape",
  },

  variable = {
    "@variable.builtin",
    "@variable.parameter", "@parameter",
    "@lsp.type.parameter",
  },

  tag = {
    "@string.regexp",
    "@markup.list", "@markup.list.checked", "@markup.list.unchecked",
    "markdownBold",
    "NvimTreeOpenedFolderName",
  },

  text = {
    -- Everything that should be the default fg color
    "Identifier",
    "@variable", "@variable.member",
    "@property",
    "Operator", "@operator",
    "Delimiter", "@punctuation.delimiter", "@punctuation.special",
    "Special", "Tag", "@punctuation.bracket",
    "@lsp.type.variable", "@lsp.type.property",
    "@lsp.type.operator", "@lsp.type.function", -- LSP 'function' = reference, not decl
    "@lsp.mod.defaultLibrary",
    "@variable.kotlin", "@property.kotlin",
    "jsonKey", "jsonQuote", "jsonNull",
    "NonText", "EndOfBuffer",
    "NvimTreeNormal", "NvimTreeNormalNC",
    "NvimTreeIndentMarker",
  },

  -- ── UI (non-syntax, separate from the slot system) ────

  -- These are handled directly by the applicator using
  -- palette keys like bg, fg, selection, etc.
}
```

**Key design choice**: when a palette doesn't define a slot (e.g. github-light
has no `fn_call`), the mapping falls back to `text`. This is how we get GitHub's
intentional simplicity — many captures collapse to the default text color.

When your green-dark palette *does* define `fn_call = '#88d8c8'`, those same
captures get a distinct color. Same mapping, different palette, different result.

### Layer 3: Applicator

The engine that reads palette + mapping and calls `nvim_set_hl`. This is the only
file that touches the Neovim API.

```lua
-- lua/colorscheme/apply.lua

local M = {}

--- Apply a palette using the shared mapping
---@param palette table  The palette (from palettes/*.lua)
---@param opts? table    Options: { suppress_style = true }
function M.apply(palette, opts)
  opts = opts or {}
  local suppress = opts.suppress_style or false
  local mapping = require("colorscheme.mapping")

  vim.cmd("hi clear")
  if vim.fn.exists("syntax_on") then
    vim.cmd("syntax reset")
  end

  vim.o.background = palette.background or "dark"

  -- Resolve a slot name to a color value.
  -- Falls back: slot → text → fg
  local function resolve(slot)
    return palette[slot] or palette.text or palette.fg
  end

  -- Apply syntax groups from the mapping
  for slot, groups in pairs(mapping) do
    local fg = resolve(slot)
    for _, entry in ipairs(groups) do
      local group, extra
      if type(entry) == "string" then
        group = entry
        extra = {}
      else
        group = entry[1]
        extra = entry[2] or {}
      end

      local hl_opts = { fg = fg }
      if extra.bg then hl_opts.bg = extra.bg end
      if extra.sp then hl_opts.sp = extra.sp end

      -- Parse gui string into individual flags
      if extra.gui then
        if extra.gui:find("bold") then hl_opts.bold = true end
        if extra.gui:find("italic") then hl_opts.italic = true end
        if extra.gui:find("underline") then hl_opts.underline = true end
      end

      -- D2: suppress bold/italic for code, allow for prose
      if suppress and not M.prose_groups[group] then
        hl_opts.bold = false
        hl_opts.italic = false
      end

      vim.api.nvim_set_hl(0, group, hl_opts)
    end
  end

  -- Apply UI highlights (not part of the slot mapping)
  M.apply_ui(palette, suppress)
end

function M.apply_ui(p, suppress)
  local hi = vim.api.nvim_set_hl
  hi(0, "Normal",       { fg = p.fg, bg = p.bg })
  hi(0, "NormalFloat",   { fg = p.fg, bg = p.bg_float or p.bg })
  hi(0, "Cursor",       { fg = p.bg, bg = p.cursor or p.fg })
  hi(0, "CursorLine",   { bg = p.bg_highlight })
  hi(0, "CursorColumn", { bg = p.bg_highlight })
  hi(0, "LineNr",        { fg = p.line_number or p.fg_dim })
  hi(0, "CursorLineNr", { fg = p.cursor or p.fg })
  hi(0, "Visual",       { bg = p.selection })
  hi(0, "VisualNOS",    { bg = p.selection })
  hi(0, "Search",       { fg = p.fg, bg = p.search })
  hi(0, "IncSearch",    { fg = p.fg, bg = p.search_current })
  hi(0, "MatchParen",   { fg = p.search_current, bold = not suppress })
  hi(0, "StatusLine",   { fg = p.fg, bg = p.bg_float or p.bg })
  hi(0, "StatusLineNC", { fg = p.fg_dim, bg = p.bg_float or p.bg })
  hi(0, "SignColumn",   { bg = p.bg })
  -- ... remaining UI groups (pmenu, tabline, folding, etc.)

  -- Diagnostics
  hi(0, "DiagnosticError", { fg = p.error })
  hi(0, "DiagnosticWarn",  { fg = p.warning })
  hi(0, "DiagnosticInfo",  { fg = p.info })
  hi(0, "DiagnosticHint",  { fg = p.hint })
  hi(0, "DiagnosticUnderlineError", { sp = p.error, underline = true })
  hi(0, "DiagnosticUnderlineWarn",  { sp = p.warning, underline = true })
  hi(0, "DiagnosticUnderlineInfo",  { sp = p.info, underline = true })
  hi(0, "DiagnosticUnderlineHint",  { sp = p.hint, underline = true })

  -- Telescope
  hi(0, "TelescopeSelection", { fg = p.fg, bg = p.bg_highlight })
  hi(0, "TelescopeMatching",  { fg = p.entity or p.fg })

  -- Git signs
  hi(0, "GitSignsAdd",    { fg = p.tag or p.keyword })
  hi(0, "GitSignsChange", { fg = p.warning })
  hi(0, "GitSignsDelete", { fg = p.error })
end

return M
```

### Entry Points (colors/*.lua)

These become tiny — just palette selection + applicator call:

```lua
-- colors/github-light.lua
vim.g.colors_name = "github-light"
local palette = require("colorscheme.palettes.github-light")
require("colorscheme.apply").apply(palette, { suppress_style = true })
```

```lua
-- colors/green-tinted.lua
vim.g.colors_name = "green-tinted"
local palette = require("colorscheme.palettes.green-dark")
require("colorscheme.apply").apply(palette, { suppress_style = false })
```

Each entry point is 3 lines.

## Migration Path

1. Create `lua/colorscheme/` directory structure
2. Extract palettes from existing `colors` tables (mechanical)
3. Build the mapping table from the existing `hi()` calls (mechanical)
4. Write the applicator (new code, ~100 lines)
5. Replace the 300-line `colors/*.lua` files with 3-line entry points
6. Add `palettes/github-light.lua` with pure Primer values
7. Test: `:colorscheme github-light` should look like GitHub

## Notes: Why GitHub Looks Better Than Neovim (Even With Identical Colours)

Even with a pixel-perfect colour match, the browser rendering of GitHub code
will look marginally better than Neovim in a terminal. This is inherent to the
medium and worth understanding so we don't chase perfection on things we can't
control, and do address the things we can.

**Factors we cannot control:**

- **Font rasterisation**: Zen Browser uses CoreText (macOS) via Gecko, which has
  decades of refinement for sub-pixel anti-aliasing, fractional kerning, and LCD
  hinting. Ghostty uses its own GPU text renderer — good, but different. Same font,
  same size, subtly different glyph edges.

- **Colour management**: Browser renders `#ffffff` through the display's ICC profile
  (colour-managed). Terminal may or may not. On the MacBook Pro's wide-gamut display,
  the same hex value can render slightly differently.

- **Proportional metrics in a monospace grid**: The browser's text layout engine
  handles optical alignment and baseline positioning with extreme precision. Terminal
  emulators render each character into a cell grid, which can introduce micro-
  misalignments the eye registers subconsciously.

**Factors we can address:**

- **Line height**: GitHub uses `line-height: 20px` at `font-size: 12px` (1.667 ratio).
  Terminal defaults are tighter. Ghostty config: `adjust-cell-height` can increase
  vertical breathing room to approximate this.

- **Horizontal padding**: GitHub code blocks have ~16px left padding before the first
  character. In Neovim, text starts right after line numbers. Increasing `numberwidth`,
  using `signcolumn=yes:2`, or adding a left padding plugin can help.

- **Code framing**: GitHub code sits inside a bordered, padded container with a
  slightly different background from the page. This "presented" framing makes code
  feel polished. Neovim fills edge-to-edge, which reads as utilitarian. Not easily
  replicated but worth knowing.

- **Font size**: Matching GitHub's 12px/13px for SF Mono in Ghostty brings the overall
  density closer. Default terminal font sizes tend to be larger.

## Decision Log

Decisions made during design. Recorded so we can revisit and try alternatives later.

### D1: Green-dark palette granularity
**Decision**: Keep the finer-grained slots (`fn_decl`, `fn_call`, `param`,
`property`, `bracket_top`, etc.). Do not collapse to GitHub's 8 slots.
**Rationale**: The mapping falls back gracefully when a palette doesn't define
a slot (→ `text` → `fg`), so green-dark can be richer without affecting
github-light. We may rationalise later once both palettes are working.
**Alternative**: Collapse green-dark to the same 8 slots as GitHub. Would
simplify the palette but lose the visual hierarchy that makes the dark theme
distinctive.

### D2: Style suppression (bold/italic)
**Decision**: Suppress bold/italic for all *code* tokens. Allow bold/italic
for *prose* groups (`@markup.heading`, `@markup.bold`, `@markup.italic`,
`markdownH*`, `markdownBold`, `markdownItalic`). This matches GitHub's
actual behaviour — code is uniform weight, prose retains typographic emphasis.
**Implementation**: The applicator maintains a `prose_groups` set. Groups in
this set get their `gui` attributes applied even when `suppress_style = true`.
All other groups have bold/italic forced to `false`.
**Alternative**: Full suppression everywhere (no bold/italic even on prose).
Simpler but loses readability in markdown.

### D3: @markup.heading slot
**Decision**: Keep headings in the `entity` slot. No separate `heading` slot.
**Rationale**: Matches GitHub (headings are the same colour as function
declarations / class names — purple in light, yellow-green in dark). Simpler
mapping with fewer slots to manage.
**Alternative**: Separate `heading` slot that falls back to `entity`. Would
allow heading colour to diverge between themes independently.

### D4: @lsp.type.function mapping
**Decision**: Map `@lsp.type.function` to the `fn_call` slot.
**Rationale**: In github-light, `fn_call` is not defined in the palette, so
it falls back to `text` — matching GitHub's behaviour (function references
are unstyled). In green-dark, `fn_call` is defined as `#88d8c8`, so function
references get the cyan-green colour automatically. Both palettes get the
right result without any overrides.
**Context**: LSP semantic tokens have higher priority than tree-sitter captures.
`@lsp.type.function` fires for function references (calls, arguments, etc.).
If mapped to `text`, green-dark would lose its fn_call colouring unless an
explicit override was added in the entry point.
**Alternative A**: Map to `text`, require green-dark entry point to override.
**Alternative B**: New `fn_ref` slot — adds complexity for no benefit over
using `fn_call` with fallback.
