-- Green Dark palette
-- Dark green background. Syntax colours follow Tonsky's rules: highlight
-- definitions, strings, constants and comments; dim punctuation; leave
-- everything else as default text.

return {
  name = "green-dark",
  background = "dark",

  -- Canvas
  bg             = "#0d1f1a",
  bg_float       = "#132825",
  bg_highlight   = "#1a3d35",
  fg             = "#c8d8d0",
  fg_dim         = "#8fa89f",

  -- Syntax slots — Tonsky's rules (doc/syntax-highlighting-tonsky.md).
  -- Four highlight colours, each at a different lightness. Keywords,
  -- calls, constructors, variables, params, properties and types are not
  -- set, so they fall back to `text`.
  comment        = "#f0d860",     -- yellow, brightest (L 0.88)
  entity         = "#88c8ff",     -- definitions, light blue (L 0.81)
  type_definition = "#88c8ff",    -- type declarations: entity blue
  entity_ref     = "#c8d8d0",     -- type and module references: text
  constant       = "#c898e8",     -- constants and numbers, purple (L 0.75)
  number         = "#c898e8",
  string         = "#60b060",     -- strings and escapes, green (L 0.69)
  string_escape  = "#60b060",

  -- Punctuation: one dim colour
  operator       = "#5f7f6f",
  punctuation    = "#5f7f6f",
  bracket_top    = "#5f7f6f",
  string_interpolation = "#5f7f6f", -- ${...} delimiters

  text           = "#c8d8d0",     -- default text (same as fg)

  -- UI
  cursor         = "#7fd87f",
  selection      = "#2a4d45",
  line_number    = "#4a6860",
  folded         = "#4a6860",     -- folded line text; not the comment colour
  terminal_blue  = "#88c8ff",     -- :terminal ANSI 4/12; `type` is unset
  terminal_magenta = "#d8eb8b",   -- :terminal ANSI 5/13; kept from old entity
  search         = "#d8d888",
  search_current = "#ffff88",

  -- Diagnostics
  error          = "#ff6b6b",
  warning        = "#ffb86b",
  info           = "#88d8d8",
  hint           = "#88b8d8",

  -- Git
  git_add        = "#5ab85a",
  git_change     = "#ffb86b",
  git_delete     = "#ff6b6b",
}
