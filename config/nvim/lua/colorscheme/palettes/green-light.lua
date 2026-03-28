-- Green Light palette
-- Extracted from green-tinted-light.lua — same hierarchy as green-dark,
-- inverted for a light background. Uses GitHub's Primer colours as the
-- base but inherits the green-dark slot structure.

return {
  name = "green-light",
  background = "light",

  -- Canvas
  bg             = "#ffffff",
  bg_float       = "#f0f4f0",
  bg_highlight   = "#e8efe8",
  fg             = "#1f2328",
  fg_dim         = "#656d76",

  -- Syntax slots
  comment        = "#59636e",
  keyword        = "#cf222e",
  keyword_control = "#cf222e",
  entity         = "#8250df",
  fn_call        = "#1f2328",     -- GitHub doesn't highlight calls
  param          = "#1f2328",     -- params are default text
  type           = "#0550ae",
  string         = "#0a3069",
  string_escape  = "#0550ae",
  variable       = "#1f2328",
  constant       = "#0550ae",
  property       = "#1f2328",
  bracket_top    = "#1f2328",
  operator       = "#1f2328",
  punctuation    = "#1f2328",
  number         = "#0550ae",
  text           = "#1f2328",

  -- UI
  cursor         = "#0550ae",
  selection      = "#ddf4ff",
  line_number    = "#8c959f",
  cursor_line_nr = "#1a7a2a",
  search         = "#fff8c5",
  search_current = "#ffdf5d",

  -- Diagnostics
  error          = "#cf222e",
  warning        = "#9a6700",
  info           = "#0550ae",
  hint           = "#0550ae",

  -- Git
  git_add        = "#1a7f37",
  git_change     = "#9a6700",
  git_delete     = "#cf222e",
}
