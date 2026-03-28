-- GitHub Light palette
-- Derived from Primer Design System's PrettyLights token set
-- https://primer.style/product/getting-started/react/theme-reference/

return {
  name = "github-light",
  background = "light",

  -- Canvas
  bg             = "#ffffff",
  bg_float       = "#f6f8fa",
  bg_highlight   = "#eaeef2",
  fg             = "#1f2328",
  fg_dim         = "#656d76",

  -- Syntax slots (aligned with PrettyLights)
  comment        = "#59636e",
  keyword        = "#cf222e",     -- pl-k: keywords, storage types
  keyword_control = "#cf222e",    -- return, break, continue — same as keyword on GitHub
  entity         = "#8250df",     -- pl-en: function decls, class names, headings
  type           = "#0550ae",     -- pl-c1: constants, numbers, types
  string         = "#0a3069",     -- pl-s: strings
  string_escape  = "#0550ae",     -- string escapes, special chars
  variable       = "#953800",     -- pl-v: variable builtins, params
  tag            = "#116329",     -- pl-ent/regexp: HTML tags, regexp, markup lists
  builtin        = "#0550ae",     -- stdlib types (String) and functions (println)
  operator       = "#cf222e",     -- =, +, - etc. — red on GitHub
  text           = "#1f2328",     -- default text: identifiers, operators, punctuation

  -- UI
  cursor         = "#0550ae",
  cursor_line_nr = "#1a7f37",     -- current line number — dark green
  selection      = "#ddf4ff",
  line_number    = "#8c959f",
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
