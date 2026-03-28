-- Green Dark palette
-- Extracted from green-tinted.lua — cognitive-load hierarchy with
-- green monochromatic theme. Retains finer-grained slots (D1).

return {
  name = "green-dark",
  background = "dark",

  -- Canvas
  bg             = "#0d1f1a",
  bg_float       = "#132825",
  bg_highlight   = "#1a3d35",
  fg             = "#c8d8d0",
  fg_dim         = "#8fa89f",

  -- Syntax slots — finer-grained than GitHub's 8
  comment        = "#4a6860",
  keyword        = "#5ab85a",     -- structural anchors
  keyword_control = "#88ff88",    -- return, break, continue
  entity         = "#d8eb8b",     -- fn declarations (yellow-green)
  fn_call        = "#88d8c8",     -- function calls (cyan-green)
  param          = "#a8d8b8",     -- parameters
  type           = "#88c8ff",     -- types (blue-green)
  string         = "#88a888",     -- strings (moderate green)
  string_escape  = "#98b898",     -- string escapes
  variable       = "#789878",     -- variables (muted sage)
  variable_local = "#6a8a6a",     -- local variables (even more muted)
  constant       = "#d8b888",     -- constants (warm tan-green)
  property       = "#98c8b8",     -- object properties
  bracket_top    = "#7fd87f",     -- structural brackets
  operator       = "#5a7a6a",     -- operators (very subtle)
  punctuation    = "#4a6a5a",     -- punctuation (barely visible)
  number         = "#98b8a8",     -- numbers
  text           = "#c8d8d0",     -- default text (same as fg)

  -- UI
  cursor         = "#7fd87f",
  selection      = "#2a4d45",
  line_number    = "#4a6860",
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
