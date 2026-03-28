-- Colorscheme applicator
--
-- Reads a palette and the shared mapping table, then calls
-- nvim_set_hl for every highlight group.
--
-- Options:
--   suppress_style (bool): suppress bold/italic on code tokens.
--     Prose groups (defined in mapping._prose_groups) are exempt (D2).

local M = {}

--- Resolve a slot name to a colour value from the palette.
--- Fallback chain: slot → text → fg
---@param palette table
---@param slot string
---@return string
local function resolve(palette, slot)
  return palette[slot] or palette.text or palette.fg
end

--- Apply a palette using the shared mapping.
---@param palette table  A palette table (from palettes/*.lua)
---@param opts? table    { suppress_style = boolean }
function M.apply(palette, opts)
  opts = opts or {}
  local suppress = opts.suppress_style or false
  local transparent = opts.transparent or false
  local mapping = require("colorscheme.mapping")
  local hi = vim.api.nvim_set_hl

  vim.cmd("hi clear")
  if vim.fn.exists("syntax_on") then
    vim.cmd("syntax reset")
  end

  vim.o.background = palette.background or "dark"

  local prose = mapping._prose_groups or {}

  -- Apply syntax groups from the mapping
  for slot, groups in pairs(mapping) do
    -- Skip internal keys (prefixed with _)
    if slot:sub(1, 1) == "_" then goto continue end

    -- Skip empty slot tables
    if #groups == 0 then goto continue end

    local fg = resolve(palette, slot)

    for _, entry in ipairs(groups) do
      local group, extra
      if type(entry) == "string" then
        group = entry
        extra = {}
      elseif type(entry) == "table" then
        group = entry[1]
        extra = entry[2] or {}
      end

      local hl_opts = { fg = fg }

      -- Handle special bg references (e.g. "_bg_float")
      if extra.bg then
        if type(extra.bg) == "string" and extra.bg:sub(1, 1) == "_" then
          hl_opts.bg = palette[extra.bg:sub(2)] or nil
        else
          hl_opts.bg = extra.bg
        end
      end

      if extra.sp then
        hl_opts.sp = extra.sp
      end

      -- Parse gui string into flags
      local wants_bold = extra.gui and extra.gui:find("bold") and true or false
      local wants_italic = extra.gui and extra.gui:find("italic") and true or false
      local wants_underline = extra.gui and extra.gui:find("underline") and true or false

      -- D2: suppress bold/italic on code, allow on prose
      if suppress and not prose[group] then
        hl_opts.bold = false
        hl_opts.italic = false
      else
        if wants_bold then hl_opts.bold = true end
        if wants_italic then hl_opts.italic = true end
      end

      if wants_underline then
        hl_opts.underline = true
      end

      hi(0, group, hl_opts)
    end

    ::continue::
  end

  -- Apply prose-only style groups (italic, bold, underline with slot colour)
  if mapping._prose_style_only then
    for _, entry in ipairs(mapping._prose_style_only) do
      local group = entry[1]
      local extra = entry[2] or {}
      local hl_opts = {}

      -- These groups get style attributes regardless of suppress_style
      if extra.gui then
        if extra.gui:find("bold") then hl_opts.bold = true end
        if extra.gui:find("italic") then hl_opts.italic = true end
        if extra.gui:find("underline") then hl_opts.underline = true end
      end

      -- Only set fg if the group has a slot colour; otherwise inherit
      -- from whatever was already set by the mapping
      hi(0, group, hl_opts)
    end
  end

  -- Apply UI highlights
  M.apply_ui(palette, suppress, transparent)
end

--- Apply non-syntax UI highlights.
---@param p table   The palette
---@param suppress boolean
---@param transparent boolean
function M.apply_ui(p, suppress, transparent)
  local hi = vim.api.nvim_set_hl
  local bg = transparent and "NONE" or p.bg

  -- Editor chrome
  hi(0, "Normal",       { fg = p.fg, bg = bg })
  hi(0, "NormalFloat",  { fg = p.fg, bg = p.bg_float or p.bg })
  hi(0, "FloatBorder",  { fg = p.fg_dim, bg = p.bg_float or p.bg })
  hi(0, "FloatTitle",   { fg = p.fg, bg = p.bg_float or p.bg, bold = not suppress })
  hi(0, "Cursor",       { fg = p.bg, bg = p.cursor or p.fg })
  hi(0, "CursorLine",   { bg = p.bg_highlight })
  hi(0, "CursorColumn", { bg = p.bg_highlight })
  hi(0, "ColorColumn",  { bg = p.bg_highlight })
  hi(0, "LineNr",       { fg = p.line_number or p.fg_dim })
  hi(0, "CursorLineNr", { fg = p.cursor_line_nr or p.cursor or p.fg })
  hi(0, "Visual",       { bg = p.selection })
  hi(0, "VisualNOS",    { bg = p.selection })
  hi(0, "Search",       { fg = p.fg, bg = p.search })
  hi(0, "IncSearch",    { fg = p.fg, bg = p.search_current })
  hi(0, "CurSearch",    { fg = p.fg, bg = p.search_current })
  hi(0, "MatchParen",   { fg = p.search_current, bold = not suppress })

  -- Status line
  hi(0, "StatusLine",   { fg = p.fg, bg = p.bg_float or p.bg })
  hi(0, "StatusLineNC", { fg = p.fg_dim, bg = p.bg_float or p.bg })
  hi(0, "WinSeparator", { fg = p.bg_highlight })

  -- Messages
  hi(0, "Title",        { fg = p.fg, bold = not suppress })
  hi(0, "MoreMsg",      { fg = p.git_add or p.tag or p.fg })
  hi(0, "ModeMsg",      { fg = p.fg, bold = not suppress })
  hi(0, "Question",     { fg = p.git_add or p.tag or p.fg })
  hi(0, "ErrorMsg",     { fg = p.error })
  hi(0, "WarningMsg",   { fg = p.warning })

  -- Popup menu
  hi(0, "Pmenu",        { fg = p.fg, bg = p.bg_float or p.bg })
  hi(0, "PmenuSel",     { fg = p.fg, bg = p.bg_highlight })
  hi(0, "PmenuSbar",    { bg = p.bg_float or p.bg })
  hi(0, "PmenuThumb",   { bg = p.bg_highlight })

  -- Tabs
  hi(0, "TabLine",      { fg = p.fg_dim, bg = p.bg_float or p.bg })
  hi(0, "TabLineFill",  { bg = p.bg_float or p.bg })
  hi(0, "TabLineSel",   { fg = p.fg, bg = p.bg })

  -- Signs & fold
  hi(0, "SignColumn",   { bg = bg })
  hi(0, "FoldColumn",   { fg = p.fg_dim, bg = p.bg })
  hi(0, "Folded",       { fg = p.comment or p.fg_dim, bg = p.bg_float or p.bg })

  -- Diff
  local diff_add_bg = p.background == "light" and "#e0f0e0" or p.bg_float
  local diff_del_bg = p.background == "light" and "#f0e0e0" or p.bg_float
  local diff_chg_bg = p.background == "light" and "#f0f0e0" or p.bg_float
  hi(0, "DiffAdd",      { fg = p.git_add or p.tag, bg = diff_add_bg })
  hi(0, "DiffDelete",   { fg = p.git_delete or p.error, bg = diff_del_bg })
  hi(0, "DiffChange",   { fg = p.git_change or p.warning, bg = diff_chg_bg })
  hi(0, "DiffText",     { fg = p.info or p.type, bg = diff_chg_bg })

  -- Git signs
  hi(0, "GitSignsAdd",    { fg = p.git_add or p.tag })
  hi(0, "GitSignsChange", { fg = p.git_change or p.warning })
  hi(0, "GitSignsDelete", { fg = p.git_delete or p.error })

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
  hi(0, "TelescopeBorder",    { fg = p.fg_dim })

  -- Misc
  hi(0, "Todo",         { fg = p.warning or p.keyword, bg = p.bg, bold = not suppress })
  hi(0, "Underlined",   { fg = p.info or p.type, underline = true })
  hi(0, "Error",        { fg = p.error, bg = p.bg })
  hi(0, "Debug",        { fg = p.error })

  -- Terminal colours (for :terminal)
  if p.background == "dark" then
    vim.g.terminal_color_0  = p.bg_float or "#132825"
    vim.g.terminal_color_8  = p.fg_dim or "#8fa89f"
  else
    vim.g.terminal_color_0  = "#1f2328"
    vim.g.terminal_color_8  = "#656d76"
  end
  vim.g.terminal_color_1  = p.error or "#cf222e"
  vim.g.terminal_color_9  = p.error or "#cf222e"
  vim.g.terminal_color_2  = p.git_add or p.tag or "#116329"
  vim.g.terminal_color_10 = p.git_add or p.tag or "#116329"
  vim.g.terminal_color_3  = p.warning or "#9a6700"
  vim.g.terminal_color_11 = p.warning or "#9a6700"
  vim.g.terminal_color_4  = p.type or p.info or "#0550ae"
  vim.g.terminal_color_12 = p.type or p.info or "#0550ae"
  vim.g.terminal_color_5  = p.entity or "#8250df"
  vim.g.terminal_color_13 = p.entity or "#8250df"
  vim.g.terminal_color_6  = p.info or "#0550ae"
  vim.g.terminal_color_14 = p.info or "#0550ae"
  vim.g.terminal_color_7  = p.fg or "#1f2328"
  vim.g.terminal_color_15 = p.fg or "#1f2328"
end

return M
