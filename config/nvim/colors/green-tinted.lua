-- Green Tinted Neovim Color Scheme
-- Save this file as ~/.config/nvim/colors/green-tinted.lua
-- Then add `vim.cmd('colorscheme green-tinted')` to your init.lua

vim.cmd('hi clear')
if vim.fn.exists('syntax_on') then
  vim.cmd('syntax reset')
end

vim.g.colors_name = 'green-tinted'

local colors = {
  bg = '#0a1f0a',
  fg = '#a8d5a8',
  cursor = '#66ff66',
  selection = '#1a4d1a',
  
  -- Normal colors
  black = '#1a3d1a',
  red = '#4d7a4d',
  green = '#66b366',
  yellow = '#7acc7a',
  blue = '#52a852',
  magenta = '#5cb35c',
  cyan = '#70d970',
  white = '#b8e6b8',
  
  -- Bright colors
  bright_black = '#2d5c2d',
  bright_red = '#6b9e6b',
  bright_green = '#85cc85',
  bright_yellow = '#a3e6a3',
  bright_blue = '#70c270',
  bright_magenta = '#7acc7a',
  bright_cyan = '#99ff99',
  bright_white = '#d9f2d9',
  
  -- UI colors
  line_number = '#4d7a4d',
  visual = '#1a4d1a',
  comment = '#5cb35c',
  string = '#85cc85',
  function_name = '#70c270',
  keyword = '#7acc7a',
  type = '#66b366',
  constant = '#99ff99',
}

local function hi(group, opts)
  local cmd = 'hi ' .. group
  if opts.fg then cmd = cmd .. ' guifg=' .. opts.fg end
  if opts.bg then cmd = cmd .. ' guibg=' .. opts.bg end
  if opts.gui then cmd = cmd .. ' gui=' .. opts.gui end
  if opts.sp then cmd = cmd .. ' guisp=' .. opts.sp end
  vim.cmd(cmd)
end

-- Editor highlights
hi('Normal', { fg = colors.fg, bg = colors.bg })
hi('NormalFloat', { fg = colors.fg, bg = colors.black })
hi('Cursor', { fg = colors.bg, bg = colors.cursor })
hi('CursorLine', { bg = colors.black })
hi('CursorColumn', { bg = colors.black })
hi('LineNr', { fg = colors.line_number })
hi('CursorLineNr', { fg = colors.bright_yellow })
hi('Visual', { bg = colors.visual })
hi('VisualNOS', { bg = colors.visual })
hi('Search', { fg = colors.bg, bg = colors.bright_yellow })
hi('IncSearch', { fg = colors.bg, bg = colors.cyan })
hi('MatchParen', { fg = colors.bright_cyan, gui = 'bold' })
hi('StatusLine', { fg = colors.bright_white, bg = colors.bright_black })
hi('StatusLineNC', { fg = colors.comment, bg = colors.black })
hi('VertSplit', { fg = colors.bright_black })
hi('Pmenu', { fg = colors.fg, bg = colors.black })
hi('PmenuSel', { fg = colors.bright_white, bg = colors.bright_black })
hi('PmenuSbar', { bg = colors.black })
hi('PmenuThumb', { bg = colors.bright_black })
hi('TabLine', { fg = colors.comment, bg = colors.black })
hi('TabLineFill', { bg = colors.black })
hi('TabLineSel', { fg = colors.bright_white, bg = colors.bg })
hi('SignColumn', { bg = colors.bg })
hi('Folded', { fg = colors.comment, bg = colors.black })
hi('FoldColumn', { fg = colors.comment, bg = colors.bg })

-- Syntax highlights
hi('Comment', { fg = colors.comment, gui = 'italic' })
hi('Constant', { fg = colors.constant })
hi('String', { fg = colors.string })
hi('Character', { fg = colors.string })
hi('Number', { fg = colors.constant })
hi('Boolean', { fg = colors.constant })
hi('Float', { fg = colors.constant })
hi('Identifier', { fg = colors.fg })
hi('Function', { fg = colors.function_name })
hi('Statement', { fg = colors.keyword })
hi('Conditional', { fg = colors.keyword })
hi('Repeat', { fg = colors.keyword })
hi('Label', { fg = colors.keyword })
hi('Operator', { fg = colors.fg })
hi('Keyword', { fg = colors.keyword })
hi('Exception', { fg = colors.red })
hi('PreProc', { fg = colors.magenta })
hi('Include', { fg = colors.magenta })
hi('Define', { fg = colors.magenta })
hi('Macro', { fg = colors.magenta })
hi('PreCondit', { fg = colors.magenta })
hi('Type', { fg = colors.type })
hi('StorageClass', { fg = colors.type })
hi('Structure', { fg = colors.type })
hi('Typedef', { fg = colors.type })
hi('Special', { fg = colors.cyan })
hi('SpecialChar', { fg = colors.cyan })
hi('Tag', { fg = colors.cyan })
hi('Delimiter', { fg = colors.fg })
hi('SpecialComment', { fg = colors.comment })
hi('Debug', { fg = colors.red })
hi('Underlined', { fg = colors.blue, gui = 'underline' })
hi('Error', { fg = colors.bright_red, bg = colors.bg })
hi('ErrorMsg', { fg = colors.bright_red })
hi('WarningMsg', { fg = colors.yellow })
hi('Todo', { fg = colors.bright_yellow, bg = colors.bg, gui = 'bold' })

-- Diff
hi('DiffAdd', { fg = colors.green, bg = colors.black })
hi('DiffChange', { fg = colors.yellow, bg = colors.black })
hi('DiffDelete', { fg = colors.red, bg = colors.black })
hi('DiffText', { fg = colors.blue, bg = colors.black })

-- Git signs
hi('GitSignsAdd', { fg = colors.green })
hi('GitSignsChange', { fg = colors.yellow })
hi('GitSignsDelete', { fg = colors.red })

-- Treesitter
hi('@variable', { fg = colors.fg })
hi('@variable.builtin', { fg = colors.magenta })
hi('@function', { fg = colors.function_name })
hi('@function.builtin', { fg = colors.function_name })
hi('@keyword', { fg = colors.keyword })
hi('@keyword.function', { fg = colors.keyword })
hi('@string', { fg = colors.string })
hi('@comment', { fg = colors.comment, gui = 'italic' })
hi('@constant', { fg = colors.constant })
hi('@type', { fg = colors.type })
hi('@parameter', { fg = colors.fg })
hi('@property', { fg = colors.cyan })
hi('@punctuation.bracket', { fg = colors.fg })
hi('@punctuation.delimiter', { fg = colors.fg })
hi('@constructor', { fg = colors.type })

-- LSP
hi('DiagnosticError', { fg = colors.red })
hi('DiagnosticWarn', { fg = colors.yellow })
hi('DiagnosticInfo', { fg = colors.blue })
hi('DiagnosticHint', { fg = colors.cyan })