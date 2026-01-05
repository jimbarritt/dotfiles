-- Green Tinted Neovim Color Scheme - Optimized for function block navigation
-- Based on cognitive load research and visual hierarchy principles
-- Save this file as ~/.config/nvim/colors/green-tinted.lua

vim.cmd('hi clear')
if vim.fn.exists('syntax_on') then
  vim.cmd('syntax reset')
end

vim.g.colors_name = 'green-tinted'

local colors = {
  -- Background & UI
  bg = '#0d1f1a',
  bg_light = '#132825',
  bg_highlight = '#1a3d35',
  fg = '#c8d8d0',
  fg_dim = '#8fa89f',
  cursor = '#7fd87f',
  selection = '#2a4d45',
  
  -- STRUCTURAL ANCHORS (Brightest - navigation beacons)
  keyword = '#5ab85a',           -- Matches rainbow level 3 (if/then/end blocks)
  keyword_control = '#88ff88',   -- Even brighter for return, break, continue
  fn_decl = '#d8eb8b',          -- Yellow-green - function declarations stand out
  bracket_top = '#7fd87f',       -- Matches rainbow level 1 (function/end delimiters)
  
  -- NAVIGATION (Medium brightness)
  fn_call = '#88d8c8',          -- Medium cyan-green for function calls
  param = '#a8d8b8',            -- Light green for parameters
  type = '#88c8ff',             -- Light blue-green for types
  constant = '#d8b888',         -- Warm tan-green for constants
  property = '#98c8b8',         -- For object properties
  
  -- CONTENT/NOISE REDUCTION (Dimmest)
  variable = '#789878',         -- Muted sage green for variables
  variable_local = '#6a8a6a',   -- Even more muted for locals
  operator = '#5a7a6a',         -- Very subtle operators
  punctuation = '#4a6a5a',      -- Barely visible punctuation
  string = '#88a888',           -- Moderate green for strings
  string_special = '#98b898',   -- Slightly brighter for escapes
  number = '#98b8a8',           -- Subtle blue-green for numbers
  comment = '#4a6860',          -- Muted gray-green for comments
  
  -- UI elements
  line_number = '#4a6860',
  visual = '#2a4d45',
  
  -- Special/Diagnostics
  search = '#d8d888',
  search_current = '#ffff88',
  error = '#ff6b6b',
  warning = '#ffb86b',
  hint = '#88b8d8',
  info = '#88d8d8',
  
  -- Legacy colors (kept for compatibility)
  black = '#132825',
  bright_black = '#1a3d35',
  bright_white = '#e8f8f0',
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
hi('Normal', { fg = colors.fg, bg = 'NONE' })
hi('NormalFloat', { fg = colors.fg, bg = colors.bg_light })
hi('Cursor', { fg = colors.bg, bg = colors.cursor })
hi('CursorLine', { bg = colors.bg_highlight })
hi('CursorColumn', { bg = colors.bg_highlight })
hi('LineNr', { fg = colors.line_number })
hi('CursorLineNr', { fg = colors.keyword })
hi('Visual', { bg = colors.visual })
hi('VisualNOS', { bg = colors.visual })
hi('Search', { fg = colors.bg, bg = colors.search })
hi('IncSearch', { fg = colors.bg, bg = colors.search_current })
hi('MatchParen', { fg = colors.search_current })
hi('StatusLine', { fg = '#00ff00', bg = '#1a3a1a' })
hi('StatusLineNC', { fg = '#00aa00', bg = '#1a3a1a' })

-- Folding
hi('Folded', { fg = colors.comment, bg = colors.bg_light })
hi('UfoFoldingVirtText', { fg = colors.comment })
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

-- SYNTAX HIGHLIGHTS (Traditional Vim groups)
hi('Comment', { fg = colors.comment, gui = 'italic' })

-- Constants (medium brightness)
hi('Constant', { fg = colors.constant })
hi('String', { fg = colors.string })
hi('Character', { fg = colors.string })
hi('Number', { fg = colors.number })
hi('Boolean', { fg = colors.constant })
hi('Float', { fg = colors.number })

-- Identifiers (dimmed for noise reduction)
hi('Identifier', { fg = colors.variable })
hi('Function', { fg = colors.fn_decl })  -- Yellow-green - navigation anchor

-- Statements (structural anchors - bright)
hi('Statement', { fg = colors.keyword })
hi('Conditional', { fg = colors.keyword })
hi('Repeat', { fg = colors.keyword })
hi('Label', { fg = colors.keyword })
hi('Operator', { fg = colors.operator })               -- Dimmed
hi('Keyword', { fg = colors.keyword })                 -- Bright green
hi('Exception', { fg = colors.keyword_control })

-- PreProc
hi('PreProc', { fg = colors.keyword })
hi('Include', { fg = colors.keyword })
hi('Define', { fg = colors.keyword })
hi('Macro', { fg = colors.keyword })
hi('PreCondit', { fg = colors.keyword })

-- Types (medium brightness for navigation)
hi('Type', { fg = colors.type })
hi('StorageClass', { fg = colors.type })
hi('Structure', { fg = colors.type })
hi('Typedef', { fg = colors.type })

-- Special
hi('Special', { fg = colors.bracket_top })
hi('SpecialChar', { fg = colors.string_special })
hi('Tag', { fg = colors.bracket_top })
hi('Delimiter', { fg = colors.punctuation })           -- Dimmed
hi('SpecialComment', { fg = colors.comment })
hi('Debug', { fg = colors.error })

-- Misc
hi('Underlined', { fg = colors.info, gui = 'underline' })
hi('Error', { fg = colors.error, bg = colors.bg })
hi('ErrorMsg', { fg = colors.error })
hi('WarningMsg', { fg = colors.warning })
hi('Todo', { fg = colors.search_current, bg = colors.bg })

-- Diff
hi('DiffAdd', { fg = colors.keyword, bg = colors.black })
hi('DiffChange', { fg = colors.warning, bg = colors.black })
hi('DiffDelete', { fg = colors.error, bg = colors.black })
hi('DiffText', { fg = colors.info, bg = colors.black })

-- Git signs
hi('GitSignsAdd', { fg = colors.keyword })
hi('GitSignsChange', { fg = colors.warning })
hi('GitSignsDelete', { fg = colors.error })

-- TREESITTER (Modern syntax highlighting)
-- Variables (dimmest - noise reduction)
hi('@variable', { fg = colors.variable })
hi('@variable.builtin', { fg = colors.variable })
hi('@variable.parameter', { fg = colors.param })       -- Medium for params
hi('@variable.member', { fg = colors.property })

-- Functions (Yellow-green for declarations, medium for calls)
hi('@function', { fg = colors.fn_decl })               -- Yellow-green
hi('@function.builtin', { fg = colors.fn_decl })
hi('@function.call', { fg = colors.fn_call })          -- Medium cyan-green
hi('@function.method', { fg = colors.fn_decl })
hi('@function.method.call', { fg = colors.fn_call })
hi('@constructor', { fg = colors.type })

-- Keywords (bright green - structural anchors)
hi('@keyword', { fg = colors.keyword })
hi('@keyword.function', { fg = colors.keyword })       -- THIS SHOULD FIX 'function' in Lua
hi('@keyword.return', { fg = colors.keyword_control })
hi('@keyword.operator', { fg = colors.keyword })
hi('@keyword.import', { fg = colors.keyword })
hi('@keyword.repeat', { fg = colors.keyword })
hi('@keyword.conditional', { fg = colors.keyword })

-- Lua-specific keyword fixes (explicitly set to prevent red)
hi('@keyword.function.lua', { fg = colors.keyword })   -- 'function' keyword
hi('@keyword.lua', { fg = colors.keyword })            -- 'end', 'do', etc.

-- Strings & literals
hi('@string', { fg = colors.string })
hi('@string.escape', { fg = colors.string_special })
hi('@string.special', { fg = colors.string_special })
hi('@character', { fg = colors.string })
hi('@number', { fg = colors.number })
hi('@boolean', { fg = colors.constant })
hi('@float', { fg = colors.number })

-- Types (medium - navigation)
hi('@type', { fg = colors.type })
hi('@type.builtin', { fg = colors.type })
hi('@type.definition', { fg = colors.type })
hi('@attribute', { fg = colors.type })

-- Other identifiers
hi('@constant', { fg = colors.constant })
hi('@constant.builtin', { fg = colors.constant })
hi('@namespace', { fg = colors.type })
hi('@property', { fg = colors.property })
hi('@parameter', { fg = colors.param })               -- Medium for navigation

-- Operators & punctuation (dimmest - noise reduction)
hi('@operator', { fg = colors.operator })
hi('@punctuation.bracket', { fg = colors.bracket_top }) -- Bright for structure
hi('@punctuation.delimiter', { fg = colors.punctuation }) -- Dim for commas etc
hi('@punctuation.special', { fg = colors.punctuation })

-- Comments
hi('@comment', { fg = colors.comment, gui = 'italic' })
hi('@comment.documentation', { fg = colors.comment, gui = 'italic' })

-- LSP Semantic Tokens (additional layer - this might be causing the red!)
-- Explicitly disable or set to our colors to prevent overrides
hi('@lsp.type.function', { fg = colors.fn_decl })
hi('@lsp.type.method', { fg = colors.fn_decl })
hi('@lsp.type.parameter', { fg = colors.param })
hi('@lsp.type.variable', { fg = colors.variable })
hi('@lsp.type.property', { fg = colors.property })
hi('@lsp.type.namespace', { fg = colors.type })
hi('@lsp.type.type', { fg = colors.type })
hi('@lsp.type.class', { fg = colors.type })
hi('@lsp.type.enum', { fg = colors.type })
hi('@lsp.type.interface', { fg = colors.type })
hi('@lsp.type.keyword', { fg = colors.keyword })      -- Prevent LSP from making keywords red
hi('@lsp.type.operator', { fg = colors.operator })
hi('@lsp.type.comment', { fg = colors.comment })

-- Additional LSP modifiers that might affect appearance
hi('@lsp.mod.readonly', { fg = colors.constant })
hi('@lsp.mod.defaultLibrary', { fg = colors.variable })

-- LSP Diagnostics
hi('DiagnosticError', { fg = colors.error })
hi('DiagnosticWarn', { fg = colors.warning })
hi('DiagnosticInfo', { fg = colors.info })
hi('DiagnosticHint', { fg = colors.hint })
hi('DiagnosticUnderlineError', { sp = colors.error, gui = 'underline' })
hi('DiagnosticUnderlineWarn', { sp = colors.warning, gui = 'underline' })
hi('DiagnosticUnderlineInfo', { sp = colors.info, gui = 'underline' })
hi('DiagnosticUnderlineHint', { sp = colors.hint, gui = 'underline' })

-- Telescope (if you use it)
hi('TelescopeSelection', { fg = colors.bright_white, bg = colors.bg_highlight })
hi('TelescopeMatching', { fg = colors.fn_decl })

-- nvim-tree (if you use it)
hi('NvimTreeFolderName', { fg = colors.type })
hi('NvimTreeOpenedFolderName', { fg = colors.keyword })
hi('NvimTreeRootFolder', { fg = colors.fn_decl })
hi('NvimTreeSpecialFile', { fg = colors.fn_decl })

-- Markdown
hi('@markup.heading', { fg = colors.fn_decl, gui = 'bold' })
hi('@markup.heading.1', { fg = colors.fn_decl, gui = 'bold' })
hi('@markup.heading.2', { fg = colors.fn_decl, gui = 'bold' })
hi('@markup.heading.3', { fg = colors.fn_decl, gui = 'bold' })
hi('@markup.heading.4', { fg = colors.fn_decl, gui = 'bold' })
hi('@markup.heading.5', { fg = colors.fn_decl, gui = 'bold' })
hi('@markup.heading.6', { fg = colors.fn_decl, gui = 'bold' })
hi('@markup.italic', { gui = 'italic' })
hi('@markup.bold', { gui = 'bold' })
hi('@markup.raw', { fg = colors.string })
hi('@markup.raw.block', { fg = colors.string })
hi('@markup.link', { fg = colors.type, gui = 'underline' })
hi('@markup.list', { fg = colors.keyword })
hi('@markup.list.checked', { fg = colors.keyword })
hi('@markup.list.unchecked', { fg = colors.keyword })

-- JSON (Vim syntax)
hi('jsonKey', { fg = colors.type })
hi('jsonString', { fg = colors.string })
hi('jsonNumber', { fg = colors.number })
hi('jsonBoolean', { fg = colors.constant })
hi('jsonNull', { fg = colors.keyword })
hi('jsonQuote', { fg = colors.punctuation })

-- Markdown (Vim syntax)
hi('markdownH1', { fg = colors.fn_decl })
hi('markdownH2', { fg = colors.fn_decl })
hi('markdownH3', { fg = colors.fn_decl })
hi('markdownBold', { fg = colors.keyword })
hi('markdownItalic', { gui = 'italic' })
hi('markdownCode', { fg = colors.string })
hi('markdownLink', { fg = colors.type })
