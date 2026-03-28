-- Green Tinted Light — companion to green-tinted (dark)
-- Same cognitive-load hierarchy, inverted for a light background.
--
-- Hierarchy (darkest = most important on light bg):
--   STRUCTURAL ANCHORS  – darkest / most saturated (keywords, fn decls)
--   NAVIGATION           – medium saturation (fn calls, types, params)
--   CONTENT / NOISE      – lightest / least saturated (variables, operators)

vim.cmd('hi clear')
if vim.fn.exists('syntax_on') then
  vim.cmd('syntax reset')
end

vim.g.colors_name = 'green-tinted-light'
vim.o.background = 'light'

local c = {
  -- Background & UI
  bg           = '#ffffff',          -- pure white
  bg_light     = '#f0f4f0',          -- slightly darker for floats/popups
  bg_highlight = '#e8efe8',          -- cursor line, visual
  fg           = '#1f2328',          -- GitHub default text
  fg_dim       = '#656d76',          -- GitHub secondary text
  cursor       = '#0550ae',
  selection    = '#ddf4ff',

  -- STRUCTURAL ANCHORS — GitHub red for keywords
  keyword         = '#cf222e',       -- GitHub keyword red
  keyword_control = '#cf222e',       -- same red for return/break
  fn_decl         = '#8250df',       -- GitHub purple for function declarations
  bracket_top     = '#1f2328',       -- brackets match default text

  -- NAVIGATION
  fn_call  = '#1f2328',             -- GitHub doesn't highlight calls
  param    = '#1f2328',             -- params are default text
  type     = '#0550ae',             -- GitHub blue for types
  constant = '#0550ae',             -- constants are blue
  property = '#1f2328',             -- properties are default text

  -- CONTENT
  variable       = '#1f2328',        -- default text
  variable_local = '#1f2328',        -- default text
  operator       = '#1f2328',        -- default text
  punctuation    = '#1f2328',        -- default text
  string         = '#0a3069',        -- GitHub dark blue for strings
  string_special = '#0550ae',        -- escapes slightly lighter blue
  number         = '#0550ae',        -- numbers are blue
  comment        = '#6e7781',        -- GitHub gray for comments

  -- UI elements
  line_number = '#8c959f',
  visual      = '#ddf4ff',

  -- Diagnostics
  search         = '#fff8c5',
  search_current = '#ffdf5d',
  error          = '#cf222e',
  warning        = '#9a6700',
  hint           = '#0550ae',
  info           = '#0550ae',

  -- Legacy
  black        = '#f6f8fa',
  bright_black = '#eaeef2',
  bright_white = '#1f2328',
}

local function hi(group, opts)
  local cmd = 'hi ' .. group
  if opts.fg then cmd = cmd .. ' guifg=' .. opts.fg end
  if opts.bg then cmd = cmd .. ' guibg=' .. opts.bg end
  if opts.gui then cmd = cmd .. ' gui=' .. opts.gui end
  if opts.sp then cmd = cmd .. ' guisp=' .. opts.sp end
  vim.cmd(cmd)
end

-- ── Editor ──────────────────────────────────────────────────────────
hi('Normal',       { fg = c.fg, bg = c.bg })
hi('NormalFloat',  { fg = c.fg, bg = c.bg_light })
hi('Cursor',       { fg = c.bg, bg = c.cursor })
hi('CursorLine',   { bg = c.bg_highlight })
hi('CursorColumn', { bg = c.bg_highlight })
hi('LineNr',       { fg = c.line_number })
hi('CursorLineNr', { fg = c.keyword })
hi('Visual',       { bg = c.visual })
hi('VisualNOS',    { bg = c.visual })
hi('Search',       { fg = c.fg, bg = c.search })
hi('IncSearch',    { fg = c.fg, bg = c.search_current })
hi('MatchParen',   { fg = c.search_current, gui = 'bold' })
hi('StatusLine',   { fg = '#1f2328', bg = '#f6f8fa' })
hi('StatusLineNC', { fg = '#656d76', bg = '#f6f8fa' })

-- Folding & chrome
hi('Folded',       { fg = c.comment, bg = c.bg_light })
hi('UfoFoldingVirtText', { fg = c.comment })
hi('VertSplit',    { fg = c.bright_black })
hi('Pmenu',        { fg = c.fg, bg = c.bg_light })
hi('PmenuSel',     { fg = c.bright_white, bg = c.bg_highlight })
hi('PmenuSbar',    { bg = c.bg_light })
hi('PmenuThumb',   { bg = c.bright_black })
hi('TabLine',      { fg = c.comment, bg = c.bg_light })
hi('TabLineFill',  { bg = c.bg_light })
hi('TabLineSel',   { fg = c.fg, bg = c.bg })
hi('SignColumn',   { bg = c.bg })
hi('FoldColumn',   { fg = c.comment, bg = c.bg })

-- ── Traditional syntax ──────────────────────────────────────────────
hi('Comment',   { fg = c.comment, gui = 'italic' })

hi('Constant',  { fg = c.constant })
hi('String',    { fg = c.string })
hi('Character', { fg = c.string })
hi('Number',    { fg = c.number })
hi('Boolean',   { fg = c.constant })
hi('Float',     { fg = c.number })

hi('Identifier', { fg = c.variable })
hi('Function',   { fg = c.fn_decl })

hi('Statement',   { fg = c.keyword })
hi('Conditional', { fg = c.keyword })
hi('Repeat',      { fg = c.keyword })
hi('Label',       { fg = c.keyword })
hi('Operator',    { fg = c.operator })
hi('Keyword',     { fg = c.keyword })
hi('Exception',   { fg = c.keyword_control })

hi('PreProc',   { fg = c.keyword })
hi('Include',   { fg = c.keyword })
hi('Define',    { fg = c.keyword })
hi('Macro',     { fg = c.keyword })
hi('PreCondit', { fg = c.keyword })

hi('Type',         { fg = c.type })
hi('StorageClass', { fg = c.type })
hi('Structure',    { fg = c.type })
hi('Typedef',      { fg = c.type })

hi('Special',        { fg = c.bracket_top })
hi('SpecialChar',    { fg = c.string_special })
hi('Tag',            { fg = c.bracket_top })
hi('Delimiter',      { fg = c.punctuation })
hi('SpecialComment', { fg = c.comment })
hi('Debug',          { fg = c.error })

hi('Underlined', { fg = c.info, gui = 'underline' })
hi('Error',      { fg = c.error, bg = c.bg })
hi('ErrorMsg',   { fg = c.error })
hi('WarningMsg', { fg = c.warning })
hi('Todo',       { fg = c.keyword_control, bg = c.bg, gui = 'bold' })

-- Diff
hi('DiffAdd',    { fg = c.keyword, bg = '#e0f0e0' })
hi('DiffChange', { fg = c.warning, bg = '#f0f0e0' })
hi('DiffDelete', { fg = c.error, bg = '#f0e0e0' })
hi('DiffText',   { fg = c.info, bg = '#e0f0f0' })

-- Git signs
hi('GitSignsAdd',    { fg = c.keyword })
hi('GitSignsChange', { fg = c.warning })
hi('GitSignsDelete', { fg = c.error })

-- ── TreeSitter ──────────────────────────────────────────────────────
-- Variables (lightest — noise reduction)
hi('@variable',           { fg = c.variable })
hi('@variable.builtin',   { fg = c.variable })
hi('@variable.parameter', { fg = c.param })
hi('@variable.member',    { fg = c.property })

-- Functions
hi('@function',             { fg = c.fn_decl })
hi('@function.builtin',     { fg = c.fn_decl })
hi('@function.call',        { fg = c.fn_call })
hi('@function.method',      { fg = c.fn_decl })
hi('@function.method.call', { fg = c.fn_call })
hi('@constructor',          { fg = c.type })

-- Keywords (darkest — structural anchors)
hi('@keyword',             { fg = c.keyword })
hi('@keyword.function',    { fg = c.keyword })
hi('@keyword.return',      { fg = c.keyword_control })
hi('@keyword.operator',    { fg = c.keyword })
hi('@keyword.import',      { fg = c.keyword })
hi('@keyword.repeat',      { fg = c.keyword })
hi('@keyword.conditional', { fg = c.keyword })

-- Lua-specific
hi('@keyword.function.lua', { fg = c.keyword })
hi('@keyword.lua',          { fg = c.keyword })

-- Strings & literals
hi('@string',         { fg = c.string })
hi('@string.escape',  { fg = c.string_special })
hi('@string.special', { fg = c.string_special })
hi('@character',      { fg = c.string })
hi('@number',         { fg = c.number })
hi('@boolean',        { fg = c.constant })
hi('@float',          { fg = c.number })

-- Types (medium — navigation)
hi('@type',            { fg = c.type })
hi('@type.builtin',    { fg = c.type })
hi('@type.definition', { fg = c.type })
hi('@attribute',       { fg = c.type })

-- Other identifiers
hi('@constant',         { fg = c.constant })
hi('@constant.builtin', { fg = c.constant })
hi('@namespace',        { fg = c.type })
hi('@property',         { fg = c.property })
hi('@parameter',        { fg = c.param })

-- Operators & punctuation
hi('@operator',              { fg = c.operator })
hi('@punctuation.bracket',   { fg = c.bracket_top })
hi('@punctuation.delimiter', { fg = c.punctuation })
hi('@punctuation.special',   { fg = c.punctuation })

-- Comments
hi('@comment',               { fg = c.comment, gui = 'italic' })
hi('@comment.documentation', { fg = c.comment, gui = 'italic' })

-- ── LSP Semantic Tokens ─────────────────────────────────────────────
hi('@lsp.type.function',  { fg = c.fn_decl })
hi('@lsp.type.method',    { fg = c.fn_decl })
hi('@lsp.type.parameter', { fg = c.param })
hi('@lsp.type.variable',  { fg = c.variable })
hi('@lsp.type.property',  { fg = c.property })
hi('@lsp.type.namespace', { fg = c.type })
hi('@lsp.type.type',      { fg = c.type })
hi('@lsp.type.class',     { fg = c.type })
hi('@lsp.type.enum',      { fg = c.type })
hi('@lsp.type.interface', { fg = c.type })
hi('@lsp.type.keyword',   { fg = c.keyword })
hi('@lsp.type.operator',  { fg = c.operator })
hi('@lsp.type.comment',   { fg = c.comment })

hi('@lsp.mod.readonly',       { fg = c.constant })
hi('@lsp.mod.defaultLibrary', { fg = c.variable })

-- ── Diagnostics ─────────────────────────────────────────────────────
hi('DiagnosticError', { fg = c.error })
hi('DiagnosticWarn',  { fg = c.warning })
hi('DiagnosticInfo',  { fg = c.info })
hi('DiagnosticHint',  { fg = c.hint })
hi('DiagnosticUnderlineError', { sp = c.error, gui = 'underline' })
hi('DiagnosticUnderlineWarn',  { sp = c.warning, gui = 'underline' })
hi('DiagnosticUnderlineInfo',  { sp = c.info, gui = 'underline' })
hi('DiagnosticUnderlineHint',  { sp = c.hint, gui = 'underline' })

-- ── Telescope ───────────────────────────────────────────────────────
hi('TelescopeSelection', { fg = c.fg, bg = c.bg_highlight })
hi('TelescopeMatching',  { fg = c.fn_decl, gui = 'bold' })

-- ── nvim-tree ───────────────────────────────────────────────────────
hi('Directory',                  { fg = c.type })
hi('NvimTreeFolderName',       { fg = c.type })
hi('NvimTreeOpenedFolderName', { fg = c.keyword })
hi('NvimTreeRootFolder',       { fg = c.fn_decl })
hi('NvimTreeSpecialFile',      { fg = c.fn_decl })
hi('NvimTreeNormal',           { fg = c.fg })
hi('NvimTreeNormalNC',         { fg = c.fg })
hi('NvimTreeExecFile',         { fg = c.keyword, gui = 'bold' })
hi('NvimTreeSymlink',          { fg = c.fn_call })
hi('NvimTreeGitDirty',         { fg = c.constant })
hi('NvimTreeGitNew',           { fg = c.keyword })
hi('NvimTreeIndentMarker',     { fg = c.punctuation })
hi('NonText',                  { fg = c.punctuation })
hi('EndOfBuffer',              { fg = c.punctuation })

-- ── Markdown ────────────────────────────────────────────────────────
hi('@markup.heading',   { fg = c.fn_decl, gui = 'bold' })
hi('@markup.heading.1', { fg = c.fn_decl, gui = 'bold' })
hi('@markup.heading.2', { fg = c.fn_decl, gui = 'bold' })
hi('@markup.heading.3', { fg = c.fn_decl, gui = 'bold' })
hi('@markup.heading.4', { fg = c.fn_decl, gui = 'bold' })
hi('@markup.heading.5', { fg = c.fn_decl, gui = 'bold' })
hi('@markup.heading.6', { fg = c.fn_decl, gui = 'bold' })
hi('@markup.italic',    { gui = 'italic' })
hi('@markup.bold',      { gui = 'bold' })
hi('@markup.raw',       { fg = c.string })
hi('@markup.raw.block', { fg = c.string })
hi('@markup.link',      { fg = c.type, gui = 'underline' })
hi('@markup.list',              { fg = c.keyword })
hi('@markup.list.checked',      { fg = c.keyword })
hi('@markup.list.unchecked',    { fg = c.keyword })

-- ── JSON ────────────────────────────────────────────────────────────
hi('jsonKey',     { fg = c.type })
hi('jsonString',  { fg = c.string })
hi('jsonNumber',  { fg = c.number })
hi('jsonBoolean', { fg = c.constant })
hi('jsonNull',    { fg = c.keyword })
hi('jsonQuote',   { fg = c.punctuation })

-- ── Markdown (Vim syntax) ───────────────────────────────────────────
hi('markdownH1',     { fg = c.fn_decl, gui = 'bold' })
hi('markdownH2',     { fg = c.fn_decl, gui = 'bold' })
hi('markdownH3',     { fg = c.fn_decl, gui = 'bold' })
hi('markdownBold',   { fg = c.keyword, gui = 'bold' })
hi('markdownItalic', { gui = 'italic' })
hi('markdownCode',   { fg = c.string, bg = c.bg_light })
hi('markdownLink',   { fg = c.type })

-- ── Kotlin ──────────────────────────────────────────────────────────
hi('@keyword.kotlin',          { fg = c.keyword })
hi('@lsp.type.keyword.kotlin', { fg = c.keyword })
hi('@lsp.keyword.kotlin',      { fg = c.keyword })
hi('@type.kotlin',             { fg = c.type })
hi('@function.kotlin',         { fg = c.fn_decl })
hi('@property.kotlin',         { fg = c.property })
hi('@variable.kotlin',         { fg = c.variable })
