-- Slot → highlight group mapping
--
-- Declarative table. No nvim API calls here — just data.
--
-- Each key is a palette slot name. Each value is a list of highlight groups
-- that should receive that slot's colour.
--
-- Entries can be:
--   "GroupName"                     → fg = slot colour, no extras
--   { "GroupName", { gui = "bold" } }  → fg = slot colour + style overrides
--
-- If a palette doesn't define a slot, the applicator falls back:
--   slot → text → fg
--
-- Prose groups (markup headings, bold, italic) are tagged with gui attributes
-- here. The applicator decides whether to honour or suppress them based on
-- the suppress_style option (see D2 in design doc).

local M = {}

-- ── Syntax slots ────────────────────────────────────────────────────

M.comment = {
  "Comment", "SpecialComment",
  "@comment", "@comment.documentation",
  "@lsp.type.comment",
}

M.keyword = {
  "Statement", "Conditional", "Repeat", "Label", "Keyword", "Exception",
  "PreProc", "Include", "Define", "Macro", "PreCondit",
  "@keyword", "@keyword.function", "@keyword.operator",
  "@keyword.import", "@keyword.repeat", "@keyword.conditional",
  "@keyword.function.lua", "@keyword.lua",
  "@keyword.kotlin",
  "@lsp.type.keyword",
  "@lsp.type.keyword.kotlin", "@lsp.keyword.kotlin",
}

-- keyword_control: return, break, continue — falls back to keyword
M.keyword_control = {
  "@keyword.return",
}

M.entity = {
  -- Function declarations + user-defined type references
  "Function",
  "@function", "@function.method",
  "@module",
  "@lsp.type.struct",            -- Kotlin LSP: user-defined types (User, App)
  "@lsp.type.namespace",         -- package names (org.example)
  "@lsp.typemod.function.declaration",
  "@lsp.typemod.function.declaration.kotlin",
  "@lsp.typemod.method.declaration",
  "@lsp.typemod.method.declaration.kotlin",
  "@lsp.typemod.class.declaration",
  "@lsp.typemod.class.declaration.kotlin",
  "@lsp.type.enum.kotlin", "@lsp.type.enumMember",
  "@lsp.type.enumMember.kotlin",
  "@lsp.typemod.enum.declaration", "@lsp.typemod.enum.declaration.kotlin",
  "@lsp.typemod.enumMember.declaration", "@lsp.typemod.enumMember.declaration.kotlin",
  "@lsp.typemod.enumMember.readonly", "@lsp.typemod.enumMember.readonly.kotlin",
  "@lsp.type.class",
  "@lsp.type.interface", "@lsp.type.interface.kotlin",
  "@lsp.typemod.interface.declaration", "@lsp.typemod.interface.declaration.kotlin",
  "@function.kotlin",
  -- Markup headings (D3: stay in entity)
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
  -- Special file indicators
  "NvimTreeRootFolder", "NvimTreeSpecialFile",
  -- Telescope match highlight
  "TelescopeMatching",
}

-- fn_call: falls back to text if palette doesn't define it (D4)
M.fn_call = {
  "@function.call", "@function.method.call",
  "@function.builtin",
  "@lsp.type.function",  -- D4: LSP function references → fn_call slot
  "@lsp.type.method",    -- method/constructor calls → black (decls stay purple via TS @function)
}

M.type = {
  "Type", "StorageClass", "Structure", "Typedef",
  "@type", "@type.builtin", "@type.definition",
  "@attribute", "@namespace",
  "@lsp.type.type", "@lsp.type.enum",
  "@type.kotlin",
  "Directory", "NvimTreeFolderName",
  "jsonBoolean",
}

M.constant = {
  "Constant", "Boolean",
  "@constant", "@constant.builtin",
  "@boolean",
  "@lsp.mod.readonly",
}

M.number = {
  "Number", "Float",
  "@number", "@float",
  "jsonNumber",
}

M.string = {
  "String", "Character",
  "@string", "@character",
  "@string.special",
  "@markup.raw", "@markup.raw.block",
  { "markdownCode", { bg = "_bg_float" } },  -- special: uses bg_float for bg
  "jsonString",
}

M.string_escape = {
  "SpecialChar",
  "@string.escape",
}

M.variable = {
  "@variable.builtin",
  "@variable.parameter", "@parameter",
  "@lsp.type.parameter",
}

M.param = {
  -- intentionally empty at mapping level; variable slot handles params
  -- by default. Green-dark palette defines param separately and the
  -- green-dark entry point can add overrides if needed.
  -- Kept as a slot so palettes can define it.
}

M.property = {
  "@variable.member", "@property",
  "@lsp.type.property",
  "@property.kotlin",
}

M.tag = {
  "@string.regexp",
  "@markup.list", "@markup.list.checked", "@markup.list.unchecked",
  { "markdownBold", { gui = "bold" } },
  "NvimTreeOpenedFolderName",
}

M.text = {
  -- Everything that should be the default fg colour
  "Identifier",
  "@variable",
  "Delimiter", "@punctuation.delimiter", "@punctuation.special",
  "Special", "Tag",
  "@lsp.type.variable",
  "@variable.kotlin",
  "jsonKey", "jsonQuote", "jsonNull",
  "NonText", "EndOfBuffer",
  "NvimTreeNormal", "NvimTreeNormalNC",
  "NvimTreeIndentMarker",
}

-- builtin: stdlib types (String) and functions (println)
-- At pri=126, @lsp.mod.defaultLibrary overrides @lsp.type.* at pri=125
-- Falls back to text if palette doesn't define it
M.builtin = {
  "@lsp.mod.defaultLibrary",
}

-- string_interpolation: ${...} blocks inside strings — falls back to text
M.string_interpolation = {
  "@string.interpolation",
}

-- constructor: uppercase call expressions (User()) — falls back to entity
M.constructor = {
  "@constructor",
}

-- operator: falls back to text if palette doesn't define it
M.operator = {
  "Operator", "@operator",
  "@lsp.type.operator",
}

-- punctuation: falls back to text if palette doesn't define it
M.punctuation = {
  -- Only used when palette defines a separate punctuation colour (green-dark)
}

-- bracket_top: falls back to text if palette doesn't define it
M.bracket_top = {
  "@punctuation.bracket",
}

-- variable_local: falls back to variable → text
M.variable_local = {
  -- Placeholder for green-dark's dimmer local variable colour
}

-- ── Prose groups (D2: exempt from style suppression) ────────────────

M._prose_groups = {
  ["@markup.heading"]   = true,
  ["@markup.heading.1"] = true,
  ["@markup.heading.2"] = true,
  ["@markup.heading.3"] = true,
  ["@markup.heading.4"] = true,
  ["@markup.heading.5"] = true,
  ["@markup.heading.6"] = true,
  ["@markup.bold"]      = true,
  ["@markup.italic"]    = true,
  ["markdownH1"]        = true,
  ["markdownH2"]        = true,
  ["markdownH3"]        = true,
  ["markdownBold"]      = true,
  ["markdownItalic"]    = true,
}

-- ── Prose-only groups (style but no slot colour) ────────────────────

M._prose_style_only = {
  { "@markup.italic",  { gui = "italic" } },
  { "@markup.bold",    { gui = "bold" } },
  { "markdownItalic",  { gui = "italic" } },
  { "@markup.link",    { gui = "underline" } },
  { "markdownLink",    { gui = "underline" } },
}

return M
