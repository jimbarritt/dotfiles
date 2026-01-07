local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

return {
  -- Interface
  s("interface", {
    t("interface "),
    i(1, "Name"),
    t({" {", "  "}),
    i(2, "property"),
    t(": "),
    i(3, "Type"),
    t({"", "}"}),
    i(0),
  }),
  
  -- Function
  s("fun", {
    t("function "),
    i(1, "name"),
    t("("),
    i(2, "params"),
    t("): "),
    i(3, "void"),
    t({" {", "  "}),
    i(0),
    t({"", "}"}),
  }),
  
  -- Test function
  s("test", {
    t("test('"),
    i(1, "description"),
    t("', () => {"),
    t({"", "  "}),
    i(0),
    t({"", "})"}),
  }),
  
  -- For loop
  s("for", {
    t("for (const "),
    i(1, "item"),
    t(" of "),
    i(2, "collection"),
    t({") {", "  "}),
    i(0),
    t({"", "}"}),
  }),
  
  -- Switch statement
  s("switch", {
    t("switch ("),
    i(1, "value"),
    t({") {", "  case "}),
    i(2, "condition"),
    t({":", "    "}),
    i(3, "action"),
    t({"", "    break"}),
    t({"", "  default:", "    "}),
    i(0),
    t({"", "}"}),
  }),
}
