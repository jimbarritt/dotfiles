local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

return {
  -- Data class
  s("data", {
    t("data class "),
    i(1, "ClassName"),
    t("("),
    i(2, "val property: Type"),
    t(")"),
    i(0),
  }),
  
  -- Function
  s("fun", {
    t("fun "),
    i(1, "name"),
    t("("),
    i(2, "params"),
    t("): "),
    i(3, "Unit"),
    t({" {", "    "}),
    i(0),
    t({"", "}"}),
  }),
  
  -- Test function
  s("test", {
    t({"@Test", "fun `"}),
    i(1, "test description"),
    t("`() {"),
    t({"", "    "}),
    i(0),
    t({"", "}"}),
  }),
  
  -- For loop
  s("for", {
    t("for ("),
    i(1, "item"),
    t(" in "),
    i(2, "collection"),
    t({") {", "    "}),
    i(0),
    t({"", "}"}),
  }),
  
  -- When expression
  s("when", {
    t("when ("),
    i(1, "value"),
    t({") {", "    "}),
    i(2, "condition"),
    t(" -> "),
    i(3, "result"),
    t({"", "    else -> "}),
    i(0),
    t({"", "}"}),
  }),
}
