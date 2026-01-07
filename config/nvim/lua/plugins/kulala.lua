return {
  "mistweaverco/kulala.nvim",
  ft = { "http" },
  keys = {
    { "<leader>r", "", desc = "+rest" },
    { "<leader>rr", "<cmd>lua require('kulala').run()<cr>", desc = "Run request" },
    { "<leader>rt", "<cmd>lua require('kulala').toggle_view()<cr>", desc = "Toggle view" },
    { "<leader>rp", "<cmd>lua require('kulala').copy()<cr>", desc = "Copy as curl" },
    { "<leader>ri", "<cmd>lua require('kulala').inspect()<cr>", desc = "Inspect request" },
    { "<leader>re", "<cmd>lua require('kulala').set_selected_env()<cr>", desc = "Select environment" },
  },
  opts = {
    default_view = "body",
    default_env = "dev",
    debug = false,
    formatters = {
      json = { "jq", "." },
      xml = { "xmllint", "--format", "-" },
      html = { "xmllint", "--format", "--html", "-" },
    },
  },
}
