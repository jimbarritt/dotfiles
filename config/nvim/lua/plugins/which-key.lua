return {
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      preset = "modern",
      delay = 1000,
      icons = {
        breadcrumb = "»",
        separator = "➜",
        group = "+",
      },
      spec = {
        { "<leader>f", group = "Find (Telescope)" },
        { "<leader>c", group = "Code Actions" },
        { "<leader>r", group = "Rename" },
      },
    },
    config = function(_, opts)
      local wk = require("which-key")
      wk.setup(opts)
    end,
  },
}
