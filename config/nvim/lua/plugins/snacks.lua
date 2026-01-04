return {
  {
    "folke/snacks.nvim",
    opts = {
      indent = { enabled = false },  -- Disable indent guides
      scroll = { enabled = false },  -- Also disable smooth scrolling if annoying
      notifier = { enabled = false },  -- Disable notifications
      bigfile = { enabled = true },
      quickfile = { enabled = true },
      statuscolumn = { enabled = false },
      words = { enabled = false },
      styles = {},
      -- Disable yank highlight
      animate = { enabled = false },
    },
  },
}
