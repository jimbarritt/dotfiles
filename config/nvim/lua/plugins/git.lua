return {
  {
    "lewis6991/gitsigns.nvim",
    opts = {
      signs = {
        add          = { text = '▎' },
        change       = { text = '▎' },
        delete       = { text = '▎' },
        topdelete    = { text = '▎' },
        changedelete = { text = '▎' },
      },
      sign_priority = 6,
      numhl = false,
      linehl = false,
      attach_to_untracked = false,
    },
  },
}
