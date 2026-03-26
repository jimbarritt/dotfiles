return {
  'projekt0n/github-nvim-theme',
  name = 'github-theme',
  lazy = true,
  config = function()
    require('github-theme').setup({})
  end,
}
