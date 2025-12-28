-- Telescope configuration 
return {
  {
    'nvim-telescope/telescope.nvim',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
      require('telescope').setup{
        defaults = {
          layout_strategy = 'horizontal',
          layout_config = {
            horizontal = {
              preview_width = 0.5,
              width = 0.9,
              height = 0.8,
            },
          },
          mappings = {
            i = {  -- Insert mode
              ["<C-g>"] = "close",
            },
            n = {  -- Normal mode
              ["<C-g>"] = "close",
            },
          },
        },
      }
      -- Load fzf extension
      pcall(require('telescope').load_extension, 'fzf')
    end
  },
  {
    'nvim-telescope/telescope-fzf-native.nvim',
    build = 'make'
  }
}
