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
            -- Ghostty rewrites ctrl+g -> Esc (see config/ghostty/config), so a
            -- <C-g> mapping never fires here. Map <Esc> -> close instead, in
            -- insert mode too, so one press closes the picker straight from the
            -- prompt rather than first dropping into Telescope's normal mode.
            i = {  -- Insert mode
              ["<Esc>"] = "close",
            },
            n = {  -- Normal mode
              ["<Esc>"] = "close",
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
