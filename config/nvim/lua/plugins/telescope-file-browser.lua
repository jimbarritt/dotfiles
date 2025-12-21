return {
  'nvim-telescope/telescope-file-browser.nvim',
  dependencies = { 'nvim-telescope/telescope.nvim', 'nvim-lua/plenary.nvim' },
  config = function()
    require('telescope').load_extension('file_browser')

    -- Keybinding for file browser
    vim.keymap.set('n', '<leader>fe', ':Telescope file_browser<CR>', { desc = 'File explorer' })
    vim.keymap.set('n', '<leader>fE', ':Telescope file_browser path=%:p:h select_buffer=true<CR>', { desc = 'File explorer (current dir)' })
  end,
}
