-- Auto-save on focus lost
vim.cmd([[autocmd FocusLost * silent! wa]])

-- Restore cursor position when opening files
vim.cmd([[
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif
]])

require("nvim-tree").setup({
  sort = {
    sorter = "case_sensitive",
  },
  view = {
    width = 30,
  },
  renderer = {
    group_empty = true,
    root_folder_label = ":~:s?$?/..?",
  },
  filters = {
    dotfiles = false,
  },
  -- Don't change root when navigating files
  update_focused_file = {
    enable = true,
    update_root = false,  -- Changed to false - keeps root fixed
  },
  sync_root_with_cwd = false,  -- Changed to false
  respect_buf_cwd = false,     -- Changed to false
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = {"java", "kotlin"},
  callback = function()
    vim.bo.shiftwidth = 4
    vim.bo.tabstop = 4
    vim.bo.softtabstop = 4
  end,
})
