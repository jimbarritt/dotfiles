-- Auto-save on focus lost
vim.cmd([[autocmd FocusLost * silent! wa]])

-- Restore cursor position when opening files
vim.cmd([[
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif
]])

vim.api.nvim_create_autocmd("FileType", {
  pattern = {"java", "kotlin"},
  callback = function()
    vim.bo.shiftwidth = 4
    vim.bo.tabstop = 4
    vim.bo.softtabstop = 4
  end,
})

-- Gitconfig filetype detection
vim.api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
  pattern = {".gitconfig", "gitconfig", "*.gitconfig"},
  callback = function()
    vim.bo.filetype = "gitconfig"
  end,
})
