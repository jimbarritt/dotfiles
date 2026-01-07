vim.api.nvim_create_autocmd("FocusLost", {
  pattern = "*",
  command = "silent! wa",
})

-- Restore cursor position when opening files within a session
vim.api.nvim_create_autocmd("BufReadPost", {
  pattern = "*",
  callback = function()
    -- Skip certain filetypes (git commits, etc.)
    local ft = vim.bo.filetype
    if vim.tbl_contains({ "gitcommit", "gitrebase", "svn", "mail" }, ft) then
      return
    end

    -- Skip special buffer types
    if vim.bo.buftype ~= "" then
      return
    end

    -- Defer to ensure buffer is fully initialized
    vim.defer_fn(function()
      local mark = vim.api.nvim_buf_get_mark(0, '"')
      local lcount = vim.api.nvim_buf_line_count(0)
      if mark[1] > 0 and mark[1] <= lcount then
        pcall(vim.api.nvim_win_set_cursor, 0, mark)
      end
    end, 0)
  end,
})

-- Restore cursor position when launching nvim from command line
vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    -- Force read ShaDa to load marks BEFORE trying to restore
    vim.cmd("silent! rshada!")

    -- Only for regular files, not special buffers
    if vim.bo.buftype == "" then
      vim.defer_fn(function()
        local mark = vim.api.nvim_buf_get_mark(0, '"')
        local lcount = vim.api.nvim_buf_line_count(0)
        if mark[1] > 0 and mark[1] <= lcount then
          pcall(vim.api.nvim_win_set_cursor, 0, mark)
        end
      end, 0)
    end
  end,
})
