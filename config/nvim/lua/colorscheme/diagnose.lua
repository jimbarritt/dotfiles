-- Colorscheme diagnostic: dump highlight info for key tokens
-- Usage: :luafile ~/.config/nvim/lua/colorscheme/diagnose.lua

local bufnr = vim.api.nvim_get_current_buf()
local row = vim.api.nvim_win_get_cursor(0)[1] - 1
local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, false)[1]

local header = string.format("=== Line %d: %s ===\n", row + 1, line)
print("\n" .. header)
local output = { header }

local col = 0
while col < #line do
  if line:sub(col + 1, col + 1):match("%s") then
    col = col + 1
  else
    local start = col
    local ch = line:sub(col + 1, col + 1)
    if ch:match("[%w_]") then
      while col < #line and line:sub(col + 1, col + 1):match("[%w_.]") do
        col = col + 1
      end
    else
      col = col + 1
    end

    local token = line:sub(start + 1, col)
    local inspect = vim.inspect_pos(bufnr, row, start)

    local parts = {}

    for _, ts in ipairs(inspect.treesitter or {}) do
      table.insert(parts, string.format("  TS: @%s  pri=%d", ts.capture, ts.priority or 0))
    end

    -- Dump semantic tokens with full detail
    for _, st in ipairs(inspect.semantic_tokens or {}) do
      local hl = (st.opts and st.opts.hl_group) or "?"
      local link = (st.opts and st.opts.hl_group_link) or ""
      local pri = (st.opts and st.opts.priority) or 0
      -- Get the actual colour this resolves to
      local color = ""
      local name = link ~= "" and link or hl
      local hl_info = vim.api.nvim_get_hl(0, { name = name, link = false })
      if hl_info.fg then
        color = string.format("  fg=#%06x", hl_info.fg)
      end
      table.insert(parts, string.format("  SEM: %s → %s%s  pri=%d", hl, link, color, pri))
    end

    for _, ext in ipairs(inspect.extmarks or {}) do
      if ext.opts and ext.opts.hl_group then
        local color = ""
        local hl_info = vim.api.nvim_get_hl(0, { name = ext.opts.hl_group, link = false })
        if hl_info.fg then
          color = string.format("  fg=#%06x", hl_info.fg)
        end
        table.insert(parts, string.format("  EXT: %s%s  pri=%d", ext.opts.hl_group, color, ext.opts.priority or 0))
      end
    end

    if #parts > 0 then
      local block = string.format("'%s' (col %d):\n%s\n", token, start, table.concat(parts, "\n"))
      print(block)
      table.insert(output, block)
    end
  end
end

vim.fn.setreg("+", table.concat(output, "\n"))
print("(copied to clipboard)")
