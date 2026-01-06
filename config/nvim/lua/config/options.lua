-- Detect macOS
local is_mac = vim.fn.has("macunix") == 1

if not is_mac then
  vim.notify("This configuration is optimized for macOS", vim.log.levels.WARN)
end


-- Leader key
vim.g.mapleader = " "

-- vim.cmd("syntax off")
vim.cmd("syntax on")

-- UI settings
vim.opt.compatible = false
-- Cursor shapes: solid block in normal, blinking block in insert
vim.opt.guicursor = "n-v-c:block,i-ci-ve:block-blinkon100-blinkoff100-blinkwait100"
vim.opt.cursorline = true  -- Highlight current line
vim.opt.number = true
-- vim.opt.numberwidth = 6
vim.opt.relativenumber = true
vim.opt.mouse = 'a'
-- Always show sign column (prevents layout shift)
vim.opt.signcolumn = "yes"
vim.opt.wrap = false
vim.opt.linebreak = true

vim.opt.ttyfast = true
vim.opt.redrawtime = 1500
vim.opt.lazyredraw = true

-- Clipboard
vim.opt.clipboard = 'unnamedplus'

-- Splits
vim.opt.splitright = true
vim.opt.splitbelow = true

-- Search
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.incsearch = true
vim.opt.hlsearch = true

-- Spaces and tabs
vim.opt.tabstop = 2        -- Tab displays as 2 spaces
vim.opt.shiftwidth = 2     -- Indent with 2 spaces
vim.opt.softtabstop = 2    -- Tab key inserts 2 spaces
vim.opt.expandtab = true   -- Use spaces, not tabs
vim.opt.smartindent = true -- Smart auto-indenting
vim.api.nvim_create_autocmd("FileType", {
  pattern = "make",
  callback = function()
    vim.opt_local.expandtab = false  -- Use real tabs in Makefiles
  end,
})
-- Status line
vim.opt.laststatus = 2
vim.opt.showmode = false  -- Disable mode in command line

-- Custom highlight groups for statusline
vim.api.nvim_set_hl(0, 'StatusLineMode', { fg = '#000000', bg = '#88c0d0', bold = true })
vim.api.nvim_set_hl(0, 'StatusLineNormal', { fg = '#d8dee9', bg = '#3b4252' })

-- Custom statuscolumn with git signs and folds
local function get_statuscolumn()
  local col = table.concat({
    "%s",  -- Signs (gitsigns will display here)
    "%=",  -- Right align everything after this
    "%l",  -- Line number
    "%=",  -- Right align fold indicators
    "%{foldclosed(v:lnum) >= 0 ? ' ›' : ' '}",  -- Fold indicator only when fold is closed
    " ",   -- Spacing
  })
  -- Wrap with clickable region for fold toggling
  return "%@v:lua.require'config.statuscolumn'.click_fold@" .. col .. "%T"
end

vim.opt.statuscolumn = get_statuscolumn()

-- Create statuscolumn module for click handling
local statuscolumn = {}
function statuscolumn.click_fold()
  -- Get current window from global context (before getmousepos might affect it)
  local cur_win = vim.api.nvim_get_current_win()
  local cursor = vim.api.nvim_win_get_cursor(cur_win)

  local pos = vim.fn.getmousepos()
  if vim.fn.foldlevel(pos.line) > 0 then
    vim.api.nvim_win_call(pos.winid, function()
      -- Toggle fold on the clicked line
      vim.fn.execute(pos.line .. "normal! za", "silent")
    end)

    -- Restore cursor position AFTER fold toggle
    vim.api.nvim_win_set_cursor(cur_win, cursor)
  end
end

package.loaded['config.statuscolumn'] = statuscolumn

vim.schedule(function()
  vim.notify = function(msg, level, opts)
    -- Silent - do nothing, no popups ever
  end
end)
-- Set colorscheme
vim.cmd('colorscheme green-tinted')
