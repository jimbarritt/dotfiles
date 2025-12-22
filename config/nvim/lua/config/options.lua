-- Detect macOS
local is_mac = vim.fn.has("macunix") == 1

if not is_mac then
  vim.notify("This configuration is optimized for macOS", vim.log.levels.WARN)
end

-- Set colorscheme
vim.cmd('colorscheme green-tinted')

-- Leader key
vim.g.mapleader = " "

-- UI settings
vim.opt.compatible = false
-- Cursor shapes: solid block in normal, blinking block in insert
vim.opt.guicursor = "n-v-c:block,i-ci-ve:block-blinkon100-blinkoff100-blinkwait100"
vim.opt.cursorline = true  -- Highlight current line
vim.opt.number = true
-- vim.opt.numberwidth = 6
vim.opt.relativenumber = true
vim.opt.mouse = 'a'
vim.opt.wrap = false
vim.opt.linebreak = true

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

-- Status line
vim.opt.laststatus = 2
vim.opt.showmode = false  -- Disable mode in command line

-- Custom highlight groups for statusline
vim.api.nvim_set_hl(0, 'StatusLineMode', { fg = '#000000', bg = '#88c0d0', bold = true })
vim.api.nvim_set_hl(0, 'StatusLineNormal', { fg = '#d8dee9', bg = '#3b4252' })

-- Function to get current mode with color
function _G.get_mode()
  local mode_map = {
    ['n']  = 'N',
    ['i']  = 'I',
    ['v']  = 'v',
    ['V']  = 'V',
    [''] = 'B',
    ['c']  = 'C',
    ['r']  = 'R',
    ['t']  = 'T',
  }
  local mode = vim.api.nvim_get_mode().mode
  return mode_map[mode] or mode
end

-- Statusline with mode block effect
vim.opt.statusline = '%#StatusLineMode# %{%v:lua.get_mode()%} %#StatusLineNormal# %t %h%m%r%=%l,%c %P '
