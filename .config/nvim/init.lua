– init.lua - Emacs-like Neovim configuration for macOS
– Place this file at ~/.config/nvim/init.lua

– Detect macOS
local is_mac = vim.fn.has(“macunix”) == 1

if not is_mac then
vim.notify(“This configuration is optimized for macOS”, vim.log.levels.WARN)
end

– Basic settings
vim.opt.compatible = false
vim.opt.number = true
vim.opt.relativenumber = false
vim.opt.mouse = ‘a’  – Enable mouse support
vim.opt.clipboard = ‘unnamedplus’  – Use system clipboard
vim.opt.wrap = true
vim.opt.linebreak = true
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.incsearch = true
vim.opt.hlsearch = true

– Start in insert mode (Emacs-like behavior)
vim.cmd([[autocmd VimEnter * startinsert]])
vim.cmd([[autocmd BufEnter * if &buftype != ‘terminal’ | startinsert | endif]])

– Configure Alt/Option key as Meta (for macOS Terminal/iTerm2)
– You may need to configure your terminal to send proper escape sequences
vim.g.mapleader = “ “

– Telescope setup for ido-like completion
– First, ensure Telescope is installed via a plugin manager
– Using lazy.nvim as an example (install separately)

– Bootstrap lazy.nvim if not installed
local lazypath = vim.fn.stdpath(“data”) .. “/lazy/lazy.nvim”
if not vim.loop.fs_stat(lazypath) then
vim.fn.system({
“git”,
“clone”,
“–filter=blob:none”,
“https://github.com/folke/lazy.nvim.git”,
“–branch=stable”,
lazypath,
})
end
vim.opt.rtp:prepend(lazypath)

– Plugin setup
require(“lazy”).setup({
{
‘nvim-telescope/telescope.nvim’,
dependencies = { ‘nvim-lua/plenary.nvim’ },
config = function()
require(‘telescope’).setup{
defaults = {
layout_strategy = ‘horizontal’,
layout_config = {
horizontal = {
preview_width = 0.55,
},
},
},
}
end
},
{
‘nvim-telescope/telescope-fzf-native.nvim’,
build = ‘make’
},
})

– Helper function to stay in insert mode after command
local function stay_insert(func)
return function()
func()
vim.cmd(‘startinsert’)
end
end

– ============================================================================
– EMACS-STYLE KEY BINDINGS
– ============================================================================

– C-f: Find file (like C-x C-f in Emacs)
vim.keymap.set(‘i’, ‘<C-f>’, ‘<Esc>:Telescope find_files<CR>’, { noremap = true, silent = true })
vim.keymap.set(‘n’, ‘<C-f>’, ‘:Telescope find_files<CR>’, { noremap = true, silent = true })

– C-x: Prefix key (we’ll map specific combinations)
– C-x C-c: Quit (exit nvim)
vim.keymap.set(‘i’, ‘<C-x><C-c>’, ‘<Esc>:qa<CR>’, { noremap = true, silent = true })
vim.keymap.set(‘n’, ‘<C-x><C-c>’, ‘:qa<CR>’, { noremap = true, silent = true })

– C-x C-s: Save file
vim.keymap.set(‘i’, ‘<C-x><C-s>’, ‘<Esc>:w<CR>a’, { noremap = true, silent = true })
vim.keymap.set(‘n’, ‘<C-x><C-s>’, ‘:w<CR>’, { noremap = true, silent = true })

– C-b: Switch buffers
vim.keymap.set(‘i’, ‘<C-b>’, ‘<Esc>:Telescope buffers<CR>’, { noremap = true, silent = true })
vim.keymap.set(‘n’, ‘<C-b>’, ‘:Telescope buffers<CR>’, { noremap = true, silent = true })

– C-x 2: Split window horizontally
vim.keymap.set(‘i’, ‘<C-x>2’, ‘<Esc>:split<CR>a’, { noremap = true, silent = true })
vim.keymap.set(‘n’, ‘<C-x>2’, ‘:split<CR>’, { noremap = true, silent = true })

– C-x 3: Split window vertically
vim.keymap.set(‘i’, ‘<C-x>3’, ‘<Esc>:vsplit<CR>a’, { noremap = true, silent = true })
vim.keymap.set(‘n’, ‘<C-x>3’, ‘:vsplit<CR>’, { noremap = true, silent = true })

– C-x 1: Close other windows (only keep current)
vim.keymap.set(‘i’, ‘<C-x>1’, ‘<Esc>:only<CR>a’, { noremap = true, silent = true })
vim.keymap.set(‘n’, ‘<C-x>1’, ‘:only<CR>’, { noremap = true, silent = true })

– C-x 0: Close current window
vim.keymap.set(‘i’, ‘<C-x>0’, ‘<Esc>:q<CR>’, { noremap = true, silent = true })
vim.keymap.set(‘n’, ‘<C-x>0’, ‘:q<CR>’, { noremap = true, silent = true })

– C-x o: Switch to other window
vim.keymap.set(‘i’, ‘<C-x>o’, ‘<Esc><C-w>wa’, { noremap = true, silent = true })
vim.keymap.set(‘n’, ‘<C-x>o’, ‘<C-w>w’, { noremap = true, silent = true })

– ============================================================================
– EMACS NAVIGATION (works in insert mode)
– ============================================================================

– C-p: Previous line
vim.keymap.set(‘i’, ‘<C-p>’, ‘<Up>’, { noremap = true })
– C-n: Next line
vim.keymap.set(‘i’, ‘<C-n>’, ‘<Down>’, { noremap = true })
– C-b: Backward character (conflicts with buffer, using M-b for word)
vim.keymap.set(‘i’, ‘<C-h>’, ‘<Left>’, { noremap = true })  – C-h as alternative
– C-f: Forward character (conflicts with find file, handled separately)
vim.keymap.set(‘i’, ‘<C-l>’, ‘<Right>’, { noremap = true })  – C-l as alternative

– M-f (Alt-f): Forward word
vim.keymap.set(‘i’, ‘<M-f>’, ‘<Esc>ea’, { noremap = true })
– M-b (Alt-b): Backward word
vim.keymap.set(‘i’, ‘<M-b>’, ‘<Esc>bi’, { noremap = true })

– C-a: Beginning of line
vim.keymap.set(‘i’, ‘<C-a>’, ‘<Esc>I’, { noremap = true })
– C-e: End of line
vim.keymap.set(‘i’, ‘<C-e>’, ‘<Esc>A’, { noremap = true })

– C-k: Kill line (delete to end of line)
vim.keymap.set(‘i’, ‘<C-k>’, ‘<Esc>lDA’, { noremap = true })

– C-d: Delete character forward
vim.keymap.set(‘i’, ‘<C-d>’, ‘<Del>’, { noremap = true })

– C-w: Kill word backward
vim.keymap.set(‘i’, ‘<C-w>’, ‘<C-w>’, { noremap = true })

– M-d (Alt-d): Kill word forward
vim.keymap.set(‘i’, ‘<M-d>’, ‘<Esc>ldea’, { noremap = true })

– C-y: Yank (paste)
vim.keymap.set(‘i’, ‘<C-y>’, ‘<Esc>pa’, { noremap = true })

– ============================================================================
– MAC-STYLE KEY BINDINGS (CMD key mappings)
– ============================================================================

– Cmd-c: Copy (in visual mode)
vim.keymap.set(‘v’, ‘<D-c>’, ‘”+y’, { noremap = true })
– Cmd-x: Cut (in visual mode)
vim.keymap.set(‘v’, ‘<D-x>’, ‘”+d’, { noremap = true })
– Cmd-v: Paste
vim.keymap.set(‘i’, ‘<D-v>’, ‘<C-r>+’, { noremap = true })
vim.keymap.set(‘n’, ‘<D-v>’, ‘”+p’, { noremap = true })
vim.keymap.set(‘v’, ‘<D-v>’, ‘”+p’, { noremap = true })

– Shift + Arrow keys for selection in visual mode
– Enter visual mode with shift-arrow
vim.keymap.set(‘i’, ‘<S-Up>’, ‘<Esc>vk’, { noremap = true })
vim.keymap.set(‘i’, ‘<S-Down>’, ‘<Esc>vj’, { noremap = true })
vim.keymap.set(‘i’, ‘<S-Left>’, ‘<Esc>vh’, { noremap = true })
vim.keymap.set(‘i’, ‘<S-Right>’, ‘<Esc>vl’, { noremap = true })

– Continue selection in visual mode
vim.keymap.set(‘v’, ‘<S-Up>’, ‘k’, { noremap = true })
vim.keymap.set(‘v’, ‘<S-Down>’, ‘j’, { noremap = true })
vim.keymap.set(‘v’, ‘<S-Left>’, ‘h’, { noremap = true })
vim.keymap.set(‘v’, ‘<S-Right>’, ‘l’, { noremap = true })

– Cmd-a: Select all
vim.keymap.set(‘n’, ‘<D-a>’, ‘ggVG’, { noremap = true })
vim.keymap.set(‘i’, ‘<D-a>’, ‘<Esc>ggVG’, { noremap = true })

– ============================================================================
– SEARCH AND REPLACE
– ============================================================================

– C-s: Search forward (incremental search)
vim.keymap.set(‘i’, ‘<C-s>’, ‘<Esc>/’, { noremap = true })
vim.keymap.set(‘n’, ‘<C-s>’, ‘/’, { noremap = true })

– C-r: Search backward
vim.keymap.set(‘i’, ‘<C-r>’, ‘<Esc>?’, { noremap = true })
vim.keymap.set(‘n’, ‘<C-r>’, ‘?’, { noremap = true })

– M-%: Query replace (using Telescope for search)
vim.keymap.set(‘n’, ‘<M-%>’, ‘:%s/’, { noremap = true })

– ============================================================================
– ADDITIONAL UTILITIES
– ============================================================================

– C-g: Cancel/escape (like Emacs)
vim.keymap.set(‘i’, ‘<C-g>’, ‘<Esc>’, { noremap = true })
vim.keymap.set(‘n’, ‘<C-g>’, ‘<Esc>’, { noremap = true })
vim.keymap.set(‘v’, ‘<C-g>’, ‘<Esc>’, { noremap = true })

– C-x C-b: List buffers
vim.keymap.set(‘i’, ‘<C-x><C-b>’, ‘<Esc>:Telescope buffers<CR>’, { noremap = true, silent = true })
vim.keymap.set(‘n’, ‘<C-x><C-b>’, ‘:Telescope buffers<CR>’, { noremap = true, silent = true })

– M-x: Command palette (like Emacs M-x)
vim.keymap.set(‘i’, ‘<M-x>’, ‘<Esc>:Telescope commands<CR>’, { noremap = true, silent = true })
vim.keymap.set(‘n’, ‘<M-x>’, ‘:Telescope commands<CR>’, { noremap = true, silent = true })

– ============================================================================
– ADDITIONAL SETTINGS
– ============================================================================

– Disable arrow keys in normal mode to encourage Emacs-style navigation
– (optional - comment out if you want to keep arrow keys)
– vim.keymap.set(‘n’, ‘<Up>’, ‘<Nop>’, { noremap = true })
– vim.keymap.set(‘n’, ‘<Down>’, ‘<Nop>’, { noremap = true })
– vim.keymap.set(‘n’, ‘<Left>’, ‘<Nop>’, { noremap = true })
– vim.keymap.set(‘n’, ‘<Right>’, ‘<Nop>’, { noremap = true })

– Auto-save on focus lost
vim.cmd([[autocmd FocusLost * silent! wa]])

– Restore cursor position when opening files
vim.cmd([[
autocmd BufReadPost *
\ if line(”’"”) > 1 && line(”’"”) <= line(”$”) |
\   exe “normal! g`"” |
\ endif
]])

– Status line
vim.opt.laststatus = 2
vim.opt.statusline = ‘%f %h%m%r%=%l,%c %P’

print(“Emacs-like Neovim configuration loaded for macOS”)