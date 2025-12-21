-- Disable arrow keys in normal mode to encourage hjkl navigation
vim.keymap.set('n', '<Up>', '<Nop>', { noremap = true })
vim.keymap.set('n', '<Down>', '<Nop>', { noremap = true })
vim.keymap.set('n', '<Left>', '<Nop>', { noremap = true })
vim.keymap.set('n', '<Right>', '<Nop>', { noremap = true })

-- Page up/down and keep cursor centered
vim.keymap.set('n', '<C-f>', '<C-f>zz', { noremap = true, desc = 'Page down and center' })
vim.keymap.set('n', '<C-b>', '<C-b>zz', { noremap = true, desc = 'Page up and center' })

-- Half page up/down and keep cursor centered
vim.keymap.set('n', '<C-d>', '<C-d>zz', { noremap = true, desc = 'Half page down and center' })
vim.keymap.set('n', '<C-u>', '<C-u>zz', { noremap = true, desc = 'Half page up and center' })

-- Telescope keybindings
local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = 'Find files' })
vim.keymap.set('n', '<C-p>', builtin.find_files, { desc = 'Find files' })
vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = 'Live grep' })
vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Find buffers' })
vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'Find help' })
vim.keymap.set('n', '<leader>fo', builtin.oldfiles, { desc = 'Find old files' })

-- Ctrl+G to exit insert mode (like Emacs cancel)
vim.keymap.set('i', '<C-g>', '<Esc>', { noremap = true, silent = true })

-- Add to your init.lua
-- Insert blank line below (without entering insert mode)
vim.keymap.set('n', '<Leader>o', 'o<Esc>', { noremap = true, silent = true })

-- Insert blank line above (without entering insert mode)
vim.keymap.set('n', '<Leader>O', 'O<Esc>', { noremap = true, silent = true })

-- Ctrl+G to exit command mode (like Emacs cancel)
vim.keymap.set('c', '<C-g>', '<C-c>', { noremap = true })

-- Toggle nvim-tree
vim.keymap.set('n', '<leader>n', ':NvimTreeToggle<CR>', { noremap = true, silent = true, desc = "Toggle file tree" })
