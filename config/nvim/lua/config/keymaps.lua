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

-- Indent and stay in visual mode without the "top of block" jump
vim.keymap.set("v", ">", ":<C-u>normal! >gv<CR>", { silent = true })
vim.keymap.set("v", "<", ":<C-u>normal! <gv<CR>", { silent = true })

-- Telescope keybindings
local builtin = require('telescope.builtin')

local function get_git_root()
  local git_root = vim.fn.system('git rev-parse --show-toplevel 2> /dev/null')
  if vim.v.shell_error == 0 then
    return vim.trim(git_root)
  else
    return vim.fn.getcwd()
  end
end

vim.keymap.set('n', '<leader>ff', function()
  builtin.find_files({ cwd = get_git_root() })
end, { desc = 'Find files (git root or cwd)' })

vim.keymap.set('n', '<leader>fg', function()
  builtin.live_grep({ cwd = get_git_root() })
end, { desc = 'Live grep (git root or cwd)' })

vim.keymap.set('n', '<leader>fe', ':NvimTreeToggle<CR>', { desc = 'Toggle file explorer', silent = true })

vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Find buffers' })
vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'Find help' })
vim.keymap.set('n', '<leader>fo', builtin.oldfiles, { desc = 'Find old files' })

-- Ctrl+G to exit mode (like Emacs cancel)
vim.keymap.set('i', '<C-g>', '<Esc>', { noremap = true, silent = true })
vim.keymap.set('v', '<C-g>', '<Esc>', { noremap = true, silent = true })
vim.keymap.set('c', '<C-g>', '<C-c>', { noremap = true })

-- Insert blank line below (without entering insert mode)
vim.keymap.set('n', '<Leader>o', 'o<Esc>', { noremap = true, silent = true })

-- Insert blank line above (without entering insert mode)
vim.keymap.set('n', '<Leader>O', 'O<Esc>', { noremap = true, silent = true })

-- Clear search highlights with double Esc
vim.keymap.set('n', '<Esc><Esc>', ':nohlsearch<CR>', { noremap = true, silent = true, desc = "Clear search highlights" })
vim.keymap.set('n', '<leader>/', ':nohlsearch<CR>', { noremap = true, silent = true, desc = "Clear search highlights" })

-- Toggle nvim-tree
vim.keymap.set('n', '<leader>n', ':NvimTreeToggle<CR>', { noremap = true, silent = true, desc = "Toggle file tree" })
