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

-- Indent and stay in visual mode
vim.keymap.set("v", ">", ">gv", { noremap = true, silent = true })
vim.keymap.set("v", "<", "<gv", { noremap = true, silent = true })

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
vim.keymap.set("n", "<C-g>", function()
  vim.cmd("nohlsearch")

  -- Get values with explicit fallbacks
  local file = vim.fn.expand('%:~') or ""
  local line = vim.fn.line('.')
  local total = vim.fn.line('$')
  local col = vim.fn.col('.')
  
  -- Convert to numbers, ensuring we never have nil
  line = type(line) == "number" and line or 0
  total = type(total) == "number" and total or 0  
  col = type(col) == "number" and col or 0
  
  local percent = total > 0 and math.floor(line/total * 100) or 0
  
  local is_exec = vim.fn.executable(file) == 1
  local status = is_exec and "[executable] " or "" 

  print(string.format('"%s" %s(%d lines)', 
    file, status, total ))

end, { desc = "Clear highlights and show file info" })

-- Insert mode navigation with Ctrl+hjkl
vim.keymap.set('i', '<C-h>', '<Left>', { noremap = true, silent = true })
vim.keymap.set('i', '<C-j>', '<Down>', { noremap = true, silent = true })
vim.keymap.set('i', '<C-k>', '<Up>', { noremap = true, silent = true })
vim.keymap.set('i', '<C-l>', '<Right>', { noremap = true, silent = true })

-- Emacs-style line navigation in insert mode
vim.keymap.set('i', '<C-a>', '<C-o>^', { noremap = true, silent = true, desc = 'Start of text' })
vim.keymap.set('i', '<C-e>', '<End>', { noremap = true, silent = true, desc = 'End of line' })

-- Insert blank line below (without entering insert mode)
vim.keymap.set('n', '<Leader>o', 'o<Esc>', { noremap = true, silent = true })

-- Insert blank line above (without entering insert mode)
vim.keymap.set('n', '<Leader>O', 'O<Esc>', { noremap = true, silent = true })

-- Clear search highlights with double Esc
vim.keymap.set('n', '<Esc><Esc>', ':nohlsearch<CR>', { noremap = true, silent = true, desc = "Clear search highlights" })
vim.keymap.set('n', '<leader>/', ':nohlsearch<CR>', { noremap = true, silent = true, desc = "Clear search highlights" })

-- Toggle nvim-tree
vim.keymap.set('n', '<leader>n', ':NvimTreeToggle<CR>', { noremap = true, silent = true, desc = "Toggle file tree" })

-- LSP bindings
vim.keymap.set("n", "grn", vim.lsp.buf.rename, { desc = "LSP Rename" })
vim.keymap.set("n", "<S-F6>", vim.lsp.buf.rename, { desc = "LSP Rename" })
