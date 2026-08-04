-- Prose files wrap; code files do not (see options.lua's global wrap = false).
-- This lives in after/ftplugin rather than a FileType autocmd in autocmds.lua
-- because the runtime's ftplugin mechanism is registered before init.lua runs.
-- A FileType autocmd created in autocmds.lua is registered after config.lazy,
-- by which point the event has already fired for a file named on the command
-- line. See doc/nvim-lsp-filetype-race.md for the same race in LSP attach.
vim.opt_local.wrap = true
vim.opt_local.linebreak = true
