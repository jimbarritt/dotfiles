return {
  "neovim/nvim-lspconfig",
  dependencies = {
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
    "hrsh7th/cmp-nvim-lsp",
  },
  config = function()
    local capabilities = require("cmp_nvim_lsp").default_capabilities()

    -- Shared on_attach function with keybindings
    local on_attach = function(client, bufnr)
      local bufopts = { noremap = true, silent = true, buffer = bufnr }

      -- Navigation
      vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
      vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
      vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
      vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)

      -- Documentation
      vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
      vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)

      -- Code actions
      vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, bufopts)
      vim.keymap.set('n', '<S-F6>', vim.lsp.buf.rename, bufopts)  -- IntelliJ-style
      vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, bufopts)
      vim.keymap.set('n', '<leader>f', function() vim.lsp.buf.format { async = true } end, bufopts)

      -- Diagnostics
      vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, bufopts)
      vim.keymap.set('n', ']d', vim.diagnostic.goto_next, bufopts)
      vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, bufopts)
    end

    -- TypeScript/JavaScript
    vim.lsp.config('ts_ls', {
      capabilities = capabilities,
      on_attach = on_attach,
    })

    -- HTML
    vim.lsp.config('html', {
      capabilities = capabilities,
      on_attach = on_attach,
    })

    -- Rust
    vim.lsp.config('rust_analyzer', {
      capabilities = capabilities,
      on_attach = on_attach,
    })

    -- C/C++
    vim.lsp.config('clangd', {
      capabilities = capabilities,
      on_attach = on_attach,
    })

    -- Python
    vim.lsp.config('pyright', {
      capabilities = capabilities,
      on_attach = on_attach,
    })

    -- Java
    vim.lsp.config('jdtls', {
      capabilities = capabilities,
      on_attach = on_attach,
    })

    -- Kotlin (minimal config)
    vim.lsp.config('kotlin_language_server', {
      capabilities = capabilities,
      on_attach = on_attach,
    })

    -- Bash / Zsh
    vim.lsp.config('bashls', {
      capabilities = capabilities,
      on_attach = on_attach,
    })

    vim.api.nvim_create_autocmd("FileType", {
      pattern = "kotlin",
      callback = function() vim.lsp.enable('kotlin_language_server') end,
    })
       
    -- Enable LSP servers on appropriate file types
    vim.api.nvim_create_autocmd("FileType", {
      pattern = {"typescript", "javascript", "typescriptreact", "javascriptreact"},
      callback = function() vim.lsp.enable('ts_ls') end,
    })

    vim.api.nvim_create_autocmd("FileType", {
      pattern = "html",
      callback = function() vim.lsp.enable('html') end,
    })

    vim.api.nvim_create_autocmd("FileType", {
      pattern = "rust",
      callback = function() vim.lsp.enable('rust_analyzer') end,
    })

    vim.api.nvim_create_autocmd("FileType", {
      pattern = {"c", "cpp"},
      callback = function() vim.lsp.enable('clangd') end,
    })

    vim.api.nvim_create_autocmd("FileType", {
      pattern = "python",
      callback = function() vim.lsp.enable('pyright') end,
    })

    vim.api.nvim_create_autocmd("FileType", {
      pattern = "java",
      callback = function() vim.lsp.enable('jdtls') end,
    })

    vim.api.nvim_create_autocmd("FileType", {
      pattern = "kotlin",
      callback = function() vim.lsp.enable('kotlin_language_server') end,
    })
  end,
}
