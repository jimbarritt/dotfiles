return {
  "neovim/nvim-lspconfig",
  dependencies = {
    "mason-org/mason.nvim",
    "mason-org/mason-lspconfig.nvim",
  },
  config = function()
    -- Initialize Mason FIRST
    require("mason").setup()
    require("mason-lspconfig").setup({
      ensure_installed = {
        "lua_ls",
        "ts_ls",
        "html",
        "rust_analyzer",
        "clangd",
        "pyright",
        "jdtls",
        "kotlin_language_server",
        "bashls",
      },
    })
    -- Configure diagnostics display
    vim.diagnostic.config({
      virtual_text = true,       -- Show diagnostics as virtual text
      signs = true,              -- Show signs in gutter
      underline = true,          -- Underline problematic code
      update_in_insert = false,  -- Don't update while typing
      severity_sort = true,      -- Sort by severity
      float = {
        border = "rounded",
        source = true,
      },
    })

    -- Define diagnostic signs
    local signs = { Error = "X", Warn = "W", Hint = "?", Info = "I" }
    for type, icon in pairs(signs) do
      local hl = "DiagnosticSign" .. type
      vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
    end

    local capabilities = vim.lsp.protocol.make_client_capabilities()
    -- Add blink.cmp capabilities if available
    local ok, blink = pcall(require, "blink.cmp")
    if ok then
      capabilities = blink.get_lsp_capabilities(capabilities)
    end

    -- Shared on_attach function with keybindings
    local on_attach = function(_, bufnr)
      local bufopts = { noremap = true, silent = true, buffer = bufnr }
      local opts = function(desc)
        return { buffer = bufnr, desc = desc }
      end

      -- Force show diagnostics when LSP attaches
      vim.defer_fn(function()
        vim.diagnostic.show(nil, bufnr)
      end, 100)

      -- Diagnostics
      vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, opts("Show line diagnostics"))
      vim.keymap.set("n", "]d", function() vim.diagnostic.jump({ count = 1, float = true }) end, opts("Next diagnostic"))
      vim.keymap.set("n", "[d", function() vim.diagnostic.jump({ count = -1, float = true }) end, opts("Previous diagnostic"))
      vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, opts("All diagnostics"))

      -- LSP Actions
      vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts("Go to definition"))
      vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts("Go to declaration"))
      vim.keymap.set("n", "gr", vim.lsp.buf.references, opts("Show references"))
      vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts("Go to implementation"))
      vim.keymap.set("n", "K", vim.lsp.buf.hover, opts("Hover documentation"))
      vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts("Rename symbol"))
      vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts("Code action"))
      vim.keymap.set('n', '<S-F6>', vim.lsp.buf.rename, opts("Rename symbol - Intellij Style"))  

      -- Documentation
      vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
      vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)

      vim.keymap.set('n', '<leader>f', function() vim.lsp.buf.format { async = true } end, bufopts)
    end

    -- Lua
    vim.lsp.config('lua_ls', {
      capabilities = capabilities,
      on_attach = on_attach,
      settings = {
        Lua = {
          diagnostics = {
            globals = { "vim" },
            disable = { "trailing-space", "trailing-newline", "redundant-value" },
          },
          workspace = {
            library = vim.api.nvim_get_runtime_file("", true),
            checkThirdParty = false,
          },
          telemetry = { enable = false },
          locale = "en-GB",
        },
      },
    })

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

    -- Kotlin
    vim.lsp.config('kotlin_language_server', {
      capabilities = capabilities,
      on_attach = on_attach,
    })

    -- Bash / Zsh
    vim.lsp.config('bashls', {
      capabilities = capabilities,
      on_attach = on_attach,
    })

    -- Enable LSP servers on appropriate file types
    vim.api.nvim_create_autocmd("FileType", {
      pattern = {"typescript", "javascript", "typescriptreact", "javascriptreact"},
      callback = function() vim.lsp.enable('ts_ls') end,
    })

    vim.api.nvim_create_autocmd("FileType", {
      pattern = "lua",
      callback = function() vim.lsp.enable('lua_ls') end,
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
