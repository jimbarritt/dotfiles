return {
  "neovim/nvim-lspconfig",
  dependencies = {
    "mason-org/mason.nvim",
    "mason-org/mason-lspconfig.nvim",
  },
  config = function()
    require("mason").setup()
    require("mason-lspconfig").setup({
      ensure_installed = {
        "lua_ls", "ts_ls", "html", "rust_analyzer",
        "clangd", "pyright", "jdtls", "kotlin_language_server", "bashls", "marksman",
      },
    })
    vim.diagnostic.config({
      virtual_text = true,
      signs = true,
      underline = true,
      -- THE KEY: Don't refresh error highlights while in Insert/Visual changes
      update_in_insert = false,
      severity_sort = true,
    })

    -- Ensure the sign column doesn't "pop" in and out
    vim.opt.signcolumn = "yes"

    -- 1. Shared on_attach (The Flicker Fix is here)
    local on_attach = function(client, bufnr)
      -- DISBALE SEMANTIC TOKENS: This stops the LSP from "repainting" over Treesitter
      if client.supports_method("textDocument/semanticTokens") then
        vim.lsp.semantic_tokens.stop(bufnr, client.id)
      end
      local opts = function(desc)
        return { buffer = bufnr, desc = desc, noremap = true, silent = true }
      end

      -- Keybindings (Consolidated)
      vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts("Go to definition"))
      vim.keymap.set("n", "gr", vim.lsp.buf.references, opts("Show references"))
      vim.keymap.set("n", "K", vim.lsp.buf.hover, opts("Hover documentation"))
      vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts("Rename symbol"))
      vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts("Code action"))
      vim.keymap.set('n', '<leader>f', function() vim.lsp.buf.format { async = true } end, opts("Format file"))

      -- Diagnostics
      vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, opts("Show line diagnostics"))
      vim.keymap.set("n", "]d", function() vim.diagnostic.jump({ count = 1, float = true }) end, opts("Next diagnostic"))
      vim.keymap.set("n", "[d", function() vim.diagnostic.jump({ count = -1, float = true }) end, opts("Previous diagnostic"))
    end

    -- 2. Capabilities (Blink.cmp)
    local capabilities = vim.lsp.protocol.make_client_capabilities()
    local ok, blink = pcall(require, "blink.cmp")
    if ok then capabilities = blink.get_lsp_capabilities(capabilities) end

    -- 3. Server Configurations
    local servers = {
      lua_ls = {
        settings = {
          Lua = {
            diagnostics = { globals = { "vim" } },
            workspace = { library = vim.api.nvim_get_runtime_file("", true), checkThirdParty = false },
            telemetry = { enable = false },
          },
        },
      },
      ts_ls = {},
      html = {},
      rust_analyzer = {},
      clangd = {},
      pyright = {},
      jdtls = {},
      kotlin_language_server = {},
      bashls = {},
      marksman = {},
    }

    -- 4. Automatically setup all servers using vim.lsp.config (Neovim 0.11+ API)
    for server, config in pairs(servers) do
      config.capabilities = capabilities
      config.on_attach = on_attach
      vim.lsp.config(server, config)
    end
  end
}
