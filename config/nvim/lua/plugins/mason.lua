return {
  -- Mason: LSP server installer
  {
    "mason-org/mason.nvim",
    config = function()
      require("mason").setup()
    end,
  },

  -- Mason-LSPConfig: Bridge between Mason and LSP config
  {
    "mason-org/mason-lspconfig.nvim",
    dependencies = {
      "mason-org/mason.nvim",
    },
    config = function()
      require("mason-lspconfig").setup({
        ensure_installed = {
          "ts_ls",                   -- TypeScript/JavaScript
          "html",                    -- HTML
          "rust_analyzer",           -- Rust
          "clangd",                  -- C/C++
          "pyright",                 -- Python
          "jdtls",                   -- Java
          "kotlin-lsp",              -- Kotlin (JetBrains official)
          "bashls",                  -- Bash/Zsh 
          "lua_ls"      -- Lua
        },
        automatic_installation = true,
      })
    end,
  },
}
