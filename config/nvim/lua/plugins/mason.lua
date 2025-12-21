return {
  -- Mason: LSP server installer
  {
    "williamboman/mason.nvim",
    config = function()
      require("mason").setup()
    end,
  },

  -- Mason-LSPConfig: Bridge between Mason and LSP config
  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = {
      "williamboman/mason.nvim",
    },
    config = function()
      require("mason-lspconfig").setup({
        ensure_installed = {
          "ts_ls",           -- TypeScript/JavaScript
          "html",            -- HTML
          "rust_analyzer",   -- Rust
          "clangd",          -- C/C++
          "pyright",         -- Python
          "jdtls",           -- Java
          "kotlin_language_server", -- Kotlin
        },
        automatic_installation = true,
      })
    end,
  },
}
