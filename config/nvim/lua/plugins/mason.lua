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
          "ts_ls",          -- TypeScript/JavaScript
          "html",           -- HTML
          "rust_analyzer",  -- Rust
          "clangd",         -- C/C++
          "pyright",        -- Python
          "jdtls",          -- Java
          -- kotlin_lsp installed via Homebrew, not Mason (JetBrains CDN blocks Mason downloads)
          -- brew install JetBrains/utils/kotlin-lsp  →  set KOTLIN_LSP_DIR in ~/.zshrc_work
          "bashls",         -- Bash/Zsh
          "lua_ls",         -- Lua
          "marksman",       -- Markdown
        },
        automatic_installation = true,
      })
    end,
  },
}
