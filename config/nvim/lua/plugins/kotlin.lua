-- JetBrains official Kotlin LSP via kotlin.nvim
-- Provides enhanced Neovim integration: import organizing, formatting,
-- decompilation, and per-project workspace isolation.
return {
  "AlexandrosAlexiou/kotlin.nvim",
  ft = "kotlin",
  dependencies = {
    "neovim/nvim-lspconfig",
  },
  config = function()
    -- Check that kotlin-lsp is available (Mason install or KOTLIN_LSP_DIR)
    local mason_lib = vim.fn.stdpath("data") .. "/mason/packages/kotlin-lsp/lib"
    local env_dir = os.getenv("KOTLIN_LSP_DIR")
    local has_mason = vim.fn.isdirectory(mason_lib) == 1
    local has_env = env_dir ~= nil and vim.fn.isdirectory(env_dir .. "/lib") == 1

    if not has_mason and not has_env then
      vim.notify(
        "kotlin-lsp not found. Install via Homebrew:\n"
          .. "  brew install JetBrains/utils/kotlin-lsp\n"
          .. "Then add to ~/.zshrc_work:\n"
          .. "  export KOTLIN_LSP_DIR=$(brew --prefix kotlin-lsp)",
        vim.log.levels.WARN,
        { title = "Kotlin LSP" }
      )
      return
    end

    require("kotlin").setup({
      lsp = {
        on_attach = function(client, bufnr)
          -- Re-use the shared on_attach from lsp.lua
          if _G._lsp_on_attach then
            _G._lsp_on_attach(client, bufnr)
          end
        end,
        capabilities = function()
          return _G._lsp_capabilities or vim.lsp.protocol.make_client_capabilities()
        end,
      },
    })
  end,
}
