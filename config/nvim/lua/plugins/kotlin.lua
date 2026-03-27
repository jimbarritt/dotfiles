-- JetBrains official Kotlin LSP via kotlin.nvim
-- Provides enhanced Neovim integration: import organizing, formatting,
-- decompilation, and per-project workspace isolation.
return {
  "AlexandrosAlexiou/kotlin.nvim",
  ft = "kotlin",
  dependencies = {
    "neovim/nvim-lspconfig",
    "mason-org/mason.nvim",
  },
  opts = {
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
  },
}
