return {
  {
    "L3MON4D3/LuaSnip",
    version = "v2.*",
    build = "make install_jsregexp",
    config = function()
      local ls = require("luasnip")
      
      -- Load snippets from lua files
      require("luasnip.loaders.from_lua").load({ paths = "~/.config/nvim/snippets/" })
      
      -- Keybindings for snippet navigation
      vim.keymap.set({"i", "s"}, "<C-k>", function()
        if ls.expand_or_jumpable() then
          ls.expand_or_jump()
        end
      end, { silent = true, desc = "Jump to next snippet placeholder" })
      
      vim.keymap.set({"i", "s"}, "<C-j>", function()
        if ls.jumpable(-1) then
          ls.jump(-1)
        end
      end, { silent = true, desc = "Jump to previous snippet placeholder" })
    end,
  },
  
  -- Completion source for LuaSnip
  {
    "saadparwaiz1/cmp_luasnip",
    dependencies = { "L3MON4D3/LuaSnip" },
  },
}
