return {
  {
    "L3MON4D3/LuaSnip",
    version = "v2.*",
    build = "make install_jsregexp",
    config = function()
      -- Load snippets from lua files
      require("luasnip.loaders.from_lua").load({ paths = "~/.config/nvim/snippets/" })

      -- Snippet placeholder navigation uses <Tab> / <S-Tab> (configured in
      -- completion.lua). <C-j>/<C-k> are reserved for insert-mode Down/Up.
    end,
  },
  
  -- Completion source for LuaSnip
  {
    "saadparwaiz1/cmp_luasnip",
    dependencies = { "L3MON4D3/LuaSnip" },
  },
}
