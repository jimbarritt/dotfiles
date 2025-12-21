-- Completion configuration (nvim-cmp)
return {
  'hrsh7th/nvim-cmp',
  dependencies = {
    'hrsh7th/cmp-nvim-lsp',     -- LSP completion source
    'hrsh7th/cmp-buffer',       -- Buffer completion source
    'hrsh7th/cmp-path',         -- Path completion source
    'L3MON4D3/LuaSnip',         -- Snippet engine
    'saadparwaiz1/cmp_luasnip', -- Snippet completion source
  },
  config = function()
    local cmp = require('cmp')
    local luasnip = require('luasnip')

    cmp.setup({
      snippet = {
        expand = function(args)
          luasnip.lsp_expand(args.body)
        end,
      },

      -- Manual completion only (trigger with <C-Space>)
      completion = {
        autocomplete = false,
      },

      mapping = cmp.mapping.preset.insert({
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({ select = true }),
        ['<Tab>'] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          elseif luasnip.expand_or_jumpable() then
            luasnip.expand_or_jump()
          else
            fallback()
          end
        end, { 'i', 's' }),
        ['<S-Tab>'] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          elseif luasnip.jumpable(-1) then
            luasnip.jump(-1)
          else
            fallback()
          end
        end, { 'i', 's' }),
      }),

      sources = cmp.config.sources({
        { name = 'nvim_lsp' },
        { name = 'luasnip' },
      }, {
        { name = 'buffer' },
        { name = 'path' },
      }),

      formatting = {
        format = function(entry, vim_item)
          -- Add source name
          vim_item.menu = ({
            nvim_lsp = '[LSP]',
            luasnip = '[Snip]',
            buffer = '[Buf]',
            path = '[Path]',
          })[entry.source.name]
          return vim_item
        end,
      },
    })
  end,
}
