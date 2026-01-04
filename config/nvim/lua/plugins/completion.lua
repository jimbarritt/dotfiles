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

      preselect = cmp.PreselectMode.Item,

      mapping = cmp.mapping.preset.insert({
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-g>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({ select = true }),

        -- Smart dot: trigger completion if after identifier
        ['.'] = cmp.mapping(function(fallback)
            -- Check if previous character (before dot) is alphanumeric or underscore
            local col = vim.fn.col('.') - 2
            local line = vim.fn.getline('.')
            local char = string.sub(line, col, col)
            
            fallback() -- Insert the dot first

            if char:match('[%w_]') or char == ')' or char == ']' then
              vim.schedule(function() 
                cmp.complete() 
              end) 
            end
          end, { 'i' }),

        ['<Tab>'] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          elseif luasnip.expand_or_jumpable() then
            luasnip.expand_or_jump()
          else
            cmp.complete()
            -- Check if only one item after completing
            vim.defer_fn(function()
              if cmp.visible() then
                local entries = cmp.get_entries()
                if #entries == 1 then
                  cmp.confirm({ select = true })
                end
              end
            end, 50)  -- Small delay to let completion populate
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
