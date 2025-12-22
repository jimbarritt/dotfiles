return {
  "windwp/nvim-autopairs",
  event = "InsertEnter",
  config = function()
    local autopairs = require("nvim-autopairs")
    
    autopairs.setup({
      check_ts = true,  -- Use treesitter for better context awareness
      ts_config = {
        lua = {'string'},  -- Don't add pairs in lua string nodes
        javascript = {'template_string'},
      },
      fast_wrap = {
        map = '<M-e>',  -- Alt-e to quickly wrap with pairs
      },
    })

    -- Integrate with nvim-cmp (if you have it)
    local cmp_autopairs = require('nvim-autopairs.completion.cmp')
    local cmp = require('cmp')
    cmp.event:on(
      'confirm_done',
      cmp_autopairs.on_confirm_done()
    )
  end,
}
