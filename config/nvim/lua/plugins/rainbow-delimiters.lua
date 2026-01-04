return {
  "HiPhish/rainbow-delimiters.nvim",
  config = function()
    local rainbow_delimiters = require('rainbow-delimiters')
    
    vim.g.rainbow_delimiters = {
      strategy = {
        [''] = rainbow_delimiters.strategy['global'],
        vim = rainbow_delimiters.strategy['local'],
      },
      query = {
        [''] = 'rainbow-delimiters',    -- For (), [], {}
        lua = 'rainbow-blocks',          -- For function/end, if/end, etc.
      },
      highlight = {
        'RainbowDelimiterGreen1',
        'RainbowDelimiterGreen2',
        'RainbowDelimiterGreen3',
        'RainbowDelimiterGreen4',
        'RainbowDelimiterGreen5',
        'RainbowDelimiterGreen6',
        'RainbowDelimiterGreen7',
      },
    }
    
    -- Smooth gradient from bright green down to almost comment-dark
    vim.cmd([[
      hi RainbowDelimiterGreen1 guifg=#7fd87f
      hi RainbowDelimiterGreen2 guifg=#68c868
      hi RainbowDelimiterGreen3 guifg=#5ab85a
      hi RainbowDelimiterGreen4 guifg=#50a050
      hi RainbowDelimiterGreen5 guifg=#4a8a4a
      hi RainbowDelimiterGreen6 guifg=#4a7a60
      hi RainbowDelimiterGreen7 guifg=#4a6a5a
    ]])
  end,
}
