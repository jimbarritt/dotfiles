return {
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = function()
      local colors = {
        green = "#00ff00",
        dark_green = "#00aa00",
        forest = "#008000",
        bg_green = "#1a3a1a",
        black = "#000000",
        gray = "#808080",
      }

      local mode = function()
        local mode_map = {
          ['n']  = 'N',
          ['i']  = 'I',
          ['v']  = 'v',
          ['V']  = 'V',
          [''] = 'B',
          ['c']  = 'C',
          ['r']  = 'R',
          ['t']  = 'T',
        }
        local ok, m = pcall(function() return vim.api.nvim_get_mode().mode end)
        if not ok then return '' end
        return mode_map[m] or m
      end

      local lsp_status = function()
        return "LSP"
      end

      local green_theme = {
        normal = {
          a = { bg = colors.green, fg = colors.black, gui = "bold" },
          b = { bg = colors.bg_green, fg = colors.dark_green },
          c = { bg = colors.bg_green, fg = colors.dark_green },
        },
        insert = {
          a = { bg = colors.dark_green, fg = colors.black, gui = "bold" },
          b = { bg = colors.bg_green, fg = colors.dark_green },
          c = { bg = colors.bg_green, fg = colors.dark_green },
        },
        visual = {
          a = { bg = colors.forest, fg = colors.green, gui = "bold" },
          b = { bg = colors.bg_green, fg = colors.dark_green },
          c = { bg = colors.bg_green, fg = colors.dark_green },
        },
        command = {
          a = { bg = colors.green, fg = colors.black, gui = "bold" },
          b = { bg = colors.bg_green, fg = colors.dark_green },
          c = { bg = colors.bg_green, fg = colors.dark_green },
        },
        inactive = {
          a = { bg = colors.black, fg = colors.gray },
          b = { bg = colors.black, fg = colors.gray },
          c = { bg = colors.black, fg = colors.gray },
        },
      }

      return {
        options = {
          theme = green_theme,
          component_separators = "",
          section_separators = "",
          globalstatus = true,
        },
        sections = {
          lualine_a = { mode },
          lualine_b = {},
          lualine_c = { "filename" },
          lualine_x = {},
          lualine_y = { "location" },
          lualine_z = { "progress" },
        },
      }
    end,
  },
}
