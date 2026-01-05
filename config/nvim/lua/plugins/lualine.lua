return {
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function(_, opts)
      require("lualine").setup(opts)
      -- Refresh statusline for spinner animation
      local timer = vim.uv.new_timer()
      timer:start(0, 80, vim.schedule_wrap(function()
        require("lualine").refresh()
      end))
    end,
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
        local clients = vim.lsp.get_clients({ bufnr = 0 })
        if #clients == 0 then
          return ""
        end
        return "(LSP)"
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
          lualine_y = {
            {
              lsp_status,
              color = function()
                local clients = vim.lsp.get_clients({ bufnr = 0 })
                if #clients == 0 then
                  return { fg = colors.dark_green }
                end

                local all_initialized = true
                for _, client in ipairs(clients) do
                  if not client.initialized then
                    all_initialized = false
                    break
                  end
                end

                if all_initialized then
                  return { fg = colors.dark_green }
                else
                  return { fg = colors.variable }
                end
              end
            },
            "location"
          },
          lualine_z = { "progress" },
        },
      }
    end,
  },
}
