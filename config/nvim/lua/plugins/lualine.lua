local function get_colors()
  if vim.o.background == "light" then
    return {
      green = "#006600",
      dark_green = "#338833",
      forest = "#228822",
      bg_green = "#d8e8d8",
      black = "#ffffff",
      gray = "#808080",
    }
  else
    return {
      green = "#00ff00",
      dark_green = "#00aa00",
      forest = "#008000",
      bg_green = "#1a3a1a",
      black = "#000000",
      gray = "#808080",
    }
  end
end

local function build_theme()
  local c = get_colors()
  return {
    normal = {
      a = { bg = c.green, fg = c.black, gui = "bold" },
      b = { bg = c.bg_green, fg = c.dark_green },
      c = { bg = c.bg_green, fg = c.dark_green },
    },
    insert = {
      a = { bg = c.dark_green, fg = c.black, gui = "bold" },
      b = { bg = c.bg_green, fg = c.dark_green },
      c = { bg = c.bg_green, fg = c.dark_green },
    },
    visual = {
      a = { bg = c.forest, fg = c.green, gui = "bold" },
      b = { bg = c.bg_green, fg = c.dark_green },
      c = { bg = c.bg_green, fg = c.dark_green },
    },
    command = {
      a = { bg = c.green, fg = c.black, gui = "bold" },
      b = { bg = c.bg_green, fg = c.dark_green },
      c = { bg = c.bg_green, fg = c.dark_green },
    },
    inactive = {
      a = { bg = c.black, fg = c.gray },
      b = { bg = c.black, fg = c.gray },
      c = { bg = c.black, fg = c.gray },
    },
  }
end

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

local function build_opts()
  return {
    options = {
      theme = build_theme(),
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
            local c = get_colors()
            local clients = vim.lsp.get_clients({ bufnr = 0 })
            if #clients == 0 then
              return { fg = c.dark_green }
            end
            local all_initialized = true
            for _, client in ipairs(clients) do
              if not client.initialized then
                all_initialized = false
                break
              end
            end
            if all_initialized then
              return { fg = c.dark_green }
            else
              return { fg = c.dark_green }
            end
          end
        },
        "location"
      },
      lualine_z = { "progress" },
    },
  }
end

return {
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("lualine").setup(build_opts())
      -- Refresh statusline for spinner animation
      local timer = vim.uv.new_timer()
      timer:start(0, 80, vim.schedule_wrap(function()
        require("lualine").refresh()
      end))
      -- Re-apply lualine theme when colorscheme changes (e.g. presentation mode)
      vim.api.nvim_create_autocmd("ColorScheme", {
        callback = function()
          vim.defer_fn(function()
            require("lualine").setup(build_opts())
          end, 50)
        end,
      })
    end,
  },
}
