return {
  "nvim-tree/nvim-tree.lua",
  config = function()
    local function on_attach(bufnr)
      local api = require('nvim-tree.api')
      
      local function opts(desc)
        return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
      end

      local function open_and_focus()
        local node = api.tree.get_node_under_cursor()
        api.node.open.edit()
        -- Only return focus if it's a file (not a directory)
        if not node.nodes then
          vim.cmd('wincmd p')
        end
      end
      
      -- Default mappings
      api.config.mappings.default_on_attach(bufnr)
      
      vim.keymap.set('n', 'l', open_and_focus, opts('Open'))
      vim.keymap.set('n', 'o', open_and_focus, opts('Open'))
      vim.keymap.set('n', '<cr>', open_and_focus, opts('Open'))
      vim.keymap.set('n', 'h', api.node.navigate.parent_close, opts('Close Directory'))
      vim.keymap.set('n', 'H', api.tree.toggle_hidden_filter, opts('Toggle Hidden'))
    end
    
    require("nvim-tree").setup({
      on_attach = on_attach,

      view = {
        width = 35,
        number = true,
        relativenumber = true,
      },

      actions = {
        open_file = {
          quit_on_open = false,  -- Don't close tree on file open
          window_picker = {
            enable = false,  -- Disable window picker
          },
        },
      },

      renderer = {
        indent_width = 4,
        icons = {
          show = {
            file = false,
            folder = false,
            folder_arrow = false,
            git = false,
          },
          symlink_arrow = " -> ",
        },
        indent_markers = {
          enable = true,
          inline_arrows = true,
          icons = {
            corner = "└",  -- Single char only
            edge = "│",    -- Single char only
            item = "├",    -- Single char only
            bottom = "─",  -- This creates the horizontal line
            none = " ",
          },
        },
        highlight_git = false,
        highlight_opened_files = "none",
        root_folder_label = false,
        special_files = {},
      },
      git = {
        enable = false,
      },
      filters = {
        dotfiles = true,
        custom = {
          "Library/Calendars",
        },
      },
    })
    
    local green = "#50fa7b"
    local light_green = "#7fc87f"
    
    vim.api.nvim_set_hl(0, "NvimTreeNormal", { bg = "NONE" })
    vim.api.nvim_set_hl(0, "NvimTreeFolderName", { fg = green })
    vim.api.nvim_set_hl(0, "NvimTreeOpenedFolderName", { fg = green, bold = true })
    vim.api.nvim_set_hl(0, "NvimTreeEmptyFolderName", { fg = green })
    vim.api.nvim_set_hl(0, "NvimTreeSymlink", { fg = green })
    vim.api.nvim_set_hl(0, "NvimTreeSymlinkFolderName", { fg = green })
    vim.api.nvim_set_hl(0, "NvimTreeFolderIcon", { fg = green })
    vim.api.nvim_set_hl(0, "NvimTreeNormalFile", { fg = green })
    vim.api.nvim_set_hl(0, "NvimTreeExecFile", { fg = green })
    vim.api.nvim_set_hl(0, "NvimTreeSpecialFile", { fg = green })
    vim.api.nvim_set_hl(0, "NvimTreeIndentMarker", { fg = light_green })
  end,
}
