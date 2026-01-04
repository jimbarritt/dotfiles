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
        if not node or node.nodes then 
          api.node.open.edit()
          return 
        end

        -- 1. Open the file but tell Nvim-Tree NOT to move the cursor
        -- This is the native way to 'preview' a file
        api.node.open.no_window_picker() 
        
        -- 2. Jump focus back immediately just in case the API moved it
        vim.cmd('wincmd p')

        -- 3. Fix the highlighting on the "First File"
        vim.schedule(function()
          -- Get the buffer number of the file we just opened
          local bufnr = vim.fn.bufnr(node.absolute_path)
          if bufnr ~= -1 then
            -- Force detection without moving our cursor
            vim.api.nvim_buf_call(bufnr, function()
              if vim.bo.filetype == "" or vim.bo.filetype == "plaintex" then
                vim.cmd("filetype detect")
              end
            end)
          end
        end)
      end

      -- Default mappings
      api.config.mappings.default_on_attach(bufnr)

      vim.keymap.set('n', 'l', open_and_focus, opts('Open'))
      vim.keymap.set('n', 'o', open_and_focus, opts('Open'))
      vim.keymap.set('n', '<cr>', open_and_focus, opts('Open'))
      vim.keymap.set('n', 'h', api.node.navigate.parent_close, opts('Close Directory'))
      vim.keymap.set('n', 'H', api.tree.toggle_hidden_filter, opts('Toggle Hidden'))

      vim.keymap.set('n', '<leader>fe', '<cmd>NvimTreeToggle<CR>', opts('Toggle tree'))  -- Add this
    end

    require("nvim-tree").setup({
      on_attach = on_attach,

      sort = {
        sorter = "name",
        folders_first = true,
        files_first = false,
      },

      view = {
        width = 35,
        number = true,
        relativenumber = true,
        signcolumn = "yes"
      },

      update_focused_file = {
        enable = true,
        update_root = false,  -- don't change root when switching files
        ignore_list = {},
      },
      
      -- Optional: also sync when opening a new file
      actions = {
        open_file = {
          quit_on_open = false,
        },
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
        indent_width = 2,
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
