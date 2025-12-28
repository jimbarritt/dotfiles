return {
  "nvim-tree/nvim-tree.lua",
  config = function()
    require("nvim-tree").setup({
      view = {
        width = 35,
        number = true,
        relativenumber = true,
      },
      renderer = {
        indent_width = 2,
        icons = {
          show = {
            file = false,
            folder = false,
            folder_arrow = false,  -- No arrows at all
            git = false,
          },
        },
        indent_markers = {
          enable = true,
          inline_arrows = false,
          icons = {
            corner = "└──",
            edge = "│  ",
            item = "├──",
            none = "   ",
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
          "Library/Calendars"
        }
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
