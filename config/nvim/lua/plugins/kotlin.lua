-- JetBrains official Kotlin LSP via kotlin.nvim
-- Provides enhanced Neovim integration: import organizing, formatting,
-- decompilation, and per-project workspace isolation.
return {
  "AlexandrosAlexiou/kotlin.nvim",
  ft = "kotlin",
  dependencies = {
    "neovim/nvim-lspconfig",
  },
  config = function()
    -- Show an info popup as a centered floating window
    local function show_popup(lines, highlight)
      highlight = highlight or "Normal"
      local width = 0
      for _, line in ipairs(lines) do
        width = math.max(width, #line)
      end
      width = math.min(width + 4, math.floor(vim.o.columns * 0.8))
      local height = #lines + 2
      local buf = vim.api.nvim_create_buf(false, true)
      -- Pad lines with leading spaces
      local padded = { "" }
      for _, line in ipairs(lines) do
        table.insert(padded, "  " .. line)
      end
      table.insert(padded, "")
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, padded)
      local win = vim.api.nvim_open_win(buf, true, {
        relative = "editor",
        width = width,
        height = height,
        col = math.floor((vim.o.columns - width) / 2),
        row = math.floor((vim.o.lines - height) / 2),
        style = "minimal",
        border = "rounded",
        title = " Kotlin LSP ",
        title_pos = "center",
      })
      vim.api.nvim_set_option_value("winhl", "Normal:" .. highlight .. ",FloatBorder:" .. highlight, { win = win })
      vim.api.nvim_buf_set_option(buf, "modifiable", false)
      -- Close on any key
      vim.keymap.set("n", "q", function()
        vim.api.nvim_win_close(win, true)
      end, { buffer = buf, nowait = true })
      vim.keymap.set("n", "<Esc>", function()
        vim.api.nvim_win_close(win, true)
      end, { buffer = buf, nowait = true })
      vim.keymap.set("n", "<CR>", function()
        vim.api.nvim_win_close(win, true)
      end, { buffer = buf, nowait = true })
    end

    -- Resolve kotlin-lsp location with auto-detection:
    --   1. KOTLIN_LSP_DIR env var set and valid → use it
    --   2. Not set → check if brew formula installed → auto-write to ~/.zshrc_machine
    --   3. Brew not installed → show install instructions
    local mason_lib = vim.fn.stdpath("data") .. "/mason/packages/kotlin-lsp/lib"
    local env_dir = os.getenv("KOTLIN_LSP_DIR")
    local has_mason = vim.fn.isdirectory(mason_lib) == 1
    local lsp_dir = nil

    if env_dir and vim.fn.isdirectory(env_dir .. "/lib") == 1 then
      -- Fast path: env var is set and valid
      lsp_dir = env_dir
    elseif has_mason then
      lsp_dir = mason_lib
    else
      -- No env var — try to detect via Homebrew
      local brew_prefix = vim.fn.system("brew --prefix kotlin-lsp 2>/dev/null"):gsub("%s+$", "")
      if vim.v.shell_error == 0 and brew_prefix ~= "" then
        local detected_dir = brew_prefix .. "/libexec"
        if vim.fn.isdirectory(detected_dir .. "/lib") == 1 then
          lsp_dir = detected_dir
          -- Write to ~/.zshrc_machine so future shells pick it up
          local machine_rc = os.getenv("HOME") .. "/.zshrc_machine"
          local export_line = 'export KOTLIN_LSP_DIR="' .. detected_dir .. '"'
          -- Read existing content to avoid duplicating
          local existing = ""
          local f = io.open(machine_rc, "r")
          if f then
            existing = f:read("*a")
            f:close()
          end
          if not existing:find("KOTLIN_LSP_DIR") then
            f = io.open(machine_rc, "a")
            if f then
              f:write("\n" .. export_line .. "\n")
              f:close()
              vim.api.nvim_echo({{"Kotlin LSP detected — written to ~/.zshrc_machine. Reload your shell.", "Normal"}}, true, {})
            end
          end
        end
      end
    end

    if not lsp_dir then
      vim.schedule(function()
        show_popup({
          "Kotlin LSP not found.",
          "",
          "Install via Homebrew:",
          "  brew install JetBrains/utils/kotlin-lsp",
          "",
          "Then restart nvim — it will be detected automatically.",
          "",
          "After first restart, reload your shell.",
          "",
          "Press q to close",
        }, "WarningMsg")
      end)
      return
    end

    -- Validate the resolved dir still exists
    if vim.fn.isdirectory(lsp_dir .. "/lib") ~= 1 then
      vim.schedule(function()
        show_popup({
          "Kotlin LSP directory is invalid:",
          "  " .. lsp_dir,
          "",
          "Reinstall via:",
          "  brew install JetBrains/utils/kotlin-lsp",
          "",
          "Then restart nvim.",
          "",
          "Press q to close",
        }, "WarningMsg")
      end)
      return
    end

    require("kotlin").setup({
      lsp = {
        on_attach = function(client, bufnr)
          -- Re-use the shared on_attach from lsp.lua
          if _G._lsp_on_attach then
            _G._lsp_on_attach(client, bufnr)
          end
        end,
        capabilities = function()
          return _G._lsp_capabilities or vim.lsp.protocol.make_client_capabilities()
        end,
      },
    })
  end,
}
