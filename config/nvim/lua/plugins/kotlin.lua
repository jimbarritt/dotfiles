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
      local height = math.min(#lines + 2, math.floor(vim.o.lines * 0.8))
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

    -- Run a diagnostic check and write results to a log file.
    -- Returns the log lines (array of strings) and the log file path.
    local function run_diagnostic()
      local log = {}
      local function add(s) table.insert(log, s) end
      local function add_blank() table.insert(log, "") end
      local function run_cmd(cmd)
        local result = vim.fn.system(cmd .. " 2>&1")
        return (result or ""):gsub("%s+$", ""), vim.v.shell_error
      end

      add("=== Kotlin LSP Diagnostic ===")
      add("Timestamp: " .. os.date("%Y-%m-%d %H:%M:%S"))
      add("Machine:   " .. (run_cmd("hostname")))
      add("OS:        " .. (run_cmd("uname -sr")))
      add_blank()

      -- Environment variables
      add("--- Environment ---")
      add("KOTLIN_LSP_DIR: " .. (os.getenv("KOTLIN_LSP_DIR") or "(not set)"))
      add("JAVA_HOME:      " .. (os.getenv("JAVA_HOME") or "(not set)"))
      add("PATH (first 3):")
      local path = os.getenv("PATH") or ""
      local count = 0
      for segment in path:gmatch("[^:]+") do
        count = count + 1
        if count <= 3 then add("  " .. segment) end
      end
      add_blank()

      -- Homebrew formula check
      add("--- Homebrew Formula (jetbrains/utils/kotlin-lsp) ---")
      local formula_prefix, formula_err = run_cmd("brew --prefix jetbrains/utils/kotlin-lsp")
      if formula_err == 0 and formula_prefix ~= "" then
        add("Prefix: " .. formula_prefix)
        local libexec = formula_prefix .. "/libexec"
        add("libexec/ exists: " .. (vim.fn.isdirectory(libexec) == 1 and "YES" or "NO"))
        if vim.fn.isdirectory(libexec) == 1 then
          add("  lib/ exists:   " .. (vim.fn.isdirectory(libexec .. "/lib") == 1 and "YES" or "NO"))
          add("  jre/ exists:   " .. (vim.fn.isdirectory(libexec .. "/jre") == 1 and "YES" or "NO"))
          local sh = libexec .. "/kotlin-lsp.sh"
          local bin = libexec .. "/bin/kotlin-lsp"
          add("  kotlin-lsp.sh: " .. (vim.fn.filereadable(sh) == 1 and "YES" or "NO"))
          add("  bin/kotlin-lsp:" .. (vim.fn.filereadable(bin) == 1 and " YES" or " NO"))
        end
        local formula_version = run_cmd("brew list --versions jetbrains/utils/kotlin-lsp")
        add("Version: " .. (formula_version ~= "" and formula_version or "(not installed)"))
      else
        add("Not installed (or tap not added)")
      end
      add_blank()

      -- Homebrew cask check
      add("--- Homebrew Cask (kotlin-lsp) ---")
      local cask_info, cask_err = run_cmd("brew list --cask kotlin-lsp")
      if cask_err == 0 then
        add("Installed: YES")
        local cask_version = run_cmd("brew list --cask --versions kotlin-lsp")
        add("Version: " .. (cask_version ~= "" and cask_version or "(unknown)"))
        -- Find cask directory
        local caskroom = "/opt/homebrew/Caskroom/kotlin-lsp"
        if vim.fn.isdirectory(caskroom) == 1 then
          add("Caskroom: " .. caskroom)
          local versions = vim.fn.glob(caskroom .. "/*", false, true)
          for _, vdir in ipairs(versions) do
            add("  Version dir: " .. vdir)
            add("    lib/ exists:   " .. (vim.fn.isdirectory(vdir .. "/lib") == 1 and "YES" or "NO"))
            add("    jre/ exists:   " .. (vim.fn.isdirectory(vdir .. "/jre") == 1 and "YES" or "NO"))
            add("    kotlin-lsp.sh: " .. (vim.fn.filereadable(vdir .. "/kotlin-lsp.sh") == 1 and "YES" or "NO"))
          end
        end
      else
        add("Not installed")
      end
      add_blank()

      -- Binary on PATH
      add("--- Binary ---")
      local which_bin, which_err = run_cmd("which kotlin-lsp")
      if which_err == 0 and which_bin ~= "" then
        add("which kotlin-lsp: " .. which_bin)
        local real_path = run_cmd("readlink -f " .. which_bin)
        if real_path ~= "" then
          add("Resolves to:     " .. real_path)
        end
        local version_out = run_cmd(which_bin .. " --version")
        add("Version output:  " .. (version_out ~= "" and version_out or "(no output)"))
      else
        add("kotlin-lsp: NOT FOUND on PATH")
      end
      add_blank()

      -- JRE check
      add("--- JRE ---")
      local java_out = run_cmd("java -version")
      if java_out ~= "" then
        -- java -version outputs to stderr, which we captured via 2>&1
        add("System java: " .. java_out:match("[^\n]+"))
      else
        add("System java: not found")
      end
      add_blank()

      -- ~/.zshrc_machine contents
      add("--- ~/.zshrc_machine ---")
      local machine_rc = os.getenv("HOME") .. "/.zshrc_machine"
      local f = io.open(machine_rc, "r")
      if f then
        local contents = f:read("*a")
        f:close()
        if contents:find("KOTLIN") then
          for line in contents:gmatch("[^\n]+") do
            if line:find("KOTLIN") then
              add("  " .. line)
            end
          end
        else
          add("  (no KOTLIN entries)")
        end
      else
        add("  (file does not exist)")
      end
      add_blank()

      -- LSP log tail
      add("--- LSP Log (last 10 lines) ---")
      local log_path = vim.fn.stdpath("log") .. "/lsp.log"
      if vim.fn.filereadable(log_path) == 1 then
        local log_lines = vim.fn.readfile(log_path)
        local start = math.max(1, #log_lines - 9)
        for i = start, #log_lines do
          add("  " .. log_lines[i])
        end
      else
        add("  (no lsp.log found)")
      end

      -- Write to log file
      local state_dir = vim.fn.stdpath("state")
      vim.fn.mkdir(state_dir, "p")
      local log_path_out = state_dir .. "/kotlin-lsp-diag.log"
      local fout = io.open(log_path_out, "w")
      if fout then
        fout:write(table.concat(log, "\n") .. "\n")
        fout:close()
      end

      return log, log_path_out
    end

    -- Resolve kotlin-lsp location with auto-detection:
    --   1. KOTLIN_LSP_DIR env var set and valid → use it
    --   2. Check Homebrew cask (preferred, newer) → /opt/homebrew/Caskroom/kotlin-lsp/<ver>/
    --   3. Check Homebrew formula → brew --prefix → libexec/
    --   4. Check Mason → ~/.local/share/nvim/mason/packages/kotlin-lsp/lib
    --   5. Check ~/.zshrc_machine for previously saved path
    --   6. Not found → show install instructions
    local mason_lib = vim.fn.stdpath("data") .. "/mason/packages/kotlin-lsp/lib"
    local env_dir = os.getenv("KOTLIN_LSP_DIR")
    local has_mason = vim.fn.isdirectory(mason_lib) == 1
    local lsp_dir = nil

    if env_dir and vim.fn.isdirectory(env_dir .. "/lib") == 1 then
      -- Fast path: env var is set and valid
      lsp_dir = env_dir
      -- Nudge if pointing to the outdated formula layout
      if env_dir:find("/Cellar/") or env_dir:find("/libexec") then
        vim.schedule(function()
          show_popup({
            "KOTLIN_LSP_DIR points to the Homebrew formula (outdated):",
            "  " .. env_dir,
            "",
            "To upgrade to the official cask:",
            "  brew uninstall jetbrains/utils/kotlin-lsp",
            "  brew install --cask kotlin-lsp",
            "",
            "Then remove the KOTLIN_LSP_DIR line from ~/.zshrc_machine",
            "and restart nvim.",
            "",
            "Press q to close",
          }, "WarningMsg")
        end)
      end
    elseif has_mason then
      lsp_dir = mason_lib
    else
      -- Try Homebrew cask first (preferred)
      local caskroom = "/opt/homebrew/Caskroom/kotlin-lsp"
      if vim.fn.isdirectory(caskroom) == 1 then
        local versions = vim.fn.glob(caskroom .. "/*", false, true)
        -- Use the latest version directory
        table.sort(versions)
        local latest = versions[#versions]
        if latest and vim.fn.isdirectory(latest .. "/lib") == 1 then
          lsp_dir = latest
        end
      end

      -- Fall back to Homebrew formula (outdated — nudge user to switch)
      if not lsp_dir then
        local brew_prefix = vim.fn.system("brew --prefix kotlin-lsp 2>/dev/null"):gsub("%s+$", "")
        if vim.v.shell_error == 0 and brew_prefix ~= "" then
          local detected_dir = brew_prefix .. "/libexec"
          if vim.fn.isdirectory(detected_dir .. "/lib") == 1 then
            lsp_dir = detected_dir
            vim.schedule(function()
              show_popup({
                "Using Homebrew formula (jetbrains/utils/kotlin-lsp).",
                "This is outdated — the official cask is newer.",
                "",
                "To upgrade:",
                "  brew uninstall jetbrains/utils/kotlin-lsp",
                "  brew install --cask kotlin-lsp",
                "",
                "Then remove the KOTLIN_LSP_DIR line from ~/.zshrc_machine",
                "and restart nvim.",
                "",
                "Press q to close",
              }, "WarningMsg")
            end)
          end
        end
      end

      -- Fall back to ~/.zshrc_machine saved path
      if not lsp_dir then
        local machine_rc = os.getenv("HOME") .. "/.zshrc_machine"
        local f = io.open(machine_rc, "r")
        if f then
          local existing = f:read("*a")
          f:close()
          local saved_dir = existing:match('export KOTLIN_LSP_DIR="([^"]+)"')
          if saved_dir and vim.fn.isdirectory(saved_dir .. "/lib") == 1 then
            lsp_dir = saved_dir
            vim.api.nvim_echo({{"No KOTLIN_LSP_DIR env var — using path from ~/.zshrc_machine. Reload your shell.", "WarningMsg"}}, true, {})
          end
        end
      end

      -- Auto-write to ~/.zshrc_machine if we found it and it's not already there
      if lsp_dir then
        local machine_rc = os.getenv("HOME") .. "/.zshrc_machine"
        local existing = ""
        local f = io.open(machine_rc, "r")
        if f then
          existing = f:read("*a")
          f:close()
        end
        local export_line = 'export KOTLIN_LSP_DIR="' .. lsp_dir .. '"'
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

    -- Set the env var for this nvim session so kotlin.nvim can find it
    if lsp_dir and not os.getenv("KOTLIN_LSP_DIR") then
      vim.fn.setenv("KOTLIN_LSP_DIR", lsp_dir)
    end

    if not lsp_dir then
      vim.schedule(function()
        local diag_log, log_path = run_diagnostic()
        local popup_lines = {
          "Kotlin LSP not found.",
          "",
          "Install via Homebrew (cask):",
          "  brew install --cask kotlin-lsp",
          "",
          "Then restart nvim — it will be detected automatically.",
          "After first restart, reload your shell.",
          "",
          "Diagnostic log written to:",
          "  " .. log_path,
          "",
          "Press q to close",
        }
        show_popup(popup_lines, "WarningMsg")
      end)
      return
    end

    -- Validate the resolved dir still exists
    if vim.fn.isdirectory(lsp_dir .. "/lib") ~= 1 then
      vim.schedule(function()
        local diag_log, log_path = run_diagnostic()
        local popup_lines = {
          "Kotlin LSP directory is invalid:",
          "  " .. lsp_dir,
          "",
          "Reinstall via:",
          "  brew install --cask kotlin-lsp",
          "",
          "Diagnostic log written to:",
          "  " .. log_path,
          "",
          "Then restart nvim.",
          "",
          "Press q to close",
        }
        show_popup(popup_lines, "WarningMsg")
      end)
      return
    end

    -- Resolve bundled JRE from the detected lsp_dir
    -- Cask layout:  <caskroom>/<ver>/jre/...
    -- Formula layout: libexec/jre/...
    -- Either way, jre lives alongside lib/ under lsp_dir.
    local jre_base = lsp_dir .. "/jre"
    local bundled_jre = nil
    if vim.fn.isdirectory(jre_base) == 1 then
      local candidate = jre_base .. "/Contents/Home"  -- macOS layout
      if vim.fn.isdirectory(candidate) == 1 then
        bundled_jre = candidate
      elseif vim.fn.isdirectory(jre_base .. "/bin") == 1 then
        bundled_jre = jre_base  -- Linux layout
      end
    end

    require("kotlin").setup({
      jre_path = bundled_jre,  -- use brew-bundled JRE if available
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

    -- Check that the LSP actually starts — run diagnostics if it fails
    vim.defer_fn(function()
      local clients = vim.lsp.get_clients({ name = "kotlin_ls" })
      if #clients == 0 then
        local diag_log, log_path = run_diagnostic()

        local popup_lines = {
          "Kotlin LSP failed to start.",
          "",
          "Resolved dir: " .. lsp_dir,
        }
        -- Show binary status
        local sh = lsp_dir .. "/kotlin-lsp.sh"
        local bin = lsp_dir .. "/bin/kotlin-lsp"
        if vim.fn.filereadable(sh) == 1 then
          table.insert(popup_lines, "Binary:      " .. sh .. " (found)")
        elseif vim.fn.filereadable(bin) == 1 then
          table.insert(popup_lines, "Binary:      " .. bin .. " (found)")
        else
          table.insert(popup_lines, "Binary:      NOT FOUND")
        end
        table.insert(popup_lines, "")
        table.insert(popup_lines, "Full diagnostic log written to:")
        table.insert(popup_lines, "  " .. log_path)
        table.insert(popup_lines, "")
        table.insert(popup_lines, "Check :LspLog for LSP-specific errors.")
        table.insert(popup_lines, "")
        table.insert(popup_lines, "Press q to close")
        show_popup(popup_lines, "WarningMsg")
      end
    end, 10000)
  end,
}
