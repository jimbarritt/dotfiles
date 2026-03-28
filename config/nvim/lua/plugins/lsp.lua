return {
  "neovim/nvim-lspconfig",
  dependencies = {
    "mason-org/mason.nvim",
    "mason-org/mason-lspconfig.nvim",
  },
  config = function()
    vim.diagnostic.config({
      virtual_text = false,
      signs = {
        text = {
          [vim.diagnostic.severity.ERROR] = '●',
          [vim.diagnostic.severity.WARN]  = '●',
          [vim.diagnostic.severity.INFO]  = '●',
          [vim.diagnostic.severity.HINT]  = '●',
        },
      },
      underline = true,
      -- THE KEY: Don't refresh error highlights while in Insert/Visual changes
      update_in_insert = false,
      severity_sort = true,
      float = {
        border = "rounded",
        source = true,
      },
    })

    -- Ensure the sign column doesn't "pop" in and out
    vim.opt.signcolumn = "yes"

    -- Workaround: kotlin-lsp sends stale document versions with rename edits,
    -- causing "Buffer newer than edits" errors. Strip versions so Neovim applies them.
    local rename_handler = vim.lsp.handlers["textDocument/rename"]
    vim.lsp.handlers["textDocument/rename"] = function(err, result, ctx, config)
      if result then
        if result.documentChanges then
          for _, change in ipairs(result.documentChanges) do
            if change.textDocument then
              change.textDocument.version = nil
            end
          end
        end
      end
      return rename_handler(err, result, ctx, config)
    end

    -- 1. Shared on_attach — runs for ALL LSP clients via LspAttach autocmd
    local function on_attach(client, bufnr)
      -- DISABLE SEMANTIC TOKENS: This stops the LSP from "repainting" over Treesitter
      if client.supports_method("textDocument/semanticTokens") then
        vim.lsp.semantic_tokens.stop(bufnr, client.id)
      end
      local opts = function(desc)
        return { buffer = bufnr, desc = desc, noremap = true, silent = true }
      end

      -- Keybindings (Consolidated)
      vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts("Go to definition"))
      vim.keymap.set("n", "gI", vim.lsp.buf.implementation, opts("Go to implementation"))
      vim.keymap.set("n", "gr", vim.lsp.buf.references, opts("Show references"))
      vim.keymap.set("n", "K", function()
        vim.lsp.buf.hover({ border = "rounded", pad_left = 1, pad_right = 1 })
      end, opts("Hover documentation"))
      vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts("Rename symbol"))
      vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts("Code action"))
      vim.keymap.set('n', '<leader>f', function() vim.lsp.buf.format { async = true } end, opts("Format file"))

      -- Diagnostics
      vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, opts("Show line diagnostics"))
      vim.keymap.set("n", "]d", function() vim.diagnostic.jump({ count = 1, float = true }) end, opts("Next diagnostic"))
      vim.keymap.set("n", "[d", function() vim.diagnostic.jump({ count = -1, float = true }) end, opts("Previous diagnostic"))
    end

    vim.api.nvim_create_autocmd("LspAttach", {
      callback = function(args)
        local client = vim.lsp.get_client_by_id(args.data.client_id)
        if client then
          on_attach(client, args.buf)
        end
      end,
    })

    -- 2. Capabilities (Blink.cmp + file watching)
    local capabilities = vim.lsp.protocol.make_client_capabilities()
    local ok, blink = pcall(require, "blink.cmp")
    if ok then capabilities = blink.get_lsp_capabilities(capabilities) end

    -- Enable workspace/didChangeWatchedFiles so the LSP re-indexes
    -- when external tools (e.g. Claude) modify project files on disk.
    capabilities.workspace = capabilities.workspace or {}
    capabilities.workspace.didChangeWatchedFiles = capabilities.workspace.didChangeWatchedFiles or {}
    capabilities.workspace.didChangeWatchedFiles.dynamicRegistration = true

    -- 3. Server Configurations
    local servers = {
      lua_ls = {
        settings = {
          Lua = {
            diagnostics = { globals = { "vim" } },
            workspace = { library = vim.api.nvim_get_runtime_file("", true), checkThirdParty = false },
            telemetry = { enable = false },
          },
        },
      },
      ts_ls = {},
      html = {},
      rust_analyzer = {},
      clangd = {},
      pyright = {},
      jdtls = {},
      -- Kotlin LSP is managed by kotlin.nvim plugin (JetBrains official LSP)
      bashls = {},
      marksman = {},
    }

    -- 4. Automatically setup all servers using vim.lsp.config (Neovim 0.11+ API)
    for server, config in pairs(servers) do
      config.capabilities = capabilities
      config.on_attach = on_attach
      vim.lsp.config(server, config)
    end

    -- Clean quickfix format: just filename and line content, aligned
    vim.o.quickfixtextfunc = "v:lua.QfTextFunc"
    function _G.QfTextFunc(info)
      local items = vim.fn.getqflist({ id = info.id, items = 1 }).items
      local results = {}
      -- First pass: find max label length for alignment
      local max_label = 0
      for i = info.start_idx, info.end_idx do
        local item = items[i]
        local label = vim.fn.fnamemodify(vim.fn.bufname(item.bufnr), ":t") .. ":" .. item.lnum
        if #label > max_label then max_label = #label end
      end
      -- Second pass: build formatted lines
      for i = info.start_idx, info.end_idx do
        local item = items[i]
        local label = vim.fn.fnamemodify(vim.fn.bufname(item.bufnr), ":t") .. ":" .. item.lnum
        local text = vim.trim(item.text or "")
        table.insert(results, string.format("%-" .. max_label .. "s  %s", label, text))
      end
      return results
    end

    -- Export shared config for use by kotlin.nvim and other external LSP plugins
    _G._lsp_on_attach = on_attach
    _G._lsp_capabilities = capabilities
  end
}
