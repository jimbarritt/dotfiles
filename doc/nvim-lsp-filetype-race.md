# Intermittent LSP attach in Neovim (FileType race)

Diagnosis and fix for LSP clients intermittently failing to attach when opening a
file from the command line. Found while setting up Go/gopls, but the cause is
language-agnostic and affected every server in `lua/plugins/lsp.lua`.

## Symptom

Opening a file with `nvim main.go` sometimes produced no LSP client:

```vim
:lua =vim.lsp.get_clients({bufnr=0})   " {}
```

Quitting and reopening the same file usually fixed it. Environment checks all passed
on a failing buffer:

```
{ file = ".../go-tutorial/main.go", ft = "go", go = 1, gopls = 1,
  root = "/Users/jmdb/Code/github/jimbarritt/go-tutorial" }
```

Toolchain on `PATH`, server binary present, filetype detected, module root resolved.

## Measurement

Repeated headless runs on one unchanged file:

```bash
nvim --headless -n main.go \
  "+lua local ok=vim.wait(45000, function() return #vim.lsp.get_clients({bufnr=0})>0 end, 100); \
     io.write(string.format('attached=%s after=%.1fs\n', tostring(ok), ...))" \
  "+qa"
```

Results before the fix:

| Run | Attached | Time |
|-----|----------|------|
| 1 | true | 0.1s |
| 2 | false | 45s timeout |
| 3 | false | 45s timeout |
| 4 | false | 45s timeout |

Across three separate batches the failure rate was roughly 40%.

The timing is the diagnostic result: attachment either completed in **0.1s** or never
occurred within 45 seconds. There were no intermediate values. This rules out slow
server startup, module indexing and network access, all of which would produce a
spread of times. A binary outcome indicates a race that is either won or lost at
startup.

## Cause

LSP clients are started from the `FileType` event.

When a file is passed on the command line, that event can fire before the plugin
config in `lua/plugins/lsp.lua` has finished running. If it does, the event has
already passed by the time servers are registered and enabled, and nothing retries —
the buffer keeps no client for the lifetime of the session. Reopening the file fires
`FileType` again against a now-complete configuration, which is why restarting
appeared to fix it.

A second, independent defect was visible in `:LspLog`:

```
[WARN] "bashls does not have a configuration"
[WARN] "clangd does not have a configuration"
[WARN] "lua_ls does not have a configuration"
[WARN] "rust_analyzer does not have a configuration"
```

`lua/plugins/lsp.lua` called `vim.lsp.config()` for each server but never called
`vim.lsp.enable()`, relying on mason-lspconfig to do it. mason-lspconfig is declared
as a lazy.nvim **dependency** of `nvim-lspconfig`, so it loads first and calls
`vim.lsp.enable()` for everything in `ensure_installed` before any `vim.lsp.config()`
call has run. Each such call logs the warning above.

This ordering defect did not cause the intermittent attachment — enabling explicitly
was measured separately and the failure rate did not change.

## Fix

Both changes are in `config/nvim/lua/plugins/lsp.lua`, after the server registration
loop.

Enable explicitly, so enabling does not depend on plugin load order:

```lua
vim.lsp.enable(vim.tbl_keys(servers))
```

Re-fire `FileType` for buffers that were loaded before the config ran:

```lua
for _, buf in ipairs(vim.api.nvim_list_bufs()) do
  if vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].filetype ~= "" then
    vim.api.nvim_exec_autocmds("FileType", { buffer = buf })
  end
end
```

The second change is what resolves the intermittent attachment. The first removes the
`:LspLog` warnings.

## Result

Eight consecutive headless runs on the same file, all attaching. Against a ~40%
baseline failure rate, a clean run of eight has a probability of about 1 in 60 by
chance.

Eight runs is supporting evidence rather than proof. If intermittent failures recur,
the next step is instrumenting the actual ordering — logging timestamps for when the
plugin `config()` function completes and when `FileType` fires for the buffer — rather
than further black-box repetition.

## Diagnosing this class of problem

With the affected file open:

```vim
:lua print(vim.inspect({go=vim.fn.executable('go'), gopls=vim.fn.executable('gopls'), ft=vim.bo.filetype, file=vim.api.nvim_buf_get_name(0), root=vim.fs.root(0, {'go.mod','go.work','.git'})}))
:checkhealth vim.lsp
:LspLog
```

Interpretation:

| Field | Meaning if wrong |
|-------|------------------|
| toolchain executable = 0 | Compiler not on `PATH`. With mise, the shell that launched nvim did not run `mise activate` |
| server executable = 0 | Mason `bin` directory absent from `PATH`, or a broken install — check `:Mason` |
| `ft` not the expected filetype | Filetype detection failed; no server start was ever attempted |
| `root` = nil | No project root marker above the file. gopls requires `go.mod`, `go.work` or `.git` |

If all four are correct and there is still no client, measure timing as above. A
binary attached-instantly-or-never result indicates a startup race rather than a
server fault.

Note that a syntax error in the file is not a cause. Language servers are designed to
operate on unparseable buffers and report diagnostics; a file with an unterminated
string literal attached normally during this investigation.

## See also

- [claude-lsp-integration.md](claude-lsp-integration.md) — Claude Code's separate LSP plugin system
- [bin-scripts.md](bin-scripts.md) — `lsp-doctor`, for dangling LSP processes
- `config/nvim/lua/plugins/lsp.lua` — server registration and the fix
- `config/nvim/lua/plugins/mason.lua` — `ensure_installed` list
