# Debugging: Restore Cursor Position on Launch

## Problem Statement

Cursor position was not being restored when launching Neovim from the command line with a file argument.

**Observed Behavior:**
- Opening a file within a Neovim session and reopening it: ✅ Cursor position restored correctly
- Closing Neovim and launching with `nvim myfile.lua` from terminal: ❌ Always goes to line 1

## Root Cause Analysis

The issue was a **race condition with ShaDa file loading**.

### Understanding ShaDa

ShaDa ("Shared Data") is Neovim's persistent storage for editor state across sessions:
- **Location:** `~/.local/state/nvim/shada/main.shada`
- **Stores:** Marks (including `"` - the cursor position mark), history, registers, etc.
- **Format:** Binary MessagePack format

The `"` (double-quote) mark is Neovim's automatic cursor position tracking:
- When you exit Neovim, the current cursor position is saved to the `"` mark in ShaDa
- When you reopen a file, Neovim should restore to that mark

### The Race Condition

The problem was in the `VimEnter` autocommand timing:

```
Timeline:
1. nvim launches with file
2. VimEnter fires IMMEDIATELY
3. We try to read the '"' mark
4. Mark shows line 1 (default, not loaded yet!)
5. Cursor restored to line 1
6. LATER: ShaDa file loads from disk (too late!)
```

**Proof from debugging logs:**
```
23:39:53 VimLeave: mark_line=50     <- Mark saved correctly as 50
23:40:18 VimEnter: mark: line=1     <- But reads as 1 at startup
                                       ^ ShaDa not loaded yet!
```

## Debugging Technique: Custom Log File

Since `print()` statements are lost on exit, we used a custom logging function to write to a file:

```lua
local function debug_log(msg)
  vim.fn.writefile({ os.date("%H:%M:%S") .. " " .. msg }, "/tmp/nvim-cursor-debug.log", "a")
end
```

**Why this works:**
- Writes to disk immediately (survives Neovim exit)
- Includes timestamps for tracking sequence of events
- Append mode (`"a"`) builds a log over multiple sessions
- Can be tailed in real-time from another terminal

**Usage:**
```bash
# Terminal 1: Clear old log and launch Neovim
rm /tmp/nvim-cursor-debug.log
nvim myfile.lua

# Terminal 2: Watch the log in real-time
tail -f /tmp/nvim-cursor-debug.log
```

## Solution: Force Load ShaDa

The fix was to explicitly load the ShaDa file before trying to read marks:

```lua
vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    -- Force read ShaDa to load marks BEFORE trying to restore
    vim.cmd("silent! rshada!")

    if vim.bo.buftype == "" then
      vim.defer_fn(function()
        local mark = vim.api.nvim_buf_get_mark(0, '"')
        local lcount = vim.api.nvim_buf_line_count(0)
        if mark[1] > 0 and mark[1] <= lcount then
          pcall(vim.api.nvim_win_set_cursor, 0, mark)
        end
      end, 0)
    end
  end,
})
```

**Key points:**
- `rshada!` forces immediate ShaDa load (the `!` is important - it forces)
- `silent!` suppresses any messages
- Placed BEFORE the deferred cursor restoration
- `vim.defer_fn` gives buffer time to initialize

**After fix, logs show correct behavior:**
```
23:43:07 VimEnter: mark: line=31     <- ✅ Correctly reads line 31
23:43:07 VimEnter: Restoring to line 31
```

## Key Learnings

### 1. VimEnter vs BufReadPost
- **BufReadPost** - Fires when opening files within a session (reliable)
- **VimEnter** - Fires on startup, but ShaDa may not be loaded yet (timing issue)

### 2. ShaDa Timing
- ShaDa loads automatically during startup, but timing is unpredictable
- For guaranteed access, explicitly call `rshada!` first

### 3. Debug Techniques for Neovim
When `print()` isn't sufficient:

**Option 1: Log to file (recommended for startup code)**
```lua
vim.fn.writefile({"message"}, "/tmp/debug.log", "a")
```

**Option 2: Use notification (works in VimEnter)**
```lua
vim.notify("Debug message", vim.log.levels.INFO)
```

**Option 3: Tail nvim logs**
```bash
tail -f ~/.local/state/nvim/log/nvim.log
```

## Complete Solution

**File:** `lua/config/autocmds.lua`

```lua
vim.api.nvim_create_autocmd("FocusLost", {
  pattern = "*",
  command = "silent! wa",
})

-- Restore cursor position when opening files within a session
vim.api.nvim_create_autocmd("BufReadPost", {
  pattern = "*",
  callback = function()
    local ft = vim.bo.filetype
    if vim.tbl_contains({ "gitcommit", "gitrebase", "svn", "mail" }, ft) then
      return
    end

    if vim.bo.buftype ~= "" then
      return
    end

    vim.defer_fn(function()
      local mark = vim.api.nvim_buf_get_mark(0, '"')
      local lcount = vim.api.nvim_buf_line_count(0)
      if mark[1] > 0 and mark[1] <= lcount then
        pcall(vim.api.nvim_win_set_cursor, 0, mark)
      end
    end, 0)
  end,
})

-- Restore cursor position when launching nvim from command line
vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    -- Force read ShaDa to load marks BEFORE trying to restore
    vim.cmd("silent! rshada!")

    if vim.bo.buftype == "" then
      vim.defer_fn(function()
        local mark = vim.api.nvim_buf_get_mark(0, '"')
        local lcount = vim.api.nvim_buf_line_count(0)
        if mark[1] > 0 and mark[1] <= lcount then
          pcall(vim.api.nvim_win_set_cursor, 0, mark)
        end
      end, 0)
    end
  end,
})
```

## Testing

Verify the fix works:

```bash
# Open a file and jump to line 50
nvim myfile.lua
50G
:q

# Reopen - cursor should be at line 50
nvim myfile.lua
```

The cursor should jump directly to line 50.

## References

- [Neovim ShaDa Documentation](https://neovim.io/doc/user/starting.html#shada)
- [Neovim Marks Documentation](https://neovim.io/doc/user/motion.html#marks)
- [Neovim Autocmd Documentation](https://neovim.io/doc/user/autocmd.html)

## Related Issues

This bug is related to:
- [Neovim Issue #16339](https://github.com/neovim/neovim/issues/16339) - Restore cursor position on startup
- [Neovim Issue #19509](https://github.com/neovim/neovim/pull/19509) - Timing fixes for VimEnter

The `rshada!` workaround is a practical solution for users on older Neovim versions.
