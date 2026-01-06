# Technical Implementation of Folding and Statuscolumn

## Overview

This document describes the implementation of code folding and statuscolumn configuration in this Neovim setup. The solution provides intelligent code folding across multiple file types (code, markdown, JSON) with a clean visual interface showing git status and fold indicators.

## Problem Statement

The initial setup used nvim-ufo for folding, but it had limitations:
- UFO had complex keybindings that remained even after disabling the plugin
- Folding wasn't working properly in markdown and JSON files
- Git signs (from gitsigns.nvim) weren't displaying correctly in the statuscolumn
- snacks.nvim statuscolumn had conflicts with gitsigns, causing signs to disappear or display incorrectly

## Solution Architecture

### Core Components

1. **Treesitter Folding (via nvim-ufo)**
   - Provider selector: `{ "treesitter", "indent" }`
   - Treesitter provides code-aware folding for structured code
   - Indent fallback for markdown, JSON, and other unstructured formats

2. **Custom Statuscolumn**
   - Replaces default statuscolumn with a custom Lua implementation
   - Shows three columns: git signs, line numbers, fold indicators
   - Mouse click support for toggling folds

3. **Gitsigns Integration**
   - Displays git diff indicators (▎) for added, changed, and deleted lines
   - Automatically detected in the statuscolumn sign area

## Implementation Details

### Folding Configuration

**File:** `lua/plugins/ufo.lua`

```lua
return {
  "kevinhwang91/nvim-ufo",
  dependencies = {
    "kevinhwang91/promise-async",
  },
  config = function()
    vim.o.foldlevel = 99
    vim.o.foldlevelstart = 99
    vim.o.foldenable = true

    require("ufo").setup({
      provider_selector = function(bufnr, filetype, buftype)
        return { "treesitter", "indent" }
      end,
      open_fold_hl_timeout = 0,
      enable_get_fold_virt_text = false,
    })

    vim.keymap.set("n", "zR", require("ufo").openAllFolds)
    vim.keymap.set("n", "zM", require("ufo").closeAllFolds)
    vim.keymap.set("n", "zr", require("ufo").openFolds)
    vim.keymap.set("n", "zm", require("ufo").closeFolds)
  end,
}
```

**Key Settings:**
- `foldlevel = 99` - Keeps all folds open by default
- `foldenable = true` - Enables fold display
- Provider selector uses treesitter for code, falls back to indent for markdown/JSON
- Standard vim fold keybindings (zc, zo, zj, zk, zM, zR, etc.) work automatically

### Statuscolumn Configuration

**File:** `lua/config/options.lua`

```lua
local function get_statuscolumn()
  local col = table.concat({
    "%s",  -- Signs (gitsigns will display here)
    "%=",  -- Right align everything after this
    "%l",  -- Line number
    "%=",  -- Right align fold indicators
    "%{foldclosed(v:lnum) >= 0 ? ' ›' : ' '}",  -- Fold indicator only when fold is closed
    " ",   -- Spacing
  })
  -- Wrap with clickable region for fold toggling
  return "%@v:lua.require'config.statuscolumn'.click_fold@" .. col .. "%T"
end

vim.opt.statuscolumn = get_statuscolumn()
```

**Components:**
1. `%s` - Sign column (where gitsigns displays git status)
2. `%=` - Right-align separator
3. `%l` - Line number
4. `%=` - Right-align separator
5. Fold indicator: `›` when fold is closed, empty when open
6. `%@...%T` - Makes the entire statuscolumn clickable for fold toggling

### Mouse Click Fold Toggling

**File:** `lua/config/options.lua`

The statuscolumn is wrapped with a click handler that:
1. Detects which line was clicked using `vim.fn.getmousepos()`
2. Checks if that line has a foldable region using `vim.fn.foldlevel()`
3. Toggles the fold using the standard `za` command
4. Preserves the cursor position (with known limitation - see below)

```lua
local statuscolumn = {}
function statuscolumn.click_fold()
  -- Capture cursor position early, before getmousepos affects it
  local cur_win = vim.api.nvim_get_current_win()
  local cursor = vim.api.nvim_win_get_cursor(cur_win)

  local pos = vim.fn.getmousepos()
  if vim.fn.foldlevel(pos.line) > 0 then
    vim.api.nvim_win_call(pos.winid, function()
      -- Toggle fold on the clicked line
      vim.fn.execute(pos.line .. "normal! za", "silent")
    end)

    -- Restore cursor position AFTER fold toggle
    vim.api.nvim_win_set_cursor(cur_win, cursor)
  end
end

package.loaded['config.statuscolumn'] = statuscolumn
```

### Git Status Configuration

**File:** `lua/plugins/git.lua`

```lua
return {
  {
    "lewis6991/gitsigns.nvim",
    opts = {
      signs = {
        add          = { text = '▎' },
        change       = { text = '▎' },
        delete       = { text = '▎' },
        topdelete    = { text = '▎' },
        changedelete = { text = '▎' },
      },
      sign_priority = 6,
      numhl = false,
      linehl = false,
      attach_to_untracked = false,
    },
  },
}
```

All git signs use the same character (▎) for visual consistency.

### Snacks Configuration

**File:** `lua/plugins/snacks.lua`

Snacks statuscolumn is disabled to avoid conflicts with gitsigns:

```lua
return {
  "folke/snacks.nvim",
  opts = {
    statuscolumn = {
      enabled = false,
    },
  },
}
```

The custom statuscolumn provides all necessary functionality without the plugin conflicts.

## Visual Layout

```
[Git Sign] [Line Number]                 [› if fold is closed]
   ▎             42                       ›
```

- **Left column:** Git status indicators from gitsigns
- **Middle:** Line numbers (right-aligned from center)
- **Right column:** Fold indicators (› for closed folds, empty for open)

## Folding Behavior by File Type

### Code Files (Lua, Python, Java, etc.)
- Uses treesitter for fold detection
- Folds on: functions, classes, blocks, conditionals
- Intelligent hierarchy-aware folding

### Markdown Files
- Falls back to indent-based folding (treesitter folds only on headers/code blocks)
- Allows folding on nested indentation levels
- Works with lists, code blocks, paragraphs

### JSON Files
- Falls back to indent-based folding
- Folds on object and array nesting levels

## Keyboard Shortcuts

Standard Vim folding commands work throughout:

- `zc` - Close fold at cursor
- `zo` - Open fold at cursor
- `za` - Toggle fold at cursor
- `zC` - Close all folds in current block
- `zO` - Open all folds in current block
- `zM` - Close all folds in buffer
- `zR` - Open all folds in buffer
- `zj` - Move to next fold
- `zk` - Move to previous fold

## Known Limitations

### Cursor Position on Fold Click
When clicking the fold indicator to toggle a fold, the cursor position may shift slightly (typically one character). This is a minor visual glitch that doesn't affect functionality. The cursor returns to its original line but may not preserve the exact column position.

**Workaround:** Use keyboard shortcuts (zc/zo) for precise fold toggling without cursor movement.

### Line Offset in Git Signs
Git signs (added/deleted lines) show a one-line offset from the actual edit for new lines and deleted lines. This is a known issue with how gitsigns calculates line numbers from git diff output. Modified lines show correctly.

**Impact:** Minimal - the signs update to correct position when you type or save.

## Why This Approach

### Why Not snacks.nvim Statuscolumn?
- snacks statuscolumn has conflicts with gitsigns when trying to display both simultaneously
- Custom statuscolumn gives full control without plugin conflicts
- Simpler to debug and customize

### Why UFO Instead of Just Treesitter?
- Treesitter folding only works well for structured code
- UFO's provider selector allows intelligent fallback to indent-based folding
- UFO provides better visual presentation of fold state
- Standard vim fold keybindings work automatically with UFO

### Why Indent Fallback?
- Markdown and JSON don't have complex AST structures like code
- Indent-based folding is intuitive for these formats
- Treesitter for markdown only recognizes headers and code blocks (limiting)

## Testing

To verify the configuration works:

1. **Code folding:** Open any code file and use `zc` to close a function/class
   - Should close the entire block
   - Indicator `›` appears on the fold line

2. **Markdown folding:** Open a markdown file and try `zc` on different indentation levels
   - Should fold based on indentation
   - Works with lists, paragraphs, etc.

3. **Click toggling:** Click the `›` indicator to toggle folds
   - Should close/open the fold on that line
   - Works across all file types

4. **Git status:** Edit a file and check git signs
   - `▎` appears for added/changed/deleted lines
   - Signs appear in the left column

## Future Improvements

- [ ] Investigate and fix cursor position shift on fold click
- [ ] Consider custom fold text display (currently uses UFO defaults)
- [ ] Add fold level indicators (show current nesting level)
- [ ] Implement fold preview on hover (requires additional plugin)

## References

- [nvim-ufo GitHub](https://github.com/kevinhwang91/nvim-ufo)
- [Neovim Folding Documentation](https://neovim.io/doc/user/fold.html)
- [Treesitter Folding Research](https://www.jmaguire.tech/posts/treesitter_folding/)
- [gitsigns.nvim GitHub](https://github.com/lewis6991/gitsigns.nvim)
- [snacks.nvim GitHub](https://github.com/folke/snacks.nvim)
