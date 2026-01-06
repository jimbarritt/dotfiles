# Emacs-style Keybindings in Insert Mode

## Overview

This Neovim configuration includes Emacs-style keybindings in insert mode. These bindings are intentional customizations that improve efficiency for users with Emacs muscle memory or those who work frequently in Bash/readline environments.

## Keybindings

### Line Navigation

| Keybinding | Action | Emacs Equivalent |
|-----------|--------|-----------------|
| `CTRL+A` | Jump to start of text (after indentation) | `C-a` |
| `CTRL+E` | Jump to end of line | `C-e` |

### Character Navigation

| Keybinding | Action | Vim Equivalent |
|-----------|--------|----------------|
| `CTRL+H` | Move cursor left | `<Left>` |
| `CTRL+J` | Move cursor down | `<Down>` |
| `CTRL+K` | Move cursor up | `<Up>` |
| `CTRL+L` | Move cursor right | `<Right>` |

### Mode Navigation

| Keybinding | Action |
|-----------|--------|
| `CTRL+G` | Exit insert/visual/command mode (like Emacs cancel) |

## Implementation Details

**File:** `lua/config/keymaps.lua`

```lua
-- Emacs-style line navigation in insert mode
vim.keymap.set('i', '<C-a>', '<C-o>^', { noremap = true, silent = true, desc = 'Start of text' })
vim.keymap.set('i', '<C-e>', '<End>', { noremap = true, silent = true, desc = 'End of line' })

-- Insert mode navigation with Ctrl+hjkl
vim.keymap.set('i', '<C-h>', '<Left>', { noremap = true, silent = true })
vim.keymap.set('i', '<C-j>', '<Down>', { noremap = true, silent = true })
vim.keymap.set('i', '<C-k>', '<Up>', { noremap = true, silent = true })
vim.keymap.set('i', '<C-l>', '<Right>', { noremap = true, silent = true })
```

**Key Implementation Notes:**

- `CTRL+A` uses `<C-o>^` which executes the normal mode command `^` (first non-whitespace character) from insert mode
- This keeps you in insert mode while navigating, reducing mode switching
- Overrides Neovim's default `CTRL+E` (scroll down) since navigation is more useful in insert mode

## Philosophy: Not "Against the Vim Way"

There's a common misconception that customizing keybindings violates Vim philosophy. This is false.

### Evidence

1. **Vim's Official Documentation** - Vim itself recommends Emacs-style keybindings in command mode (`:` mode), acknowledging that users have Emacs muscle memory from Bash/readline

2. **Community Practice** - Multiple plugins exist specifically for Emacs keybindings:
   - `vim-emacs-bindings`
   - `nvimacs` (Neovim-specific)
   - `Vimacs` (long-standing Vim plugin)

3. **ThePrimeagen's Philosophy** - The popular Vim educator emphasizes:
   - Personal optimization over purity
   - Consistency with other tools matters
   - Pragmatism is more important than ideology

### The Trade-off

There are two legitimate values in the Vim community:

| Value | Focus | Approach |
|-------|-------|----------|
| **Portability** | Works on any system, consistency across machines | Stick to default Vim keybindings |
| **Personal Productivity** | Optimized for your workflow, reduced cognitive load | Selective customization for pain points |

This configuration prioritizes **personal productivity** while maintaining the core Vim modal editing philosophy.

## Best Practices

1. **Master the defaults first** - Understand standard Vim keybindings before customizing
2. **Customize selectively** - Only add bindings for genuine friction points
3. **Consider your context** - If you work on many remote systems, prioritize portability
4. **Maintain consistency** - These bindings align with Bash/readline, creating muscle memory consistency

## When These Bindings Shine

- You frequently use Bash/readline (which defaults to Emacs keybindings)
- You switch between Bash and Neovim frequently
- You want to minimize mode switching while typing
- You have deep Emacs muscle memory you want to preserve

## When to Reconsider

- You work primarily on remote systems with standard Vim
- You're learning Vim and want to build pure Vim muscle memory
- You collaborate frequently on systems without your dotfiles
- Portability is your highest priority

## References

- [vim-emacs-bindings](https://github.com/maxbrunsfeld/vim-emacs-bindings)
- [Vim recommends Emacs-style keybindings in command mode](https://til.codeinthehole.com/posts/vim-recommends-using-emacsstyle-key-bindings-in-command-mode/)
- [ThePrimeagen's Vim Philosophy](https://theprimeagen.github.io/vim-fundamentals/)
