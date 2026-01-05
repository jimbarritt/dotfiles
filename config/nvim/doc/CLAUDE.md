# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is **Jim's Neovim configuration** - a modular, ergonomic Neovim setup optimized for Kotlin, TypeScript, and full-stack development. The configuration emphasizes reduced cognitive load, modal editing with vim's composable grammar, and reproducible setups across machines through version-locked plugins.

## Quick Commands

| Task | Command |
|------|---------|
| Update plugins | `:Lazy sync` then `git add lazy-lock.json && git commit -m "chore: update plugin versions"` |
| Check LSP status | `:LspInfo` |
| View/manage language servers | `:Mason` |
| Check for errors | `:messages` |
| Clean plugin cache | `:Lazy clean` |

## High-Level Architecture

### Plugin System Architecture

The configuration uses **lazy.nvim** as the plugin manager with a modular approach:

- **Main entry point** (`init.lua`): Bootstraps in this order: options → project → lazy manager → keymaps → autocmds
- **Core configuration** (`lua/config/`): 5 files handling vim options, keymaps, autocmds, lazy.nvim bootstrap, and git root detection
- **Plugin configurations** (`lua/plugins/`): 19 independent Lua files, each configuring a single plugin with clear separation of concerns
- **Version pinning** (`lazy-lock.json`): Git-tracked file ensuring reproducible plugin versions across machines

### LSP Architecture

Language servers are managed through a three-layer system:

1. **Mason** (`lua/plugins/mason.lua`): Installer and lifecycle manager that auto-installs specified servers
2. **Mason-LSPConfig**: Bridge between Mason and nvim-lspconfig
3. **nvim-lspconfig** (`lua/plugins/lsp.lua`): Unified LSP client configuration

**Key design pattern**: All 8 supported language servers (TypeScript/JavaScript, HTML, Rust, C/C++, Python, Java, Kotlin, Bash/Zsh, Lua) share a single `on_attach` function that provides consistent keybindings and disables semantic tokens to prevent LSP repainting conflicts with TreeSitter.

### Completion Pipeline

- **Trigger**: Manual (Ctrl-Space), smart dot completion, or typing
- **Engine**: blink.cmp (modern completion framework)
- **Sources** (in priority order): LSP, LuaSnip snippets, buffer, path
- **Result**: Formatted with source labels ([LSP], [Snip], [Buffer], [Path])

### Important Design Details

**Flicker Prevention**: Semantic tokens are explicitly disabled in LSP configuration (`vim.lsp.semantic_tokens = false`) to prevent LSP diagnostic repainting from conflicting with TreeSitter syntax highlighting. This was a recent focus (commits ebd35bf, 5aee819).

**Modular Plugin System**: Each plugin in `lua/plugins/` is independent. To enable/disable, simply add/remove the return statement or the entire file. The lazy.nvim configuration in `lua/config/lazy.lua` loads LazyVim base plugins plus all files in `lua/plugins/`.

**Git Root Detection** (`lua/config/project.lua`): On startup, Neovim automatically detects and changes to the git repository root, providing project-aware context.

**Colorscheme Strategy**: Primary colorscheme is `green-tinted.lua` (custom, optimized for eye comfort). Fallbacks include Catppuccin and Tokyonight.

## Directory Structure

```
lua/
├── config/                          # Core configuration
│   ├── options.lua                  # Vim settings (UI, splits, search, tabs, indentation=2)
│   ├── keymaps.lua                  # Global keybindings + Telescope mappings
│   ├── autocmds.lua                 # Auto-commands (save on focus loss, restore cursor)
│   ├── lazy.lua                     # lazy.nvim bootstrap + LazyVim setup
│   └── project.lua                  # Git root detection on startup
│
└── plugins/                         # Each file = one plugin config
    ├── lsp.lua                      # LSP setup with shared on_attach + semantic token disable
    ├── mason.lua                    # Language server auto-installer
    ├── completion.lua               # blink.cmp engine with LSP/snippet/buffer sources
    ├── treesitter.lua               # Syntax highlighting + text objects
    ├── telescope.lua                # Fuzzy finder with fzf backend
    ├── nvim-tree.lua                # File explorer
    ├── lualine.lua                  # Status line (green theme)
    ├── colorscheme.lua              # Color scheme loading
    ├── git.lua                      # gitsigns integration
    ├── snacks.nvim.lua              # Utility suite
    ├── autopairs.lua                # Auto-closing brackets
    ├── kotlin.lua                   # Kotlin language settings (JVM target: Java 21)
    ├── rainbow-delimiters.lua       # Bracket colorization
    ├── notify.lua                   # Notifications
    ├── disable-ui.lua               # UI minimization options
    ├── vim-be-good.lua              # Vim practice game
    └── [other plugins...]
```

## Key Bindings & Navigation

**Leader key**: `Space`

**LSP Navigation** (configured in lsp.lua on_attach):
- `gd` - Go to definition
- `gD` - Go to declaration
- `gi` - Go to implementation
- `gr` - Go to references
- `K` - Hover documentation
- `Ctrl-k` - Signature help

**Code Editing**:
- `<leader>rn` or `Shift-F6` - Rename symbol (IntelliJ-style)
- `<leader>ca` - Code actions
- `<leader>f` - Format document

**Diagnostics**:
- `[d` / `]d` - Previous/next diagnostic
- `<leader>e` - Open diagnostic float

**File Navigation**:
- `<leader>n` - Toggle nvim-tree file explorer
- `<leader>ff` - Find files (Telescope)
- `<leader>fg` - Live grep (Telescope)

## Language Support

### LSP Servers (managed by Mason)

| Language | Server | Auto-installed |
|----------|--------|-----------------|
| TypeScript/JavaScript | ts_ls | Yes |
| HTML | html | Yes |
| Rust | rust_analyzer | Yes |
| C/C++ | clangd | Yes |
| Python | pyright | Yes |
| Java | jdtls | Yes |
| Kotlin | kotlin_language_server | Yes |
| Bash/Zsh | bashls | Yes |
| Lua | lua_ls | Yes |

### Kotlin-Specific Configuration

Kotlin has its own plugin file (`lua/plugins/kotlin.lua`). Key settings:
- **JVM target**: Java 21
- **External sources**: Enabled (includes Java decompilation)
- **Indexing**: Enabled for performance
- **Snippets**: Enabled

Detailed Kotlin LSP setup notes are in `doc/kotlin-lsp-setup.md`.

## Adding/Modifying Plugins

### To Add a New Plugin

Create a new file in `lua/plugins/your-plugin.lua`:

```lua
return {
  "author/plugin-name",
  -- optional: lazy loading events, dependencies
  event = "VimEnter",  -- or other lazy-loading triggers
  config = function()
    -- Configuration code here
  end,
}
```

Restart Neovim and run `:Lazy sync`.

### To Add a New LSP Server

1. Install via `:Mason` (search and install the server)
2. Add to `lua/plugins/mason.lua` in the `ensure_installed` table
3. In `lua/plugins/lsp.lua`, add a new `vim.api.nvim_create_autocmd` for the file type:

```lua
vim.api.nvim_create_autocmd("FileType", {
  pattern = "yourfiletype",
  callback = function() vim.lsp.enable('your_new_server') end,
})
```

The shared `on_attach` function and `capabilities` will automatically apply to the new server.

## Recent Development Focus

Based on git history, recent work has focused on:
- **Flicker prevention** in LSP rendering (disabling semantic tokens to prevent repainting over TreeSitter)
- **Plugin ecosystem updates** (lazy.nvim and dependency management)
- **Syntax highlighting improvements**

## Prerequisites for Development

- **Neovim 0.11+** (uses new LSP API)
- **Git** (for plugin management)
- **A Nerd Font** (for icons; configured for JetBrains Mono / MesloLGS)
- **Node.js** (for TypeScript/JavaScript LSP server)
- **Rust toolchain** (for rust-analyzer)
- **macOS** (optimized for macOS; detects and warns on other systems)

## System Integration Notes

The configuration is designed for:
- **Ergonomic setup**: Left-hand trackpad + right-hand keyboard
- **Karabiner Elements integration**: Right Enter/Left Caps Lock → Control, Shift+3/Option+3 for # and £ on UK keyboard
- **Terminal**: Kitty with custom green-tinted theme
- **Shell**: zsh with oh-my-zsh
- **Window Manager**: Aerospace (tiling, planned integration)

## Plugin Count & Dependency Tree

- **45 total plugins** (tracked in lazy-lock.json)
- **~670 lines of Lua** across 19 custom configuration files
- **45 plugin files** in lua/plugins/ directory

## Troubleshooting Patterns

**LSP Not Attaching**:
1. Check `:LspInfo` to see server status
2. Verify server installed via `:Mason`
3. Check `:messages` for error output
4. For Kotlin specifically, see `doc/kotlin-lsp-setup.md`

**Plugin Issues**:
1. Run `:Lazy clean` to clean plugin cache
2. Run `:Lazy sync` to reinstall
3. Check `:Lazy` status page

**Performance Issues**:
- Check `lua/plugins/snacks.nvim.lua` for bigfile detection settings
- Verify TreeSitter parsers are installed and up-to-date
