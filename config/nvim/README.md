# Neovim Configuration

A modular, ergonomic Neovim setup optimized for Kotlin, TypeScript, and full-stack development.

## Philosophy

This configuration follows these core principles:
- **Reduced cognitive load** - consistent patterns, minimal context switching
- **Ergonomic design** - optimized for left-handed trackpad + right-handed keyboard use
- **Modal editing** - embracing vim's composable grammar cleanly
- **Modular structure** - each plugin in its own file, clear separation of concerns
- **Reproducible setup** - version-locked plugins for consistency across machines

## Features

### Core Functionality
- **Plugin Manager**: lazy.nvim with modular plugin files
- **LSP Support**: Mason-managed language servers for 8 languages
- **File Navigation**: nvim-tree file explorer + Telescope fuzzy finder
- **Completion**: blink.cmp with LSP integration
- **Syntax**: TreeSitter for improved highlighting

### Language Support (LSP)
- TypeScript/JavaScript (`ts_ls`)
- HTML (`html`)
- Rust (`rust_analyzer`)
- C/C++ (`clangd`)
- Python (`pyright`)
- Java (`jdtls`)
- Kotlin (`kotlin_language_server`)

### Custom Features
- Green-tinted colorscheme (easy on the eyes)
- Project-aware git root detection
- IntelliJ-style keybindings for rename (Shift-F6)
- Unified LSP keybindings across all languages

## Directory Structure
```
~/.config/nvim/
├── colors/              # Custom colorschemes
│   └── green-tinted.lua
├── doc/                 # Documentation and ADRs
├── init.lua             # Main entry point
├── lazy-lock.json       # Plugin version lock (tracked)
└── lua/
    ├── config/          # Core configuration
    │   ├── autocmds.lua # Auto-commands
    │   ├── keymaps.lua  # Key mappings
    │   ├── lazy.lua     # Plugin manager setup
    │   ├── options.lua  # Vim options
    │   └── project.lua  # Git root detection
    └── plugins/         # Modular plugin configs
        ├── completion.lua
        ├── kotlin.lua
        ├── lsp.lua
        ├── mason.lua
        ├── nvim-tree.lua
        ├── telescope.lua
        ├── telescope-file-browser.lua
        ├── treesitter.lua
        └── vim-be-good.lua
```

## Key Bindings

Leader key: `Space`

### Navigation
- `gd` - Go to definition
- `gD` - Go to declaration
- `gi` - Go to implementation
- `gr` - Go to references
- `K` - Hover documentation
- `Ctrl-k` - Signature help

### Code Actions
- `<leader>rn` - Rename symbol
- `Shift-F6` - Rename symbol (IntelliJ-style)
- `<leader>ca` - Code actions
- `<leader>f` - Format document

### Diagnostics
- `[d` - Previous diagnostic
- `]d` - Next diagnostic
- `<leader>e` - Open diagnostic float

### File Navigation
- `<leader>n` - Toggle nvim-tree file explorer
- `<leader>ff` - Telescope find files (likely configured)
- `<leader>fg` - Telescope live grep (likely configured)

## Installation

### Prerequisites
- Neovim 0.11+ (uses new LSP API)
- Git
- A Nerd Font (currently using JetBrains Mono / MesloLGS)
- Node.js (for some LSP servers)
- Rust toolchain (for rust-analyzer)

### Setup

1. Clone this config to your Neovim directory:
```bash
   git clone <your-dotfiles-repo> ~/.config/nvim
```

2. Open Neovim (plugins will auto-install):
```bash
   nvim
```

3. Sync plugins:
```
   :Lazy sync
```

4. Install LSP servers via Mason:
```
   :Mason
```
   All required servers should auto-install via mason-lspconfig.

## LSP Configuration

LSP servers are managed by Mason and configured in a unified way:
- **Installation**: Automatic via `mason-lspconfig`
- **Configuration**: Centralized in `lua/plugins/lsp.lua`
- **Keybindings**: Shared across all language servers
- **Capabilities**: Enhanced with nvim-cmp completion

### Kotlin-Specific Settings
- JVM target: Java 21
- External sources enabled (including Java decompilation)
- Indexing enabled for better performance
- Snippet support enabled

See `doc/kotlin-lsp-setup.md` for detailed Kotlin LSP configuration notes.

## Customization

### Adding a New LSP Server

1. Install via Mason:
```
   :Mason
```
   Search and install the server.

2. Add to `lua/plugins/mason.lua`:
```lua
   ensure_installed = {
     -- ... existing servers
     "your_new_server",
   }
```

3. Configure in `lua/plugins/lsp.lua`:
```lua
   vim.lsp.config('your_new_server', {
     capabilities = capabilities,
     on_attach = on_attach,
   })
   
   vim.api.nvim_create_autocmd("FileType", {
     pattern = "yourfiletype",
     callback = function() vim.lsp.enable('your_new_server') end,
   })
```

### Adding a New Plugin

Create a new file in `lua/plugins/`:
```lua
-- lua/plugins/your-plugin.lua
return {
  "author/plugin-name",
  config = function()
    -- Configuration here
  end,
}
```

Restart Neovim and run `:Lazy sync`.

## Ergonomic Setup

This configuration is optimized for:
- **Left hand**: Trackpad navigation
- **Right hand**: Keyboard (stays on home row)
- **Two-handed chords**: One hand for modifiers (Control via Caps Lock), other for action keys
- **UK keyboard layout**: Custom Karabiner mappings for # and £

### Karabiner Elements Mappings
- Right Enter → Control (when held)
- Left Caps Lock → Control (when held)
- Shift+3 → # (instead of £)
- Option+3 → £ (instead of #)

## System Integration

Works alongside:
- **Terminal**: Kitty with custom green-tinted theme
- **Shell**: zsh with oh-my-zsh
- **Terminal Multiplexer**: tmux (planned)
- **Window Manager**: Aerospace (tiling)

## Maintenance

### Updating Plugins
```
:Lazy sync
```

Then commit the updated `lazy-lock.json`:
```bash
git add lazy-lock.json
git commit -m "chore: update plugin versions"
```

### Checking LSP Status
```
:LspInfo
```

### Viewing Mason Packages
```
:Mason
```

## Troubleshooting

### LSP Not Attaching
1. Check if server is installed: `:Mason`
2. Check LSP status: `:LspInfo`
3. Check for errors: `:messages`

### Plugin Issues
1. Clean and reinstall: `:Lazy clean` then `:Lazy sync`
2. Check plugin status: `:Lazy`

### Kotlin LSP Issues
See `doc/kotlin-lsp-setup.md` for Kotlin-specific troubleshooting.

## Future Enhancements

- [ ] Symbol outline view (LSP symbols tree)
- [ ] Improved nvim-tree git root detection
- [ ] Tmux integration
- [ ] Additional language servers as needed
- [ ] Code action menu improvements

## References

- [Neovim LSP Documentation](https://neovim.io/doc/user/lsp.html)
- [Mason.nvim](https://github.com/williamboman/mason.nvim)
- [lazy.nvim](https://github.com/folke/lazy.nvim)
- [ThePrimeagen's Vim Fundamentals](https://frontendmasters.com/courses/vim-fundamentals/)
