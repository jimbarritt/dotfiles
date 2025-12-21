# Kotlin LSP Setup for Neovim

## Installation Steps

### 1. Install Kotlin Language Server

**macOS (using Homebrew):**
```bash
brew install kotlin-language-server
```

**Manual Installation:**
```bash
# Download from GitHub releases
# https://github.com/fwcd/kotlin-language-server/releases

# Or build from source
git clone https://github.com/fwcd/kotlin-language-server
cd kotlin-language-server
./gradlew :server:installDist

# Add to PATH
export PATH="$PATH:$(pwd)/server/build/install/server/bin"
```

### 2. Restart Neovim

```bash
nvim
```

The first time you open Neovim after this setup, lazy.nvim will automatically install all the plugins.

### 3. Verify Installation

Open a Kotlin file:
```bash
cd /Users/jmdb/Code/github/jimbarritt/article-endianness/kotlin-client
nvim src/main/kotlin/Client.kt
```

Check LSP status:
```
:LspInfo
```

You should see `kotlin_language_server` attached to your buffer.

## Key Bindings

### Navigation
- `gd` - Go to definition
- `gD` - Go to declaration
- `gi` - Go to implementation
- `gr` - Find references

### Documentation
- `K` - Show hover documentation
- `<C-k>` - Show signature help

### Code Actions
- `<leader>rn` - Rename symbol
- `<leader>ca` - Code actions
- `<leader>f` - Format file

### Diagnostics
- `[d` - Previous diagnostic
- `]d` - Next diagnostic
- `<leader>e` - Open diagnostic float
- `<leader>q` - Add diagnostics to location list

### Completion
- `<C-Space>` - Trigger completion
- `<Tab>` - Next completion item / expand snippet
- `<S-Tab>` - Previous completion item
- `<CR>` - Confirm completion

## File Structure

```
~/.config/nvim/
├── init.lua                         # Main config
├── lua/
│   └── plugins/
│       ├── treesitter.lua          # Syntax highlighting
│       ├── lsp.lua                 # Base LSP config
│       ├── kotlin-lsp.lua          # Kotlin LSP specific
│       └── completion.lua          # nvim-cmp completion
└── KOTLIN_LSP_SETUP.md             # This file
```

## Troubleshooting

### LSP not starting

Check if kotlin-language-server is in your PATH:
```bash
which kotlin-language-server
```

### No syntax highlighting

Ensure Treesitter Kotlin parser is installed:
```
:TSInstall kotlin
```

### Completion not working

Check if nvim-cmp is loaded:
```
:Lazy
```

Look for `nvim-cmp` in the list.

### Cannot navigate to library sources (JDK, dependencies)

The kotlin-language-server has limited support for navigating to decompiled sources in JAR files. While sources are downloaded to `~/.gradle/caches/`, the LSP may not index them properly.

**Workaround options:**

1. **Use hover documentation (K)** - Shows method signatures and documentation without navigating to source
2. **Check dependency sources manually:**
   ```bash
   # Find and extract source jar
   find ~/.gradle/caches -name "truth-*-sources.jar"
   # Extract to temp location
   unzip /path/to/truth-1.1.5-sources.jar -d /tmp/truth-sources
   # Open in Neovim
   nvim /tmp/truth-sources/com/google/common/truth/Truth.java
   ```
3. **Use IntelliJ IDEA** - For deep source navigation, IntelliJ has better support for library sources

This is a known limitation of kotlin-language-server. For project files (your own Kotlin code), `gd` works correctly.

## Features

- ✅ Syntax highlighting (Treesitter)
- ✅ Auto-completion
- ✅ Go to definition
- ✅ Find references
- ✅ Hover documentation
- ✅ Rename refactoring
- ✅ Code actions
- ✅ Diagnostics (errors/warnings)
- ✅ Formatting
