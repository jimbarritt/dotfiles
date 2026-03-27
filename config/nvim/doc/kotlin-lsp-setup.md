# Kotlin LSP Setup for Neovim

## Overview

This config uses the **JetBrains official Kotlin LSP** (kotlin-lsp) via the
[kotlin.nvim](https://github.com/AlexandrosAlexiou/kotlin.nvim) plugin. This
replaced the community `kotlin-language-server` (fwcd) which is now deprecated.

The JetBrains LSP provides faster completions, inlay hints, better formatting,
and improved diagnostics compared to the community server.

> **Status**: Pre-alpha / alpha. Best suited for Gradle-based JVM Kotlin
> projects. Large projects may have slow initial indexing.

## Installation

The LSP server is installed automatically via Mason. On first launch:

1. Mason will install `kotlin-lsp` (includes bundled JRE)
2. The `kotlin.nvim` plugin configures the LSP client
3. Open a `.kt` file and check `:LspInfo` to verify

### Manual install (alternative)

```bash
# Download from https://github.com/Kotlin/kotlin-lsp/releases
# Set environment variable to point to install directory:
export KOTLIN_LSP_DIR=/path/to/kotlin-lsp
```

## Key Bindings

### Navigation
- `gd` - Go to definition
- `gI` - Go to implementation
- `gr` - Find references

### Documentation
- `K` - Show hover documentation

### Code Actions
- `<leader>rn` - Rename symbol
- `<leader>ca` - Code actions
- `<leader>f` - Format file

### Kotlin-specific (kotlin.nvim)
- `:KotlinOrganizeImports` - Organize imports
- `:KotlinFormat` - Format with Kotlin LSP

### Diagnostics
- `[d` - Previous diagnostic
- `]d` - Next diagnostic
- `<leader>e` - Open diagnostic float

### LSP Management
- `<leader>lr` - Restart LSP (useful when external tools modify files)

### Completion
- `<C-Space>` - Trigger completion
- `<Tab>` - Next completion item / expand snippet
- `<S-Tab>` - Previous completion item
- `<CR>` - Confirm completion

## Live Reload with External Tools (e.g. Claude)

When using nvim as a code viewer alongside AI coding assistants, the config
handles external file changes through two mechanisms:

### 1. Buffer auto-reload (file contents)

- `autoread` is enabled
- `updatetime` is set to 300ms
- `checktime` fires on `FocusGained`, `BufEnter`, `CursorHold`, and
  `CursorHoldI` events
- This means buffers refresh within ~300ms of cursor inactivity, even without
  switching focus

### 2. LSP re-indexing (diagnostics and analysis)

- `workspace/didChangeWatchedFiles` dynamic registration is enabled in LSP
  capabilities
- The LSP server receives filesystem change notifications and re-indexes
  automatically
- If diagnostics become stale, press `<leader>lr` to restart the LSP

## Troubleshooting

### LSP not starting

```vim
:LspInfo          " Check if kotlin-lsp is attached
:Mason            " Verify kotlin-lsp is installed
:messages         " Check for errors
```

### Stale diagnostics after external changes

1. Wait a moment — `didChangeWatchedFiles` should trigger re-indexing
2. Move cursor to trigger `CursorHold` → `checktime`
3. Press `<leader>lr` to restart the LSP as a last resort

### No syntax highlighting

Ensure Treesitter Kotlin parser is installed:
```
:TSInstall kotlin
```

### Slow startup on large projects

The JetBrains LSP tokenizes the entire project on first load. This can take
a few minutes for large codebases. Subsequent opens are faster due to caching.

## Features

- Syntax highlighting (Treesitter)
- Auto-completion (via blink.cmp)
- Go to definition / implementation / references
- Hover documentation
- Rename refactoring
- Code actions
- Diagnostics (errors/warnings)
- Formatting
- Import organization (kotlin.nvim)
- Inlay hints (JetBrains LSP)
