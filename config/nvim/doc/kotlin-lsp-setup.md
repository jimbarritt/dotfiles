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

kotlin-lsp is installed via Homebrew (not Mason, because JetBrains CDN blocks
Mason downloads). The nvim plugin handles detection automatically.

### First time on a new machine

1. Install kotlin-lsp (cask is preferred — the formula from the JetBrains tap
   is outdated):
   ```bash
   brew install --cask kotlin-lsp
   xattr -r -d com.apple.quarantine /opt/homebrew/Caskroom/kotlin-lsp
   ```
2. Open a `.kt` file in nvim
3. The plugin auto-detects the installation (checks cask, then formula, then Mason)
4. It writes `export KOTLIN_LSP_DIR="..."` to `~/.zshrc_machine`
5. It sets the env var in the current nvim session so the LSP starts immediately
6. Reload your shell so future terminals pick up the env var

### What happens on subsequent launches

- `~/.zshrc_machine` is sourced by `.zshrc`, so `KOTLIN_LSP_DIR` is set before nvim starts
- The plugin reads the env var directly — fast path, no brew calls
- If the env var is missing but `~/.zshrc_machine` already has the export (e.g. tmux
  inherited an old environment), the plugin reads the value from the file and sets
  it for the session, with a reminder to reload your shell

### Detection flow (in `lua/plugins/kotlin.lua`)

```
KOTLIN_LSP_DIR env var set and valid?
  → yes: use it (fast path)
  → no: Mason dir exists?
    → yes: use Mason
    → no: Homebrew cask installed? (/opt/homebrew/Caskroom/kotlin-lsp/<ver>/)
      → yes: use latest version dir
      → no: Homebrew formula installed? (brew --prefix → libexec/)
        → yes: use formula libexec
        → no: ~/.zshrc_machine already has the export?
          → yes: use saved value, set env var, remind to reload shell
          → no: show popup with install instructions
Auto-write to ~/.zshrc_machine if detected and not already saved.
```

### Error detection

30 seconds after the plugin loads, it checks whether a `kotlin_ls` client is
running. If not, it runs a full diagnostic and shows a popup with:
- The resolved directory and binary status
- A path to a detailed diagnostic log file at `~/.local/state/nvim/kotlin-lsp-diag.log`
- A pointer to `:LspLog` for LSP-specific errors

The diagnostic log includes:
- Environment variables (`KOTLIN_LSP_DIR`, `JAVA_HOME`, `PATH`)
- Homebrew formula and cask installation status
- Binary path resolution and version
- JRE detection
- `~/.zshrc_machine` contents
- LSP log tail (last 10 lines)
- Machine info (hostname, OS)

### Machine-specific config (`~/.zshrc_machine`)

`~/.zshrc_machine` is sourced by `.zshrc` for machine-specific exports that vary
per machine (e.g. Homebrew paths). It is not tracked in git. The kotlin.lua plugin
auto-populates it on first detection. `~/.zshrc_work` is sourced after it for
work-specific config.

## Key Bindings

### Navigation
- `gd` - Go to definition
- `gI` - Go to implementation
- `gr` - Find references

### Documentation
- `K` - Show hover documentation

### Code Actions
- `<leader>rn` or `grn` - Rename symbol
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

## Migrating from formula to cask

The JetBrains Homebrew tap formula (`jetbrains/utils/kotlin-lsp`) is outdated.
The official Homebrew cask is newer and preferred. The plugin will show a popup
with these steps if it detects the formula.

```bash
# 1. Remove the old formula
brew uninstall jetbrains/utils/kotlin-lsp

# 2. Install the cask
brew install --cask kotlin-lsp

# 3. Strip macOS quarantine flags (blocks unsigned native libs like libfilewatcher_jni.dylib)
xattr -r -d com.apple.quarantine /opt/homebrew/Caskroom/kotlin-lsp

# 4. Remove the stale KOTLIN_LSP_DIR line from ~/.zshrc_machine
#    (the plugin will auto-detect and re-write it on next launch)

# 5. Clear the env var from the current shell and reload
unset KOTLIN_LSP_DIR && source ~/.zshrc

# 6. Restart nvim
```

**Why each step matters:**
- Step 3: The cask bundles unsigned native libraries (Rust JNI code). macOS
  quarantines everything Homebrew downloads, and Gatekeeper blocks unsigned
  `.dylib` files. `xattr` removes the quarantine flag. Note: Homebrew 5.0
  removed `--no-quarantine`, so this post-install step is required.
- Steps 4-5: The old `KOTLIN_LSP_DIR` points to the formula path which no
  longer exists. If nvim inherits the stale env var, kotlin.nvim will fail
  to find the lib directory and silently not start the LSP. You must both
  remove it from `~/.zshrc_machine` AND unset it in the current shell.
- Step 6: nvim must be restarted from a shell that has the correct (or no)
  `KOTLIN_LSP_DIR`. The plugin will auto-detect the cask and set the env var.

**Layout differences:**
- Formula installs to `/opt/homebrew/Cellar/kotlin-lsp/<ver>/libexec/`
- Cask installs to `/opt/homebrew/Caskroom/kotlin-lsp/<ver>/`
- Both create `/opt/homebrew/bin/kotlin-lsp` — they conflict, so uninstall one first

The plugin handles both layouts automatically.

## Troubleshooting

### LSP not starting

```vim
:LspInfo          " Check if kotlin-lsp is attached
:LspLog           " Check LSP log for errors
:messages         " Check for startup errors
```

The plugin will show a diagnostic popup after 30 seconds if the LSP fails to
start (the Kotlin LSP is a full IntelliJ platform and can be slow to initialise).
A detailed diagnostic log is written to:

```
~/.local/state/nvim/kotlin-lsp-diag.log
```

Check the popup for the log path and `:LspLog` for LSP-specific errors.

### macOS blocks native libraries (quarantine)

If the LSP crashes or fails to load `libfilewatcher_jni.dylib`, macOS
Gatekeeper is blocking unsigned native code. Strip the quarantine flag:

```bash
xattr -r -d com.apple.quarantine /opt/homebrew/Caskroom/kotlin-lsp
```

This is needed after every `brew install --cask kotlin-lsp` or `brew upgrade`.
Homebrew 5.0 removed `--no-quarantine`, so this is the only workaround.
JetBrains needs to notarise the native binaries before September 2026 or the
cask will be removed from Homebrew.

### Stale KOTLIN_LSP_DIR after migration

If the LSP silently fails to start after migrating from formula to cask,
check that `KOTLIN_LSP_DIR` isn't pointing to the old formula path:

```bash
echo $KOTLIN_LSP_DIR
# Should be empty or point to /opt/homebrew/Caskroom/kotlin-lsp/<ver>/
# NOT /opt/homebrew/opt/kotlin-lsp/libexec
```

If stale, remove it from `~/.zshrc_machine`, run `unset KOTLIN_LSP_DIR &&
source ~/.zshrc`, and restart nvim. See "Migrating from formula to cask" above.

### Known harmless warnings in LspLog

```
WARNING: package sun.awt.windows not in java.desktop
WARNING: package sun.awt.X11 not in java.desktop
WARNING: package com.sun.java.swing.plaf.gtk not in java.desktop
```

These are Java stderr warnings about GUI packages and can be safely ignored.
The LSP starts and works correctly despite them.

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

### Go-to-definition jumps to top of file

Known kotlin-lsp limitation: when the target file hasn't been opened/indexed
yet, go-to-definition may jump to line 0. It works correctly once the file
has been opened at least once.

### Rename gives "Buffer newer than edits" error

A custom `textDocument/rename` handler in `lsp.lua` strips stale
`textDocument.version` from rename responses to work around this kotlin-lsp bug.

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
