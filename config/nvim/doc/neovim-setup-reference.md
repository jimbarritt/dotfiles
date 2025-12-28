# Neovim Setup Reference

## Core Philosophy

- Minimal visual noise
- Vim-native workflows
- Reduced cognitive load
- Terminal-centric development
- Green-tinted theme matching Kitty terminal

## Plugins

### nvim-tree (File Explorer)

**Purpose**: File tree navigation with clean, minimal aesthetic

**Features**:
- No icons, pure text
- Green color scheme
- Clean tree lines (│ ├ └)
- Relative line numbers
- Directory hijacking (opens tree when `nvim ~`)
- Hidden files toggleable
- Focus stays in tree when opening files

**Keybindings**:
- `<leader>e` - Toggle nvim-tree
- `l` - Open file or expand directory
- `o` - Open file or expand directory
- `<Enter>` - Open file or expand directory
- `h` - Close directory / navigate to parent
- `H` - Toggle hidden files
- `q` - Close nvim-tree
- `d` - Delete file/directory
- `r` - Rename file/directory
- `a` - Create new file/directory (add `/` at end for directory)
- `m` - Mark for copy/cut operations
- Standard vim navigation: `j/k`, relative numbers for jumps

**Configuration Highlights**:
- Width: 35 columns
- Symlink arrow: ` -> `
- Indent width: 2 spaces
- Colors: Green (#50fa7b) with light green accents (#7fc87f)
- Filters: Hidden files by default, excludes Library/Calendars

### Telescope (Fuzzy Finder)

**Purpose**: Fast file and content searching

**Features**:
- Searches from git root (or cwd if not in git repo)
- Uses ripgrep for live grep
- Clean, fast fuzzy finding
- Preview window for files

**Keybindings**:
- `<leader>ff` - Find files (from git root)
- `<leader>fg` - Live grep / search in files (from git root)
- `<leader>fb` - Find open buffers
- `<leader>fr` - Recent files
- `<leader>fh` - Help tags

**Inside Telescope** (Insert Mode):
- `Ctrl-n` / `Ctrl-j` / `Down` - Next result
- `Ctrl-p` / `Ctrl-k` / `Up` - Previous result
- `Ctrl-u` - Scroll preview up
- `Ctrl-d` - Scroll preview down
- `Enter` - Open file
- `Ctrl-x` - Open in horizontal split
- `Ctrl-v` - Open in vertical split
- `Ctrl-t` - Open in new tab
- `Ctrl-g` / `Esc` - Close Telescope

**Inside Telescope** (Normal Mode - after pressing Esc):
- `j` / `k` - Move down/up
- `gg` - First result
- `G` - Last result
- `i` - Back to insert mode
- `q` - Close

## Standard Vim Keybindings (Discussed)

### Window Management

**Navigation**:
- `Ctrl-w h` - Move to left window
- `Ctrl-w j` - Move to down window
- `Ctrl-w k` - Move to up window
- `Ctrl-w l` - Move to right window
- `Ctrl-w w` - Cycle through windows
- `Ctrl-w p` - Jump to previous window

**Splits**:
- `Ctrl-w v` - Create vertical split
- `Ctrl-w s` - Create horizontal split

**Closing**:
- `Ctrl-w c` - Close current window
- `Ctrl-w o` - Close all other windows (zoom current)
- `:q` - Quit window

**Resizing**:
- `Ctrl-w >` - Increase width
- `Ctrl-w <` - Decrease width
- `Ctrl-w +` - Increase height
- `Ctrl-w -` - Decrease height

## Leader Key

Default: `\` (backslash)
Common alternative: `<Space>` (may be set in your LazyVim config)

Check with: `:echo mapleader`

## Color Scheme

**Primary Colors**:
- Green: `#50fa7b`
- Light Green: `#7fc87f` (accents, tree lines)

**Applied To**:
- Directory names (bold when open)
- File names
- Symlinks and symlink targets
- Tree indent markers
- All text in nvim-tree (no color variation by file type)

## Terminal Tools

### ripgrep (rg)

**Purpose**: Fast text search for Telescope live grep

**Installation**: `brew install ripgrep`

**Use**: Automatically used by Telescope for `<leader>fg`

### osgrep (Optional)

**Purpose**: Semantic code search using AI embeddings

**Installation**: `pnpm add -g osgrep`

**Use Cases**:
- Search by concept: `osgrep "where do we handle authentication"`
- Explore unfamiliar codebases
- Find code when you don't know exact function names

**Commands**:
- `osgrep "query"` - Semantic search
- `osgrep index` - Index current directory
- `osgrep serve --background` - Start background server for faster searches
- `osgrep serve status` - Check server status
- `osgrep serve stop` - Stop server
- `osgrep doctor` - Verify installation

**Comparison**:
- **ripgrep**: Exact string matching, fast
- **osgrep**: Concept matching, semantic understanding

## Karabiner Elements Setup

**Ergonomic Key Remapping**:
- Caps Lock → Control (all the time)
- Right Enter → Control (when held)
- Shift+3 → `#` (UK keyboard fix)
- Option+3 → `£` (UK keyboard fix)

**Configuration**: `~/.config/karabiner/karabiner.json`

## eza (Terminal File Listing)

**Purpose**: Enhanced `ls` with tree view

**Configuration**:
```bash
# In ~/.zshrc
export EZA_COLORS="reset:di=1;32:ex=1;32"  # Bold green for dirs and executables
alias ll='eza -lah --no-symlinks'
alias tree='eza --tree --level=3'
```

**Colors**:
- Directories: Bold green
- Executables: Bold green
- Everything else: Plain (minimal noise)

## Git Configuration

**delta (Enhanced Diffs)**:

```ini
# In ~/.gitconfig
[core]
    pager = delta

[interactive]
    diffFilter = delta --color-only

[delta]
    navigate = true
    line-numbers = true
```

**Auto-quit behavior**:
```bash
# In ~/.zshrc
export LESS='-F -R -X'  # Auto-quit if one screen, handle colors, don't clear
```

## Workflow Tips

### File Navigation
1. Open nvim in project root: `nvim .`
2. Toggle tree: `<leader>e`
3. Navigate with `j/k` or relative numbers
4. Open files with `l` or `o` (focus stays in tree)
5. Quick search: `<leader>ff` (files) or `<leader>fg` (content)

### Window Management
1. Split vertically: `Ctrl-w v`
2. Navigate splits: `Ctrl-w h/l`
3. Close unneeded windows: `Ctrl-w c`
4. Zoom current window: `Ctrl-w o`

### Telescope Workflow
1. Trigger search: `<leader>ff` or `<leader>fg`
2. Type to filter (fuzzy matching works)
3. Navigate with `Ctrl-j/k` (stay in insert mode)
4. Preview with `Ctrl-u/d` to scroll
5. Open with `Enter` or cancel with `Ctrl-g`

### Two-Handed Ergonomics
With Karabiner setup:
- Left hand: Caps Lock (Control) for modifiers
- Right hand: Right Enter (Control when held) for modifiers
- Comfortable window management: Hold modifier with one hand, action key with other

## File Locations

**Neovim Config**:
- Main: `~/.config/nvim/init.lua`
- Plugins: `~/.config/nvim/lua/plugins/`
- Keymaps: `~/.config/nvim/lua/config/keymaps.lua`
- nvim-tree: `~/.config/nvim/lua/plugins/nvim-tree.lua`
- Telescope: `~/.config/nvim/lua/plugins/telescope.lua`

**Terminal Config**:
- Zsh: `~/.zshrc`
- Kitty: `~/.config/kitty/kitty.conf`
- Git: `~/.gitconfig`
- Karabiner: `~/.config/karabiner/karabiner.json`

## Key Principles

1. **Stick to vim standards** - Muscle memory works everywhere (bare vim, remote servers)
2. **Minimal visual noise** - No unnecessary icons, colors, or formatting
3. **Systematic configuration** - Modular, version-controlled, reproducible
4. **Ergonomic setup** - Two-handed operation, comfortable modifier keys
5. **Terminal-centric** - All tools work in the terminal, no GUI dependencies

## Dependencies

**Required**:
- Neovim (with LazyVim)
- ripgrep (`brew install ripgrep`)

**Plugins**:
- nvim-tree/nvim-tree.lua
- nvim-lua/plenary.nvim
- nvim-telescope/telescope.nvim
- BurntSushi/ripgrep

**Optional**:
- osgrep (semantic search): `pnpm add -g osgrep`
- git-delta (enhanced diffs): `brew install git-delta`
- eza (enhanced ls): `brew install eza`

## Troubleshooting

**Keybinding not working**:
1. Check if loaded: `:nmap <leader>e`
2. Check for conflicts: `:verbose nmap <leader>e`
3. Force reload: `:source ~/.config/nvim/lua/config/keymaps.lua`
4. Full restart: Quit and reopen nvim

**Telescope git root search**:
- Works in git repositories automatically
- Falls back to current directory if not in git repo
- Helper function handles both cases transparently

**nvim-tree colors wrong**:
- Highlights set in plugin config
- Check terminal true color support: `:set termguicolors?`
- Should be on for proper colors

**Window behavior unexpected**:
- Remember: `Ctrl-w` commands are two-key sequences
- Press and release `Ctrl-w`, then press action key
- With Karabiner: Hold modifier, press w, release, press action
