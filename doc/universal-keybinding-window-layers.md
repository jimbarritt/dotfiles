# Unified Keyboard Navigation Strategy

## Philosophy

This document defines a unified keybinding strategy across all layers of the development environment. The goal is to minimize cognitive load, reduce context switching, and build transferable muscle memory that works seamlessly from text editing to system-wide window management.

## Design Principles

1. **Same directional keys everywhere**: `hjkl` navigation across all tools
2. **Different modifiers per layer**: Clear separation of concerns prevents conflicts
3. **Transferable muscle memory**: Skills learned on macOS transfer to NixOS with zero retraining
4. **Physical consistency**: The same physical keys trigger the same actions across systems

## The Four Layers

| Layer | Scope | Modifier | Example |
|-------|-------|----------|---------|
| **nvim** | Window management within editor | `Ctrl` | `Ctrl-h` = focus left window |
| **tmux** | Pane management within terminal session | `Ctrl-Space` (prefix) | `Ctrl-Space h` = focus left pane |
| **AeroSpace** | System-wide window management (macOS) | `Cmd` | `Cmd-h` = focus left window |
| **Hyprland** | System-wide window management (NixOS) | `Super` | `Super-h` = focus left window |

## Mental Model

The same navigation pattern applies at every layer, just with different trigger modifiers:

| Action | nvim | tmux | AeroSpace (Mac) | Hyprland (NixOS) |
|--------|------|------|-----------------|------------------|
| **Focus left** | `Ctrl-h` | `Ctrl-Space h` | `Cmd-h` | `Super-h` |
| **Focus down** | `Ctrl-j` | `Ctrl-Space j` | `Cmd-j` | `Super-j` |
| **Focus up** | `Ctrl-k` | `Ctrl-Space k` | `Cmd-k` | `Super-k` |
| **Focus right** | `Ctrl-l` | `Ctrl-Space l` | `Cmd-l` | `Super-l` |
| **Move window** | - | - | `Cmd-Shift-hjkl` | `Super-Shift-hjkl` |
| **Split vertical** | `<leader>\|` | `Prefix \|` | `Cmd-v` | `Super-v` |
| **Split horizontal** | `<leader>-` | `Prefix -` | `Cmd-s` | `Super-s` |
| **Workspace 1** | - | `Prefix 1` | `Cmd-1` | `Super-1` |
| **Close** | `<leader>q` | `Prefix q` | `Cmd-Shift-q` | `Super-Shift-q` |
| **Fullscreen/Zoom** | `<leader>m` | `Prefix m` | `Cmd-m` | `Super-m` |
| **Resize mode** | `Ctrl-arrows` | `Prefix-arrows` | `Cmd-r` then `hjkl` | `Super-r` then `hjkl` |
| **Float toggle** | - | - | `Cmd-f` | `Super-f` |

## Physical Key Mapping

### macOS (Current)
- Physical `Cmd` key → `cmd` modifier in AeroSpace
- Karabiner Elements: Right `Enter` → `Control` when held

### NixOS (Future)
- Physical `Cmd` key (on Mac keyboard) → `Super`/`Mod4` in Hyprland
- **Same physical key, different OS label**
- Zero muscle memory retraining required

## Configuration References

### Layer 1: nvim

**Location**: `~/.config/nvim/lua/config/keymaps.lua`
```lua
