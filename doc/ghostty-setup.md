# Ghostty Setup

Ghostty replaces Kitty as the terminal emulator. Tabs and panes are handled by tmux instead of the terminal's built-in tab bar.

## Config location

`config/ghostty/` is symlinked to `~/.config/ghostty/` via `just link`.

```
config/ghostty/
├── config              # main config
└── themes/
    └── green-tinted    # custom theme ported from kitty
```

## Key decisions

### No titlebar, no tabs

`macos-titlebar-style = hidden` gives a clean borderless window. Ghostty's native macOS tabs don't match the look kitty had (no custom colours or separator styling), so we skip them entirely and use tmux for multiplexing.

### Green-tinted theme

Ported from the kitty `green-tinted.conf` theme — same 16 palette colours, background, foreground, cursor, and selection colours.

### Background blur

`background-opacity = 0.92` with `background-blur-radius = 40` replicates the frosted glass look from kitty.

### UK keyboard hash fix

`keybind = alt+3=text:#` — same workaround as kitty's `map alt+3 send_text all #` for UK keyboards where Shift+3 produces £ via Karabiner remapping.

### Shell integration

`shell-integration = detect` with `shell-integration-features = no-cursor` — lets Ghostty detect the shell automatically but keeps cursor style control in the zsh config rather than letting the shell integration override it.

## Window management

No macOS titlebar buttons with `macos-titlebar-style = hidden`, so use keyboard shortcuts:

| Shortcut | Action |
|---|---|
| `Cmd-w` | Close window |
| `Cmd-q` | Quit Ghostty |

## Useful Ghostty commands

```sh
ghostty +show-config     # dump the effective config (shows warnings for invalid keys)
ghostty +list-themes     # list all built-in themes
ghostty +list-keybinds   # list all active keybindings
```

## What's next

Full tmux setup for tabs, panes, and session management — see `doc/tmux_cheatsheet.md` for the basics.
