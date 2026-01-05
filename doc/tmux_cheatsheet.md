# TMUX Cheatsheet

## Window Commands

### Create & Navigate
| Command | Action |
|---------|--------|
| `Ctrl-b c` | Create new window |
| `Ctrl-b n` | Next window |
| `Ctrl-b p` | Previous window |
| `Ctrl-b [0-9]` | Go to window number (0-9) |
| `Ctrl-b w` | List all windows (with preview) |
| `Ctrl-b f` | Find window by name |

### Manage Windows
| Command | Action |
|---------|--------|
| `Ctrl-b ,` | Rename current window |
| `Ctrl-b &` | Close current window |
| `Ctrl-b !` | Break window into new session |
| `Ctrl-b .` | Move window to another session |

### Window Layout (Panes)
| Command | Action |
|---------|--------|
| `Ctrl-b %` | Split window vertically |
| `Ctrl-b "` | Split window horizontally |
| `Ctrl-b x` | Close current pane |
| `Ctrl-b o` | Next pane |
| `Ctrl-b ;` | Previous pane |
| `Ctrl-b {` | Swap panes left |
| `Ctrl-b }` | Swap panes right |
| `Ctrl-b z` | Toggle pane zoom |

### Window Info
| Command | Action |
|---------|--------|
| `Ctrl-b i` | Show window info |
| `Ctrl-b l` | Last active window |
