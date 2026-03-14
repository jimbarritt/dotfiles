# tmux config

## The prefix key

Every tmux command starts with a **prefix key**, defaulting to `Ctrl-b`. It was chosen because it's rarely used in normal terminal apps — unlike `Ctrl-a` which GNU Screen used and which clashes with readline's "go to start of line". You can rebind it, but `Ctrl-b` is fine unless it conflicts with something you use regularly.

## Concepts

tmux has three levels of nesting:

- **Session** — top-level named workspace, persists after detach. Use one per project.
- **Window** — a fullscreen layout within a session. You can only see one at a time. Mostly ignore these — one window per session is fine.
- **Pane** — a split within a window. Use these when you want to see multiple things at once.

**Practical mental model: sessions = projects, panes = splits. Ignore windows.**

## Our setup

Config lives at `home/tmux.conf` → symlinked to `~/.tmux.conf`.

Key settings:

- `status on` — status bar always visible, showing sessions
- `mouse on` — click to select panes, scroll with trackpad
- Terminal overrides for full colour and italic support (ghostty/kitty compatible)

### Status bar

Shows all sessions. Current session is highlighted with a darker green background. Toggle with `Ctrl-b b`.

Implemented via `bin/tmux-session-list.sh` (symlinked to `~/bin/`), called from `status-left` with the current session name passed as an argument — needed because tmux's `#()` shell commands are shared across clients, so we pass `#{session_name}` as an arg to distinguish per-client.

### key-help popup

`Ctrl-b ?` shows a context-aware cheatsheet popup. It detects what is running in the current pane and shows the matching cheatsheet. Press `q` to close.

Cheatsheets live in `config/key-help/` → symlinked to `~/.config/key-help/`. One file per command name (e.g. `nvim`, `zsh`, `claude`). Falls back to `default` if no file matches.

The popup uses `run-shell` to invoke `display-popup`, passing `#{pane_current_command}` as an argument. This is necessary because `display-popup`'s shell command argument does not expand tmux format strings — only `run-shell` does.

Currently mapped commands:
- `zsh` / `claude` → tmux cheatsheet (you're always in tmux context)
- `nvim` → nvim cheatsheet

To add a new cheatsheet: create `config/key-help/<command>` with a header line like `TOOL CHEATSHEET` and the content you want.

## Sessions

From outside tmux:

```sh
tmux new -s <name>        # create and attach
tmux attach -t <name>     # reattach
tmux ls                   # list sessions
```

From inside tmux:

| Keybinding | Action |
|---|---|
| `Ctrl-b a` | New session (prompts for name) |
| `Ctrl-b s` | Browse and switch sessions |
| `Ctrl-b (` / `)` | Previous / next session |
| `Ctrl-b L` | Toggle last session |
| `Ctrl-b $` | Rename session |
| `Ctrl-b k` | Kill current session (switches to next first) |
| `Ctrl-b d` | Detach (session keeps running) |

### Creating sessions from inside tmux

Running `tmux new -s name` from a shell inside tmux triggers a nesting warning. Use `Ctrl-b a` instead, which uses tmux's internal command system and avoids the issue.

## Panes

| Keybinding | Action |
|---|---|
| `Ctrl-b %` | Split vertically |
| `Ctrl-b "` | Split horizontally |
| `Ctrl-b z` | Zoom/unzoom pane |
| `Ctrl-b x` | Close pane |
| `Ctrl-b o` | Next pane |
| `Ctrl-b q` | Flash pane numbers, press number to jump |

## Other

| Keybinding | Action |
|---|---|
| `Ctrl-b b` | Toggle status bar |
| `Ctrl-b ?` | Show context-aware cheatsheet |
| `Ctrl-b [` | Scroll mode (q to exit) |

## Future

- **tmux-resurrect** — saves and restores sessions, pane layouts, and working directories across reboots. Pair with **tmux-continuum** for auto-save/restore on startup.
