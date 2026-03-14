# tmux config

## Concepts

tmux has three levels:

- **Session** — top-level named workspace, persists after detach. Use one per project.
- **Window** — a "tab" within a session, fills the full terminal.
- **Pane** — a split within a window.

## Our setup

Config lives at `home/tmux.conf` → symlinked to `~/.tmux.conf`.

Key settings:

- `status off` — status bar hidden by default (toggle with `Ctrl-b b`)
- `mouse on` — click to select panes, scroll with trackpad
- Terminal overrides for full colour and italic support (ghostty/kitty compatible)

## Sessions (project workspaces)

| Command | Action |
|---|---|
| `tmux new -s <name>` | Create a named session |
| `tmux attach -t <name>` | Attach to an existing session |
| `tmux ls` | List sessions |
| `Ctrl-b $` | Rename current session |
| `Ctrl-b d` | Detach (session keeps running) |
| `Ctrl-b s` | Browse and switch sessions |
| `Ctrl-b (` / `)` | Previous / next session |
| `Ctrl-b L` | Toggle last session |

## Windows (tabs within a session)

See `tmux_cheatsheet.md` for the full window command reference.

Key ones:

| Command | Action |
|---|---|
| `Ctrl-b c` | New window |
| `Ctrl-b ,` | Rename window |
| `Ctrl-b n` / `p` | Next / previous window |
| `Ctrl-b w` | Window list with preview |

## Panes (splits)

| Command | Action |
|---|---|
| `Ctrl-b %` | Split vertically |
| `Ctrl-b "` | Split horizontally |
| `Ctrl-b z` | Zoom/unzoom pane |
| `Ctrl-b x` | Close pane |

## Status bar

Hidden by default. Toggle with `Ctrl-b b`.
