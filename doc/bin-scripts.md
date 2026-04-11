# bin scripts

Custom scripts live in `bin/` and are on `$PATH` via `zshrc`. This doc gives a one-line purpose for each and fuller notes on the ones that are non-obvious.

## Listing

| Script | Purpose |
| --- | --- |
| `airdrop`, `airdrop-*` | Toggle and drive AirDrop from the command line |
| `clock` | Terminal clock display |
| `copy_file` | Copy a file to the macOS clipboard |
| `find-in` | Search helper |
| `git-status`, `git_completion` | Git helpers |
| `key-help` | Show a curated keybinding cheatsheet for the current pane command (see `config/key-help/`) |
| `lsp-doctor` | Diagnose and clean up kotlin-lsp state (see below) |
| `nw-speed` | Network download speed test |
| `presentation-mode` | Toggle between presentation (light) and normal (dark) theme for Ghostty + Neovim |
| `pywork` | Python work environment helper |
| `set-hostname` | Set the machine hostname |
| `term-colors` | Print terminal colour table |
| `tmux-session-list` | Used by `tmux` status-left to render the session list |
| `tmux-sessionizer` | Fuzzy jump to a project directory or existing tmux session |
| `use-emacs-live`, `use-jmdb-emacs` | Switch between Emacs configurations |

## `lsp-doctor`

Diagnoses the state of the JetBrains kotlin-lsp, which is known to leak
processes and stale workspace locks when nvim doesn't shut down cleanly.

### What it checks

1. **Running kotlin-lsp processes** — shows PID, PPID, elapsed time, orphan
   status (`ppid == 1` means the parent died), and the process's current
   working directory (resolved via `lsof`).
2. **Workspace lock files** under `~/.cache/kotlin-lsp-workspaces/<project>/.app.lock`.
   Each lock is matched against running processes by cwd basename. Locks
   with no matching process are flagged `stale`.

### Usage

```sh
lsp-doctor            # list processes and lock files
lsp-doctor --clean    # kill orphaned processes and remove stale locks
lsp-doctor -h         # show help
```

### When it helps

kotlin-lsp does not support multiple editing sessions for the same workspace.
If nvim crashes or exits without cleaning up:

- The JVM process may be reparented to launchd (ppid 1) and linger for days.
- The `.app.lock` file may remain, blocking the next nvim session from
  starting the LSP for that project.

`lsp-doctor` surfaces both situations; `--clean` removes them. The nvim
kotlin plugin popup that fires on LSP startup failure now points at
`lsp-doctor` as the remediation step (see
`config/nvim/lua/plugins/kotlin.lua`).
