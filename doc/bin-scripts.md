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

### Terminology

A kotlin-lsp is called **dangling** when its parent process is no longer a
live nvim — typically because nvim crashed or was killed without a clean
shutdown. Dangling LSPs hold onto workspace locks and memory but serve no
editor. This is the thing `lsp-doctor --clean` is designed to find and
remove.

A **gradle-daemon** is different and is *never* considered dangling, even
though it often shows up with parent `1 (launchd)`. See the section below
for why.

### What it checks

1. **Process tree** of nvim + kotlin-lsp + gradle-daemon, rendered with
   ASCII branches. Columns: `TREE`, `PID`, `PARENT`, `ELAPSED`, `CWD`.
   `PARENT` shows the ppid, annotated as `1 (launchd)` when the process has
   been reparented. Dangling kotlin-lsps (those not under an nvim) are
   tagged `[dangling]`.
2. **Workspace lock files** under `~/.cache/kotlin-lsp-workspaces/<project>/.app.lock`.
   Each lock is matched to a running non-dangling kotlin-lsp by cwd basename.
   Locks with no matching owner (or whose only matches are dangling) are
   flagged `stale`.
3. **Version check** (optional, via `--check-version`) — compares the
   installed kotlin-lsp cask version against the latest available from
   Homebrew and prints upgrade instructions if outdated.

### Distinguishing kotlin-lsp from gradle-daemon

Both processes run under the same JVM binary
(`/opt/homebrew/Caskroom/kotlin-lsp/<version>/jre/.../bin/java`), so a naive
`ps | grep kotlin` catches them both. `lsp-doctor` tells them apart by
inspecting the full command line: if it contains
`org.gradle.launcher.daemon.bootstrap.GradleDaemon`, it's labelled
`gradle-daemon`; otherwise `kotlin-lsp`.

This matters because **Gradle daemons are expected to outlive kotlin-lsp
and nvim**. The lifecycle is:

1. nvim starts kotlin-lsp as a child.
2. kotlin-lsp needs to resolve the Kotlin classpath and shells out to Gradle.
3. Gradle forks a daemon JVM, double-forks + `setsid` to detach, and
   reparents it to launchd (ppid 1). The daemon is now fully independent.
4. When nvim exits, kotlin-lsp dies with it — but the gradle-daemon keeps
   running. It will hang around for ~3 hours (Gradle's default idle
   timeout) serving any future build request, whether from kotlin-lsp
   re-spawning, `./gradlew build` in a terminal, or IntelliJ.

Without this distinction, `lsp-doctor` would (wrongly) flag every
gradle-daemon as a dangling kotlin-lsp and `--clean` would kill it,
throwing away the warm JVM and making the next build slow.

### Usage

```sh
lsp-doctor                  # list processes and lock files
lsp-doctor --clean          # kill dangling processes and remove stale locks
lsp-doctor --check-version  # also check kotlin-lsp cask version (slower)
lsp-doctor -h               # show help
```

### When it helps

kotlin-lsp does not support multiple editing sessions for the same workspace.
If nvim crashes or exits without cleaning up:

- The JVM process may be reparented to launchd (ppid 1) and linger for days.
- The `.app.lock` file may remain, blocking the next nvim session from
  starting the LSP for that project.

`lsp-doctor` surfaces both situations; `--clean` removes them. The nvim
kotlin plugin popup that fires on LSP startup failure points at
`lsp-doctor` as the remediation step (see
`config/nvim/lua/plugins/kotlin.lua`).
