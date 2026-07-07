# dotfiles

Personal macOS configuration: zsh, git, tmux, Neovim, Ghostty, Hammerspoon, FlashSpace — plus a fully tracked config surface for Claude Code and GitHub Copilot CLI.

Everything is symlinked from this repo into place, so changes here take effect immediately and the live config never drifts from what's tracked.

## Install

```sh
git clone git@github.com:jimbarritt/dotfiles.git
cd dotfiles

just install    # zsh plugins, git hooks, macOS defaults
just link       # symlink everything into $HOME (pass --dry-run to preview)
```

`just test` runs shellcheck over the scripts (also enforced by the pre-commit hook).

Under the hood both recipes call `do.sh`, which also supports finer-grained commands:

| Command | What it links |
|---------|---------------|
| `./do.sh link` | Everything below |
| `./do.sh link-claude` | Claude config → `~/.claude/` |
| `./do.sh link-copilot` | Copilot config → `~/.copilot/` |
| `./do.sh unlink` / `unlink-claude` / `unlink-copilot` | Remove the symlinks |

All commands accept `--dry-run`.

## Layout

| Directory | Contents |
|-----------|----------|
| `home/` | Files symlinked into `$HOME` as dotfiles: `zshrc`, `gitconfig`, `tmux.conf`, `hammerspoon/` |
| `home/claude/` | Claude Code config: `CLAUDE.md`, `settings.json`, skills, hooks, themes, statusline |
| `home/copilot/` | Copilot CLI config: denied-commands list (mirrors Claude's deny list) |
| `config/` | Files symlinked into `~/.config/`: nvim, ghostty, kitty, flashspace, mise |
| `bin/` | Scripts, symlinked as `~/bin` — see `doc/bin-scripts.md` |
| `doc/` | Notes and cached research — start at `doc/INDEX.md` |
| `doc/planning/` | The living plan (`plan.md`) and archive of completed work |
| `hooks/` | Git hooks for this repo (`core.hooksPath`) — pre-commit runs `just test` |
| `keyboard/`, `system/`, `ide/` | Keyboard layouts (UK `#`/`£` fix), macOS bits, editor settings |

## Claude Code tooling

`home/claude/` is the single source of truth, symlinked into `~/.claude/` by `link-claude`:

- **Skills** — directory symlinks into `~/.claude/skills/`, including the plan workflow (`init-plan`, `load-plan`, `update-plan`, `prune-plan`) whose shared format spec lives in `skills/plan-format/PLAN-FORMAT.md`
- **Hooks** — `canary-inject.sh` (context-rot canary) and `pre-tool-use-filter.sh` (blocks destructive commands); wired via `settings.json`
- **Statusline** — `statusline-command.sh`: context-token warning bands, subscription/API cost display
- **Permissions** — global allow/deny lists in `settings.json`; see `doc/claude-permissions.md` for the `//` absolute-path anchoring gotcha

Copilot CLI shares the same config surface: `link-copilot` points `~/.copilot/copilot-instructions.md` at the global `CLAUDE.md` and links all skills. A `copilot()` wrapper in `zshrc` applies the deny list. Details in `doc/claude-copilot-compatibility.md`.

## Machine-local overrides

`~/.zshrc_machine` (per-machine paths, OTEL vars) and `~/.zshrc_work` (work-specific) are sourced at the end of `zshrc` and are not tracked.

## Governance

Changes are made only in this upstream repo (as `jimbarritt`), then pulled on other machines — `bin/am-dotfiles-base` checks which side you're on.
