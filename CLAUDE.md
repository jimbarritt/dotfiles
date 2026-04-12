# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```sh
just install          # Install zsh plugins and configure git hooks
just link             # Symlink all dotfiles into $HOME
just link --dry-run   # Preview what link would do
just test             # Run shellcheck on do.sh and hooks/pre-commit
```

`just test` runs on every commit via the pre-commit hook.

## Repo location

The canonical repo is at `~/Code/github/jimbarritt/dotfiles`. `~/projects/dotfiles` is a symlink to the same place — they are not separate clones.

## How it works

`do.sh` is the main script. It symlinks files from two directories into the home directory:

- `home/<file>` → `~/.{file}` (e.g. `home/zshrc` → `~/.zshrc`)
- `config/<dir>` → `~/.config/<dir>` (e.g. `config/nvim` → `~/.config/nvim`)

The oh-my-zsh custom theme (`home/oh-my-zsh/green-tinted.zsh-theme`) is linked directly into `~/.oh-my-zsh/custom/themes/`.

## Shell guard (`home/zshrc`)

The top of `zshrc` has a shell guard that blocks interactive shells launched from unknown contexts. It checks `$TERM_PROGRAM` / env vars and walks up the process tree (up to 3 levels) via `_process_tree_contains`. Known terminals are kitty, ghostty, vscode, JetBrains, emacs (vterm), tmux, and SSH. Unrecognised contexts prompt for a password stored in `~/.shellpwd`.

When adding support for a new terminal, add a clause to `_is_known_terminal` that checks the appropriate env var and the process name it runs under.

## Key-help cheatsheets

Curated keybinding cheatsheets live in `config/key-help/` (symlinked to `~/.config/key-help/`). The `bin/key-help` script displays the appropriate one based on the current pane command. To add keybindings for nvim, edit `config/key-help/nvim`.

## bin/ scripts

Utility scripts live in `bin/` (symlinked to `~/bin/`). Notable ones:

- `key-help` — displays keybinding cheatsheet for the current pane command
- `reload-flashspace` — reloads the FlashSpace config after editing `config/flashspace/`

Always tell the user to run these rather than invoking them autonomously.

## Applying config changes

`config/<dir>` is symlinked as a **directory** to `~/.config/<dir>` — individual files inside appear as regular files when inspected, but are live through the directory symlink. Editing files in `config/` in the repo is sufficient; there is no need to copy to `~/.config/`.

To verify a symlink: check `ls -la ~/.config/` (directory level), not `ls -la ~/.config/<dir>/file`.

## Machine-local overrides

`~/.zshrc_machine` is sourced at the end of `zshrc` for machine-specific config (e.g. paths that differ per machine). It is auto-populated by nvim plugins (e.g. `kotlin.lua` writes `KOTLIN_LSP_DIR` here on first detection). `~/.zshrc_work` is sourced after it for work-specific config. Neither file is tracked in the repo.
