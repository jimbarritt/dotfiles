# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```sh
make install        # Install zsh plugins and configure git hooks
make link           # Symlink all dotfiles into $HOME
make link-dry-run   # Preview what link would do
make test           # Run shellcheck on do.sh and hooks/pre-commit
```

`make test` runs on every commit via the pre-commit hook.

## How it works

`do.sh` is the main script. It symlinks files from two directories into the home directory:

- `home/<file>` → `~/.{file}` (e.g. `home/zshrc` → `~/.zshrc`)
- `config/<dir>` → `~/.config/<dir>` (e.g. `config/nvim` → `~/.config/nvim`)

The oh-my-zsh custom theme (`home/oh-my-zsh/green-tinted.zsh-theme`) is linked directly into `~/.oh-my-zsh/custom/themes/`.

## Shell guard (`home/zshrc`)

The top of `zshrc` has a shell guard that blocks interactive shells launched from unknown contexts. It checks `$TERM_PROGRAM` / env vars and walks up the process tree (up to 3 levels) via `_process_tree_contains`. Known terminals are kitty, ghostty, vscode, JetBrains, emacs (vterm), tmux, and SSH. Unrecognised contexts prompt for a password stored in `~/.shellpwd`.

When adding support for a new terminal, add a clause to `_is_known_terminal` that checks the appropriate env var and the process name it runs under.

## Machine-local overrides

`~/.zshrc_work` is sourced at the end of `zshrc` if it exists — use it for machine-specific config that shouldn't live in this repo.
