# CLAUDE.md

## Commands

Build commands are in `justfile`. `just test` runs on every commit via pre-commit hook.

## Rules

- Check `doc/` and `docs/` for cached research before searching the web.
- Never run `do.sh` — always let the user run it manually.
- Never run `bin/` scripts autonomously — tell the user to run them.
- When adding a new terminal to the shell guard, add a clause to `_is_known_terminal` in `home/zshrc`.

## Machine-local overrides

`~/.zshrc_machine` and `~/.zshrc_work` are sourced at the end of zshrc. Neither is tracked.
