# CLAUDE.md

## Commands

Build commands are in `justfile`. `just test` runs on every commit via pre-commit hook.

## Rules

- Changes to this repo are only made upstream (as GitHub user `jimbarritt`), then pulled on other machines. Before editing any file here, run `am-dotfiles-base` — if it exits non-zero, do not make local changes; tell the user to make them upstream and pull.
- Cloud sessions (claude.ai/code, `CLAUDE_CODE_REMOTE=true`) are upstream: the clone pushes to GitHub directly. In a cloud session:
  - Skip `am-dotfiles-base`. It fails there because `gh` is not authenticated.
  - Commit your changes and push straight to `main`. Do not use the session's `claude/...` branch, and do not open a PR unless the user asks for one.
- Check `doc/` and `docs/` for cached research before searching the web. FlashSpace source guide: `doc/window-tiling-macos/flashspace-source-guide.md`. If asked about graphify, read `doc/graphify.md`.
- Never run `do.sh` — always let the user run it manually.
- Never run `bin/` scripts autonomously — tell the user to run them. Exception: `am-dotfiles-base`, which exists for Claude to run.
- When adding a new terminal to the shell guard, add a clause to `_is_known_terminal` in `home/zshrc`.
- Outside a cloud session, never run `git commit` in this repo — the user does all committing themselves. Only draft a PR when the user explicitly asks for one.

## Machine-local overrides

`~/.zshrc_machine` and `~/.zshrc_work` are sourced at the end of zshrc. Neither is tracked.
