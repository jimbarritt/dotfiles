# M-DOT-INSTALL-01: task notes

Imported from `doc/planning/archive` in the dotfiles repo, heading "Delta 2: Dotfiles Install Script". The bullets are as written in the plan.

### T-01: Wire Claude config into do.sh
- ✓ DONE — Add `link_claude` function to `do.sh` covering: `settings.json`, `keybindings.json`, `statusline-command.sh`, `CLAUDE.md`, all `skills/*` dirs, all `themes/*` files
- ✓ DONE — Call `link_claude` from `link_all`
- ✓ DONE — Add corresponding `unlink_claude` entries to `unlink_all`

### T-02: Update README.md
- ✓ DONE — Rewrite `README.md` to reflect current setup (replace outdated Java/Emacs/Vagrant content; salvageable keyboard/mac-setup content already lives in `doc/`)
- ✓ DONE — Document the Claude tooling section (skills, statusline, settings) + Copilot sharing, layout table, do.sh command table, governance note
- ✓ DONE — Fix `do.sh`: `slurp_arguments` didn't accept `link-copilot`/`unlink-copilot`, so both died as "Unknown option" before dispatch
