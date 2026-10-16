# Claude config layout

Imported from the Implementation Notes of `doc/planning/plan.md`. Used by: M-DOT-CLAUDE-01, M-DOT-INSTALL-01.

- All Claude config lives in `home/claude/` and is symlinked into `~/.claude/` via `do.sh link-claude`
- Hooks live in `home/claude/hooks/` — symlinked individually into `~/.claude/hooks/`
- Skills are directory symlinks — symlink the whole dir, not individual files
- All skills in `~/.claude/skills/` are directory symlinks into `home/claude/skills/` — fully tracked in dotfiles
- OTEL env vars (`OTEL_EXPORTER_OTLP_*`) belong in `~/.zshrc_machine` on machines with trakr, not in `settings.json`
