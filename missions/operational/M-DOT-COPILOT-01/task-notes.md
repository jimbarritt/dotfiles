# M-DOT-COPILOT-01: task notes

Imported from `doc/planning/archive` in the dotfiles repo, heading "Delta 4: GitHub Copilot Compatibility". The bullets are as written in the plan.

### T-01: Copilot CLI integration
- ✓ DONE — YAML frontmatter added to `proofread` and `publish-crate` skills (Copilot requires `name` matching dir + `description`; other five already had it)
- ✓ DONE — `doc/claude-copilot-compatibility.md` created: install, skills, global/per-repo instructions, permissions
- ✓ DONE — `do.sh link-copilot`/`unlink-copilot`: `~/.copilot/copilot-instructions.md` → `home/claude/CLAUDE.md`, all skills → `~/.copilot/skills/`, `~/.copilot/denied-commands` → `home/copilot/denied-commands`
- ✓ DONE — Global permissions via `copilot()` zsh wrapper: `--allow-all` + `--deny-tool` flags from `home/copilot/denied-commands` (mirrors Claude's deny list — keep in sync). Copilot has no global permissions config; per-command/per-kind allows all left residual prompts
- ✓ DONE — Work machine's `permissions-config.json` assessed: per-location + Pleo-specific → not tracked; command list superseded by deny-list approach
- ✓ DONE — `.github/copilot-instructions.md → ../CLAUDE.md` symlink in this repo
- ✓ DONE — AGENTS.md-canonical pattern documented (CLAUDE.md `@AGENTS.md` import; Claude doesn't read AGENTS.md natively) + `bin/copilot-repo-init` for the mechanical half (skill approach tried and rejected — too infrequent to justify per-session context)
- Note: open offer to convert this repo itself to the AGENTS.md pattern — optional, not decided

### T-02: Repo governance
- ✓ DONE — `bin/am-dotfiles-base` + CLAUDE.md rule: changes only upstream (as `jimbarritt`); other machines check via script and pull
- ✓ DONE — Global CLAUDE.md: PR rule (no Claude references in titles/bodies/commits)
- ✓ DONE — `settings.json` synced with work machine: `model: sonnet`, `effortLevel: medium`; graphify `Glob|Grep` hook rejected
- ✓ DONE — `home/claude/README.md` rewritten (was badly stale); zshrc `reload` → help function + `reload-zshrc`
