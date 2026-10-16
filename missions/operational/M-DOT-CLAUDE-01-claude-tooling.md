# Mission: Claude tooling

| Field | Value |
|---|---|
| ID | M-DOT-CLAUDE-01 |
| Status | DONE |
| Blocked by | none |

## Objective

- The statusline shows the account type, usage and context size
- The plan skills exist and are linked into `~/.claude/skills/`
- Claude config is documented in `home/claude/README.md`
- A context rot canary hook runs on the first message of each session
- The global `CLAUDE.md` holds the session, task tracking, projects and search rules
- The pre-tool-use filter hook is tracked and wired into `settings.json`
- The live `~/.claude/settings.json` is a symlink into the dotfiles
- `doc/graphify.md` documents graphify install and per-repo setup

## Purpose

Bring the Claude Code configuration under version control in this repo, so every machine gets the same setup.

## Decisions

- Graphify notes live in `doc/graphify.md`, not in the global `CLAUDE.md`. A repo `CLAUDE.md` rule points at the doc. Jim, 2026-07-06
- Graphify install is `uv tool install graphifyy`, and per-repo setup is upstream's `graphify claude install`. The two tracked graphify scripts were deleted. Jim, 2026-07-06
- `Skill(*)` added to global permissions to remove the skill approval prompt. Jim, before 2026-07-06

## Plan

| ID | Task | Status |
|---|---|---|
| T-01 | Statusline account-type toggle | DONE |
| T-02 | Plan skills | DONE |
| T-03 | Claude config documentation | DONE |
| T-04 | Context rot canary hook | DONE |
| T-05 | Global CLAUDE.md hardening | DONE |
| T-06 | Pre-tool-use filter hook | DONE |
| T-07 | settings.json sync and symlink | DONE |
| T-08 | Graphify awareness doc | DONE |

## Intel

- Task notes, as imported from the plan: `missions/operational/M-DOT-CLAUDE-01/task-notes.md`
- Claude config layout: `missions/operational/M-DOT-CLAUDE-01/intel/claude-config-layout.md`
