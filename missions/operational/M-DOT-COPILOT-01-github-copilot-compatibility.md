# Mission: GitHub Copilot compatibility

| Field | Value |
|---|---|
| ID | M-DOT-COPILOT-01 |
| Status | DONE |
| Blocked by | none |

## Objective

- Copilot CLI and Claude Code share one tracked config surface: instructions, skills and deny list
- Changes to this repo are made upstream only

## Purpose

Use the same instructions, skills and permissions in Copilot CLI as in Claude Code.

## Decisions

- Global Copilot permissions through a `copilot()` zsh wrapper with `--allow-all` plus `--deny-tool` flags from `home/copilot/denied-commands`. Jim, 2026-07-06
- The work machine's `permissions-config.json` is not tracked: it is per-location and work-specific. Jim, 2026-07-06
- AGENTS.md-canonical setup is a `bin/copilot-repo-init` script, not a skill: too infrequent to justify per-session context. Jim, 2026-07-06
- No Claude references in PR titles, bodies or commits. Jim, 2026-07-06

## Plan

| ID | Task | Status |
|---|---|---|
| T-01 | Copilot CLI integration | DONE |
| T-02 | Repo governance | DONE |

## Intel

- Task notes, as imported from the plan: `missions/operational/M-DOT-COPILOT-01/task-notes.md`
