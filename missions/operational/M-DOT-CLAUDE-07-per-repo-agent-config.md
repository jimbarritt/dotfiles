# Mission: Per-repo agent config

| Field | Value |
|---|---|
| ID | M-DOT-CLAUDE-07 |
| Status | TODO |
| Blocked by | none |

## Objective

- A skill asks the setup questions the first time Claude works in a repo with no local Claude config
- Each repo has an explicit stance on autonomous `git commit` and push

## Purpose

Follow-up to M-DOT-CLAUDE-06. The global git-commit guard is gone, so each repo needs its own stance, set explicitly.

## Open questions

- What else belongs in the first-run question set, for example a PR-only workflow or destructive-command allowances?
- Where do the answers get written: the project `CLAUDE.md`, `.claude/settings.json`, or both?

## Plan

| ID | Task | Status |
|---|---|---|
| T-01 | Repo-init skill for autonomy questions | TODO |

## Intel

- Task notes, as imported from the plan: `missions/operational/M-DOT-CLAUDE-07/task-notes.md`
