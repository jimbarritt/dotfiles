# Mission: Claude instruction refinements

| Field | Value |
|---|---|
| ID | M-DOT-CLAUDE-06 |
| Status | DONE |
| Blocked by | none |

## Objective

- The global `CLAUDE.md` holds a prose rule with no editorial commentary
- The global git-commit guard is removed

## Purpose

Refine the global Claude instructions after use.

## Decisions

- The "never run `git commit` unless asked" rule moves from the global `CLAUDE.md` to a per-repo setting. Jim, 2026-08-02
- Keep `~/.claude/CLAUDE.md` as the global file. No AGENTS.md migration, because Claude Code reads AGENTS.md at project level only. Jim, 2026-08-02

## Plan

| ID | Task | Status |
|---|---|---|
| T-01 | Prose commentary rule | DONE |
| T-02 | Remove global git-commit guard | DONE |

## Intel

- Task notes, as imported from the plan: `missions/operational/M-DOT-CLAUDE-06/task-notes.md`
