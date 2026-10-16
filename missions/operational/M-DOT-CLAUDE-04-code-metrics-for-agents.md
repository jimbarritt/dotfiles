# Mission: Code metrics for agents

| Field | Value |
|---|---|
| ID | M-DOT-CLAUDE-04 |
| Status | IN PROGRESS |
| Blocked by | none |

## Objective

- A `code-metrics` skill gives a coding agent a ranked report of file size, per-function complexity and duplication for an unfamiliar codebase
- The skill is linked and smoke-tested

## Purpose

Give a coding agent structural feedback about a codebase it does not know. Research: `doc/code-metrics-for-agents.md`.

## Decisions

- No own Homebrew formula. The skill documents `brew install scc` and `uv tool install lizard` as prerequisites, and the wrapper checks for them. Jim, before 2026-08-22
- `codelimit` rejected: unit length only, no CCN, no working Swift support. Jim, before 2026-08-22
- No `-l` language mapping. lizard defaults to every language it knows. Jim, before 2026-08-22
- Pure awk and sort over the CSV output. No jq or python dependency. Jim, before 2026-08-22

## Plan

| ID | Task | Status |
|---|---|---|
| T-01 | Survey the tooling | IN PROGRESS |
| T-02 | Decide packaging | DONE |
| T-03 | Build the skill | DONE |

## Intel

- Task notes, as imported from the plan: `missions/operational/M-DOT-CLAUDE-04/task-notes.md`
