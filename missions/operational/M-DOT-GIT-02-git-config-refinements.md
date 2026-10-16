# Mission: Git config refinements

| Field | Value |
|---|---|
| ID | M-DOT-GIT-02 |
| Status | DONE |
| Blocked by | none |

## Objective

- `git lol` excludes bot authors

## Purpose

Remove bot noise from the log view.

## Decisions

- Exclude any `*[bot]` plus `dependabot`, `renovate`, `github-actions` and `semantic-release`. Jim, 2026-08-18

## Plan

| ID | Task | Status |
|---|---|---|
| T-01 | Exclude bots from git lol | DONE |

## Intel

- Task notes, as imported from the plan: `missions/operational/M-DOT-GIT-02/task-notes.md`
