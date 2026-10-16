# Mission: Diff viewer readability

| Field | Value |
|---|---|
| ID | M-DOT-GIT-01 |
| Status | IN PROGRESS |
| Blocked by | none |

## Objective

- git-delta diffs are readable in the dark and the light theme
- The hunk header does not mislead
- Gutter line numbers are readable on the green background

## Purpose

git-delta colours were broken in both themes. Research: `doc/gitconfig.md`.

## Decisions

- `bin/presentation-mode` switches delta through a `~/.gitconfig-presentation` overlay. Jim, 2026-08-22
- Dark values: minus 40, minus-emph 65, plus 61, plus-emph 80 against a background of 22. Jim, 2026-08-22
- The hunk header line number is hidden. Jim, 2026-08-22
- `*.md` uses git's `markdown` driver, and `*.txt` uses a never-matching `xfuncname`. Jim, 2026-08-22

## Open questions

- Enable git's built-in diff drivers for go, rust, python, java and cpp?
- Does the light overlay need the same gutter treatment?
- Are the gutter values confirmed, so they can go in `doc/gitconfig.md`?

## Plan

| ID | Task | Status |
|---|---|---|
| T-01 | Retune diff colours for both themes | IN PROGRESS |
| T-02 | Fix the hunk header | IN PROGRESS |
| T-03 | Brighten the gutter line numbers | IN PROGRESS |

## Intel

- Task notes, as imported from the plan: `missions/operational/M-DOT-GIT-01/task-notes.md`
