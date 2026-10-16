# Mission: Permission config fixes

| Field | Value |
|---|---|
| ID | M-DOT-CLAUDE-05 |
| Status | IN PROGRESS |
| Blocked by | none |

## Objective

- Claude can run `am-dotfiles-base` while the `bin/` deny rule stays in place
- The pre-tool-use filter blocks every route to destructive git checkout behaviour

## Purpose

The `Bash(bin/*)` deny rule makes the documented `am-dotfiles-base` exception in the repo `CLAUDE.md` non-functional. Deny rules cannot be overridden by an allow entry.

## Decisions

- `git checkout` is blocked in `home/claude/hooks/pre-tool-use-filter.sh`. Jim, 2026-08-18
- Per-script deny rules rejected: 30+ scripts, and new ones default to allowed. Jim, 2026-08-02
- Preferred fix: move the script out of `bin/` and symlink `bin/am-dotfiles-base` to it. Jim, 2026-08-02

## Open questions

- Block `git switch` and `git restore` as well?
- Which path does the repo `CLAUDE.md` point at after the move?

## Plan

| ID | Task | Status |
|---|---|---|
| T-01 | Carve am-dotfiles-base out of the bin/ deny rule | TODO |
| T-02 | Extend the blocked git command list | IN PROGRESS |

## Intel

- Task notes, as imported from the plan: `missions/operational/M-DOT-CLAUDE-05/task-notes.md`
- Claude permissions: `missions/operational/M-DOT-CLAUDE-05/intel/claude-permissions.md`
