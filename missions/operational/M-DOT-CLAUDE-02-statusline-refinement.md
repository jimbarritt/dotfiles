# Mission: Statusline refinement

| Field | Value |
|---|---|
| ID | M-DOT-CLAUDE-02 |
| Status | IN PROGRESS |
| Blocked by | none |

## Objective

- The statusline shows extra usage correctly when the five-hour limit is passed
- The plan skills use one readable format

## Purpose

Make the statusline signals and the plan skills correct after real use.

## Decisions

- One canonical plan format in `home/claude/skills/plan-format/PLAN-FORMAT.md`, with Deltas and Tasks and a Summary table. Jim, 2026-07-06

## Open questions

- Can `rate_limits.five_hour.used_percentage` exceed 100, or is it capped?
- If it is capped, which field signals extra usage?

## Plan

| ID | Task | Status |
|---|---|---|
| T-01 | Verify extra usage detection | TODO |
| T-02 | Plan skill refinement | DONE |

## Intel

- Task notes, as imported from the plan: `missions/operational/M-DOT-CLAUDE-02/task-notes.md`
- Statusline: `missions/operational/M-DOT-CLAUDE-02/intel/statusline.md`
