# Plan Format

Canonical structure for `doc/planning/plan.md`. The `init-plan`, `update-plan`, and `load-plan` skills must all follow this exactly — it is the single source of truth for the format, so fix drift here rather than in an individual skill.

## Terminology

- Top-level groups are called **Deltas** (not "Phases") — `Delta 1`, `Delta 2`, ...
- Items within a Delta are called **Tasks** (not "Actions") — numbered `{delta}.{n}`, e.g. `1.1`, `1.2`.

## Structure

```markdown
# {Project Name} — Implementation Plan

## What's Next

**Next:** Task {X.Y} — {task title}
**Sub-doc:** {relative path, or "(none)"}
**Blockers:** {list, or "None"}

## Summary

| Delta | Task | Status |
|-------|------|--------|
| [Delta 1: {Name}](#delta-1-name) | [1.1 {Task title}](#task-11-task-title) | ✓ DONE |
| | [1.2 {Task title}](#task-12-task-title) | TODO |
| [Delta 2: {Name}](#delta-2-name) | [2.1 {Task title}](#task-21-task-title) | TODO |

## Delta 1: {Name}

### Task 1.1: {Task Title}
- TODO — {description}

### Task 1.2: {Task Title}
- TODO — {description}

## Delta 2: {Name}

### Task 2.1: {Task Title}
- TODO — {description}

## Checkpoint: Session {YYYY-MM-DD}

**What was completed this session:**
{bullet list — specific, file-level where useful}

**State of the project:**
{2–4 sentences}

**Immediate next priorities:**
{numbered list of next 3–5 items}

---

## Implementation Notes

### Architecture
{Brief notes on key decisions, constraints, or design choices}
```

## Rules

- **No box-drawing / ASCII separator lines** anywhere in the plan (no `──────`, no `═════`). `What's Next` and `Checkpoint` blocks are plain `##` headings like any other section.
- The **Summary table** sits directly under `## What's Next`, before the Delta sections. One row per Task. Only fill the Delta cell on that Delta's first row (leave it blank on subsequent rows for the same Delta) so the table reads as visually grouped.
- Both table columns link to the matching heading using GitHub's anchor-slugify rules: lowercase, spaces → hyphens, strip punctuation other than hyphens. E.g. `## Delta 1: Foo Bar` → `#delta-1-foo-bar`; `### Task 1.1: Foo Bar` → `#task-11-foo-bar`.
- The Status column must exactly track the body: `TODO` (no bullet done yet), `IN PROGRESS` (some bullets done, some not), or `✓ DONE` (every bullet under the Task is done).
- Whenever a Task's TODO/DONE state changes in the body, the matching Summary table row must be updated in the same edit — the table and the body must never drift apart.
- Checkpoints stack newest-last, each as its own `## Checkpoint: Session {YYYY-MM-DD}` heading, placed before `## Implementation Notes`.
