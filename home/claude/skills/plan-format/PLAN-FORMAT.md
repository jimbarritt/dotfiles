# Plan Format

Canonical structure for `doc/planning/plan.md`. The `init-plan`, `update-plan`, `load-plan`, and `prune-plan` skills must all follow this exactly — it is the single source of truth for the format, so fix drift here rather than in an individual skill.

## Terminology

- Top-level groups are called **Deltas** (not "Phases") and are identified by title, not number: `## Delta: {Name}`. Titles are the stable identifier — checkpoints and commits reference them, so avoid renaming.
- Items within a Delta are called **Tasks** (not "Actions"), numbered within their Delta only: `### Task 1: {Title}`, `### Task 2: {Title}` — restarting at 1 in each Delta. Reference a task as "{Delta name} / Task {n}" or just by its title.

## Structure

```markdown
# {Project Name} — Implementation Plan

## What's Next

**Next:** Task {n} — {task title} (Delta: {delta name})
**Sub-doc:** {relative path, or "(none)"}
**Blockers:** {list, or "None"}

## Summary

| Delta | Task | Status |
|-------|------|--------|
| [Delta: {Name}](#delta-name) | [1. {Task title}](#task-1-task-title) | ✓ DONE |
| | [2. {Task title}](#task-2-task-title) | TODO |
| [Delta: {Other Name}](#delta-other-name) | [1. {Task title}](#task-1-task-title-1) | TODO |

Archived Deltas: see the [archive index](archive/index.md)

## Delta: {Name}

### Task 1: {Task Title}
- TODO — {description}

### Task 2: {Task Title}
- TODO — {description}

## Delta: {Other Name}

### Task 1: {Task Title}
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
- The **Summary table** sits directly under `## What's Next`'s `**Next:**`/`**Sub-doc:**`/`**Blockers:**` lines, before the Delta sections. **Nothing else goes in between** — no dated history, no session narrative, no extra detail. `What's Next` is a short pointer, not a log; session detail belongs in a `## Checkpoint` entry or in the relevant Task's body, never appended under `What's Next` itself. One row per Task in the Summary table. Only fill the Delta cell on that Delta's first row (leave it blank on subsequent rows for the same Delta) so the table reads as visually grouped.
- Both table columns link to the matching heading using GitHub's anchor-slugify rules: lowercase, spaces → hyphens, strip punctuation other than hyphens. E.g. `## Delta: Foo Bar` → `#delta-foo-bar`; `### Task 1: Foo Bar` → `#task-1-foo-bar`. If two headings produce the same slug (e.g. `Task 1` in two Deltas with identical titles), GitHub suffixes the later ones `-1`, `-2`... — link accordingly.
- The Status column must exactly track the body: `TODO` (no bullet done yet), `IN PROGRESS` (some bullets done, some not), or `✓ DONE` (every bullet under the Task is done).
- Whenever a Task's TODO/DONE state changes in the body, the matching Summary table row must be updated in the same edit — the table and the body must never drift apart.
- Checkpoints stack newest-last, each as its own `## Checkpoint: Session {YYYY-MM-DD}` heading, placed before `## Implementation Notes`.
- **Old formats to migrate on sight**: "Phase"/"Action" terminology, boxed `WHAT'S NEXT`/`CHECKPOINT` separators, numbered Deltas (`## Delta 1: ...`) and delta-prefixed Task numbers (`### Task 1.1: ...`). Numbered Deltas became unmanageable once pruning archived completed ones — the survivors' numbers had permanent gaps.

## Archiving

Fully completed Deltas (every Task `✓ DONE`) can be pruned from the live plan by the `prune-plan` skill:

- Pruned `## Delta: ...` sections move verbatim to `doc/planning/archive/{YYYY-MM-DD-HHMM}-archive.md` — never reworded.
- `doc/planning/archive/index.md` is the archive index: one line per prune run — `- {YYYY-MM-DD}: {Delta names} → [{file}]({file})`.
- Archived Deltas' rows are removed from the Summary table, and a single link line sits directly under the table: `Archived Deltas: see the [archive index](archive/index.md)`.
