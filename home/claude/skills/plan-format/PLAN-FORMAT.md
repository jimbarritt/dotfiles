# Plan Format

Canonical structure for `doc/planning/plan.md`. The `init-plan`, `update-plan`, `load-plan`, and `prune-plan` skills must all follow this exactly — it is the single source of truth for the format, so fix drift here rather than in an individual skill.

## Terminology

- Top-level groups are called **Deltas** (not "Phases") and are identified by title, not number: `## Delta: {Name}`. Titles are the stable identifier — checkpoints and commits reference them, so avoid renaming.
- Items within a Delta are called **Tasks** (not "Actions"), numbered within their Delta only: `### Task 1: {Title}`, `### Task 2: {Title}` — restarting at 1 in each Delta. Reference a task as "{Delta name} / Task {n}" or just by its title.

## Storage Location

A plan lives in one of two places:

- **Local (default):** `doc/planning/plan.md` in the project repo — tracked in the repo's own history.
- **Home directory:** `~/.planning/{project-name}/plan.md`, where `{project-name}` is `basename "$(git rev-parse --show-toplevel 2>/dev/null || pwd)"`. Use this when working in a repo you don't own and don't want to add a `doc/planning/` directory to (no "ignore this dir" commit needed upstream).

Every skill's "find the plan file" step checks, in order:
1. `doc/planning/plan.md` — local, preferred
2. `doc/plan.md` — legacy local (migrate to `doc/planning/plan.md` on sight, per `update-plan`)
3. `~/.planning/{project-name}/plan.md` — home

Whichever is found first fixes **the plan root** for the rest of that skill's run: `doc/planning/` for either local case, or `~/.planning/{project-name}/` for the home case. Archive files (`archive/`, `archive/index.md`) always live under the plan root, the same way in both cases.

`init-plan` asks the user which location to use before creating anything (see its Step 1) — it does not infer this.

## Structure

```markdown
# {Project Name} — Implementation Plan

## What's Next

- **Next:** Task {n} — {task title} (Delta: {delta name})
- **Sub-doc:** {relative path, or "(none)"}
- **Blockers:** {list, or "None"}
- **Context:** {optional — link to the latest Checkpoint, or omit the field entirely}

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

- **Each `What's Next` field is its own top-level bullet** (`- **Next:** ...`, `- **Sub-doc:** ...`, etc.), never a bare `**Label:** ...` line. Consecutive non-blank lines with no bullet marker collapse into a single run-on paragraph under CommonMark — bullets are what keep each field on its own visual line regardless of renderer.
- **`What's Next` is a pointer, not a summary.** No rationale, no session narrative, no stacking multiple ideas into one run-on sentence. If reasoning matters, it belongs in the Task body or a `## Checkpoint` entry, linked via `**Context:**` — never inlined into `What's Next` itself.
  - `**Next:**` is normally a single line: `Task {n} — {task title} (Delta: {delta name})`.
  - If there are multiple undecided candidates, nest them as sub-bullets under the `**Next:**` bullet instead of cramming them into one sentence — each sub-bullet is a name + link, not a reason:
    ```markdown
    - **Next:** Undecided — see Checkpoint for context.
      - Reconciliation / Task 1
      - Credit Card Transaction Import / Task 3
      - The Gap / Task 1
    ```
  - Bad: `**Next:** Not yet decided. Delta X is done — every task in its table is complete. Candidates, not yet prioritised: Task A (because...), Task B (because...), or resuming Task C.`
- Don't invent extra top-level fields beyond `Next`/`Sub-doc`/`Blockers`/`Context`. If something else feels worth recording every session, it's checkpoint material, not a `What's Next` field.
- **Prefer sub-bullets over one dense paragraph.** A Task or Checkpoint entry that's accreting multiple facts (what was built, what was found, what's still open) should split them into separate indented sub-bullets under the main `- TODO —`/`- ✓ DONE —` line, one fact per sub-bullet, rather than one long comma/em-dash-stitched paragraph. The top-level bullet stays a short headline; detail hangs underneath it:
  ```markdown
  - ✓ DONE — {one-line headline of what shipped}
    - {supporting detail, one idea}
    - {another supporting detail, one idea}
  ```
- **No box-drawing / ASCII separator lines** anywhere in the plan (no `──────`, no `═════`). `What's Next` and `Checkpoint` blocks are plain `##` headings like any other section.
- The **Summary table** sits directly under `## What's Next`'s bullets, before the Delta sections. **Nothing else goes in between** — no dated history, no session narrative, no extra detail. `What's Next` is a short pointer, not a log; session detail belongs in a `## Checkpoint` entry or in the relevant Task's body, never appended under `What's Next` itself. One row per Task in the Summary table. Only fill the Delta cell on that Delta's first row (leave it blank on subsequent rows for the same Delta) so the table reads as visually grouped.
- Both table columns link to the matching heading using GitHub's anchor-slugify rules: lowercase, spaces → hyphens, strip punctuation other than hyphens. E.g. `## Delta: Foo Bar` → `#delta-foo-bar`; `### Task 1: Foo Bar` → `#task-1-foo-bar`. If two headings produce the same slug (e.g. `Task 1` in two Deltas with identical titles), GitHub suffixes the later ones `-1`, `-2`... — link accordingly.
- The Status column must exactly track the body: `TODO` (no bullet done yet), `IN PROGRESS` (some bullets done, some not), or `✓ DONE` (every bullet under the Task is done).
- Whenever a Task's TODO/DONE state changes in the body, the matching Summary table row must be updated in the same edit — the table and the body must never drift apart.
- Checkpoints stack newest-last, each as its own `## Checkpoint: Session {YYYY-MM-DD}` heading, placed before `## Implementation Notes`.
- **Old formats to migrate on sight**: "Phase"/"Action" terminology, boxed `WHAT'S NEXT`/`CHECKPOINT` separators, numbered Deltas (`## Delta 1: ...`) and delta-prefixed Task numbers (`### Task 1.1: ...`). Numbered Deltas became unmanageable once pruning archived completed ones — the survivors' numbers had permanent gaps.

## Archiving

Fully completed Deltas (every Task `✓ DONE`) can be pruned from the live plan by the `prune-plan` skill:

- Pruned `## Delta: ...` sections move verbatim to `{plan root}/archive/{YYYY-MM-DD-HHMM}-archive.md` — never reworded.
- `{plan root}/archive/index.md` is the archive index: one line per prune run — `- {YYYY-MM-DD}: {Delta names} → [{file}]({file})`.
- Archived Deltas' rows are removed from the Summary table, and a single link line sits directly under the table: `Archived Deltas: see the [archive index](archive/index.md)`.
