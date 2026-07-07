---
name: update-plan
description: Update doc/planning/plan.md with completed work, refresh the WHAT'S NEXT pointer, and append a checkpoint
argument-hint: "[optional note about what was done this session]"
allowed-tools: Read, Write, Edit, Bash, Glob, Grep
disable-model-invocation: false
---

# Update Plan

Update the project plan to reflect completed work, refresh the WHAT'S NEXT pointer, and append a dated checkpoint.

Deciding *what happened this session* requires the main conversation's context and stays with the current model. The mechanical file edit is delegated to a Haiku subagent — **except when the plan needs migrating** to the current format, which is judgement-heavy and must run inline in the current model (see Step 6).

## Step 1: Find the plan file

Check in order:
1. `doc/planning/plan.md` — preferred location
2. `doc/plan.md` — legacy location

If found at `doc/plan.md` (and `doc/planning/` does not already have a plan.md):
- Create `doc/planning/` if it doesn't exist
- Move the file: `git mv doc/plan.md doc/planning/plan.md`
- Tell the user it was migrated

If neither exists: tell the user no plan file was found and stop.

## Step 2: Read the shared plan format

Read `../plan-format/PLAN-FORMAT.md` (relative to this skill's own directory). It defines the canonical structure and terminology (Deltas, Tasks, the Summary table, no ASCII separators).

## Step 3: Read the plan and check its format

Read the full plan file. Also read any sub-documents linked from Tasks that are being marked complete (look for lines like `Sub-doc: \`path/to/file.md\``).

Decide: does the plan need **migrating**? It does if it uses any of the old formats listed in `PLAN-FORMAT.md`'s rules: "Phase"/"Action" terminology, boxed `WHAT'S NEXT`/`CHECKPOINT` separators, a missing Summary table, or numbered Deltas/delta-prefixed Task numbers (`## Delta 1:`, `### Task 1.1:`).

## Step 4: Compose the session summary (current model — do not delegate)

From the main conversation, work out:
- Which plan TODOs were genuinely completed this session (Delta name + Task + which bullets). Do not invent completions.
- The checkpoint content: what was completed (specific, file-level where useful), state of the project (2–4 sentences), immediate next priorities (3–5 items)
- The next step: the first remaining `TODO` across all Deltas — its Task title (and Delta name), any linked sub-document, any blockers
- Today's date (`date +%Y-%m-%d`)

If `$ARGUMENTS` was provided, incorporate it.

## Step 5: Apply the update

**If the plan needs migrating (per Step 3): do everything inline in the current model** — migration involves rewording historical text, renaming headings, and computing anchor slugs, which needs judgement. Rewrite the plan to conform to `PLAN-FORMAT.md` exactly, apply the Step 4 updates, and skip to Step 6.

**Otherwise, delegate the mechanical edit** to a background subagent with `model: haiku` (general-purpose). The prompt must contain everything the agent needs, since it starts cold:
- Absolute paths to the plan file and to `PLAN-FORMAT.md` (tell it to read both first and follow the format exactly)
- The full Step 4 summary: exact bullets to flip `- TODO` → `- ✓ DONE` (and any sub-doc items), the complete checkpoint text to append, and the new What's Next values
- The standing rules: update every Summary table row whose status changed (statuses per PLAN-FORMAT.md: `TODO` / `IN PROGRESS` / `✓ DONE` — the table must never drift from the body); append the checkpoint as `## Checkpoint: Session {date}` before `## Implementation Notes` (or at the end if that section doesn't exist), suffixing `b`, `c`... if a checkpoint for that date already exists; if there are then more than 10 checkpoints, remove the oldest until 10 remain — never touch `## Implementation Notes` or anything after it; mark only the listed items done — nothing else
- Tell it to report back what it changed

If the Agent tool is unavailable, do the same steps inline instead.

## Step 6: Confirm

When the update is applied (relay the agent's report if delegated), confirm to the user:
- What TODOs were marked done (and their Summary table rows updated)
- What the new What's Next section points to
- That the checkpoint was appended
- Whether the plan was migrated to the current format
