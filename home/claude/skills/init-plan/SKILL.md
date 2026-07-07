---
name: init-plan
description: Initialise a new doc/planning/plan.md for the project with the standard structure
argument-hint: "[optional: brief description of the project/goal]"
allowed-tools: Read, Write, Bash, Glob, Grep
disable-model-invocation: false
---

# Init Plan

Create a new `doc/planning/plan.md` for this project using the standard structure.

## Step 1: Check for an existing plan

- If `doc/planning/plan.md` already exists: tell the user and stop — do not overwrite.
- If `doc/plan.md` exists: tell the user to run `/load-plan` instead, which will migrate it.

## Step 2: Read the shared plan format

Read `../plan-format/PLAN-FORMAT.md` (relative to this skill's own directory). It defines the canonical structure and terminology (Deltas, Tasks, the Summary table, no ASCII separators) — follow it exactly.

## Step 3: Understand the project

Read enough to write a meaningful plan skeleton:
- `CLAUDE.md` (if it exists)
- `README.md` (if it exists)
- Top-level directory structure (`ls`)
- Any existing `doc/` files

If `$ARGUMENTS` was provided, use it as the project description.

## Step 4: Create the directory

```bash
mkdir -p doc/planning
```

## Step 5: Write the plan file

Follow the structure in `../plan-format/PLAN-FORMAT.md` verbatim: `## What's Next`, the `## Summary` table, then one `## Delta: {Name}` section per delta (titled, not numbered) with `### Task n: {Title}` items underneath (numbered within the delta), then `## Implementation Notes`.

Populate the Deltas and Tasks based on what you learned about the project. If you cannot infer a meaningful breakdown, write 2–3 placeholder Deltas with clearly labelled `TODO` Tasks and tell the user to fill them in. Make sure the Summary table's links and statuses match the body exactly.

## Step 6: Confirm

Tell the user:
- Where the file was created
- How many Deltas/Tasks were scaffolded
- That they can run `/update-plan` after each session to keep it current
