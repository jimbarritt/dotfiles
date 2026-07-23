---
name: init-plan
description: Initialise a new doc/planning/plan.md for the project with the standard structure
argument-hint: "[optional: brief description of the project/goal]"
allowed-tools: Read, Write, Bash, Glob, Grep
disable-model-invocation: false
---

# Init Plan

Create a new plan file for this project using the standard structure.

## Step 1: Ask where to store the plan

Read `../plan-format/PLAN-FORMAT.md`'s "Storage Location" section (relative to this skill's own directory) for the two options.

Determine the project name: `basename "$(git rev-parse --show-toplevel 2>/dev/null || pwd)"`.

Ask the user whether to store the plan:
- **Locally (default):** `doc/planning/plan.md`, tracked in this repo.
- **In the home directory:** `~/.planning/{project-name}/plan.md` — use this if you don't own the repo and don't want to add a `doc/planning/` directory to it.

Set **the plan root** to `doc/planning/` or `~/.planning/{project-name}/` accordingly, and use it for every path in the steps below.

## Step 2: Check for an existing plan

- If `{plan root}/plan.md` already exists: tell the user and stop — do not overwrite.
- If local was chosen and `doc/plan.md` exists: tell the user to run `/load-plan` instead, which will migrate it.

## Step 3: Read the shared plan format

Read `../plan-format/PLAN-FORMAT.md` in full. It defines the canonical structure and terminology (Deltas, Tasks, the Summary table, no ASCII separators) — follow it exactly.

## Step 4: Understand the project

Read enough to write a meaningful plan skeleton:
- `CLAUDE.md` (if it exists)
- `README.md` (if it exists)
- Top-level directory structure (`ls`)
- Any existing `doc/` files

If `$ARGUMENTS` was provided, use it as the project description.

## Step 5: Create the directory

```bash
mkdir -p {plan root}
```

## Step 6: Write the plan file

Follow the structure in `../plan-format/PLAN-FORMAT.md` verbatim: `## What's Next`, the `## Summary` table, then one `## Delta: {Name}` section per delta (titled, not numbered) with `### Task n: {Title}` items underneath (numbered within the delta), then `## Implementation Notes`.

Populate the Deltas and Tasks based on what you learned about the project. If you cannot infer a meaningful breakdown, write 2–3 placeholder Deltas with clearly labelled `TODO` Tasks and tell the user to fill them in. Make sure the Summary table's links and statuses match the body exactly.

## Step 7: Confirm

Tell the user:
- Where the file was created (local or home, with the full path)
- How many Deltas/Tasks were scaffolded
- That they can run `/update-plan` after each session to keep it current
