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

## Step 2: Understand the project

Read enough to write a meaningful plan skeleton:
- `CLAUDE.md` (if it exists)
- `README.md` (if it exists)
- Top-level directory structure (`ls`)
- Any existing `doc/` files

If `$ARGUMENTS` was provided, use it as the project description.

## Step 3: Create the directory

```bash
mkdir -p doc/planning
```

## Step 4: Write the plan file

Use this structure:

```markdown
# {Project Name} — Implementation Plan

## ── WHAT'S NEXT ──────────────────────────────────────────────────────────
**Next:** Action 1.1 — {first action title}
**Sub-doc:** (none)
**Blockers:** None
─────────────────────────────────────────────────────────────────────────────

## Phase 1: {Phase Name}

### Action 1.1: {Action Title}
- TODO — {description}

### Action 1.2: {Action Title}
- TODO — {description}

## Phase 2: {Phase Name}

### Action 2.1: {Action Title}
- TODO — {description}

---

## Implementation Notes

### Architecture
{Brief notes on key decisions, constraints, or design choices — leave blank if unknown}
```

Populate the phases and actions based on what you learned about the project. If you cannot infer a meaningful breakdown, write 2–3 placeholder phases with clearly labelled `TODO` actions and tell the user to fill them in.

## Step 5: Confirm

Tell the user:
- Where the file was created
- How many phases/actions were scaffolded
- That they can run `/update-plan` after each session to keep it current
