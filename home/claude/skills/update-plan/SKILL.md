---
name: update-plan
description: Update doc/planning/plan.md with completed work, refresh the WHAT'S NEXT pointer, and append a checkpoint
argument-hint: "[optional note about what was done this session]"
allowed-tools: Read, Write, Edit, Bash, Glob, Grep
disable-model-invocation: false
---

# Update Plan

Update the project plan to reflect completed work, refresh the WHAT'S NEXT pointer, and append a dated checkpoint.

## Step 1: Find the plan file

Check in order:
1. `doc/planning/plan.md` — preferred location
2. `doc/plan.md` — legacy location

If found at `doc/plan.md` (and `doc/planning/` does not already have a plan.md):
- Create `doc/planning/` if it doesn't exist
- Move the file: `git mv doc/plan.md doc/planning/plan.md`
- Tell the user it was migrated

If neither exists: tell the user no plan file was found and stop.

## Step 2: Read the plan

Read the full plan file. Also read any sub-documents linked from actions that are being marked complete (look for lines like `Design doc: \`path/to/file.md\``).

## Step 3: Update TODO → DONE in the plan body

Based on what was accomplished in this conversation:
- Change `- TODO` items to `- ✓ DONE` for completed actions
- If a completed action has a linked sub-document, open it and mark completed items there too
- Do not invent completions — only mark items genuinely done in this session

## Step 4: Identify the next step

Look at the plan after your updates. Find the first remaining `TODO` item across all phases. Note:
- The action number and title
- Whether it has a linked sub-document
- Any blockers or open questions

## Step 5: Refresh the WHAT'S NEXT pointer

The plan file must start (after the `# Title` line) with a pointer block in this format:

```markdown
## ── WHAT'S NEXT ──────────────────────────────────────────────────────────
**Next:** {Action X.Y} — {action title}
**Sub-doc:** {relative path to sub-doc, or "(none)"}
**Blockers:** {list blockers, or "None"}
─────────────────────────────────────────────────────────────────────────────
```

Update this block with the current next step. If the pointer block doesn't exist yet, insert it after the first `#` heading line.

## Step 6: Append a checkpoint

Append a new CHECKPOINT block at the bottom of the file (before `## Implementation Notes` if that section exists, otherwise at the very end):

```markdown
## ── CHECKPOINT: Session {YYYY-MM-DD} ──────────────────────────────────────

**What was completed this session:**
{bullet list — specific, file-level where useful}

**State of the project:**
{2–4 sentences: what works, test status, anything notably broken}

**Immediate next priorities:**
{numbered list of next 3–5 items}

─────────────────────────────────────────────────────────────────────────────
```

Get the date by running `date +%Y-%m-%d`.

## Step 7: Trim old checkpoints

Count the CHECKPOINT blocks in the file. If there are more than 10, remove the oldest ones (earliest dates) until only 10 remain. The `## Implementation Notes` section and anything after it must never be removed.

## Step 8: Write the file and confirm

Write the updated plan file. Then confirm to the user:
- What TODOs were marked done
- What the new WHAT'S NEXT pointer points to
- That the checkpoint was appended
