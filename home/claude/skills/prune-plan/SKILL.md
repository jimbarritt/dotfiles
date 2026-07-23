---
name: prune-plan
description: Archive fully completed Deltas from doc/planning/plan.md into doc/planning/archive/, keeping the live plan short
argument-hint: ""
allowed-tools: Read, Write, Edit, Bash, Glob, Grep
disable-model-invocation: false
---

# Prune Plan

Move fully completed Deltas out of the live plan into a timestamped archive file, so the plan stays focused on open work.

## Step 1: Find the plan file

Check in order (per `../plan-format/PLAN-FORMAT.md`'s "Storage Location" section):
1. `doc/planning/plan.md` — local, preferred
2. `doc/plan.md` — legacy local
3. `~/.planning/{project-name}/plan.md` — home, where `{project-name}` is `basename "$(git rev-parse --show-toplevel 2>/dev/null || pwd)"`

If none exist, tell the user and stop.

Whichever is found sets **the plan root** for the rest of this run (`doc/planning/` or `~/.planning/{project-name}/`) — use it in place of `doc/planning/` in every step below.

## Step 2: Read the shared plan format

Read `../plan-format/PLAN-FORMAT.md` (relative to this skill's own directory) — the archive convention is defined there. If the plan is in an old format ("Phase"/"Action", boxed separators, or numbered Deltas like `## Delta 1:`), tell the user to run `/update-plan` first to migrate it, and stop.

## Step 3: Identify prunable Deltas

A Delta is prunable when **every** Task under it is `✓ DONE` (every bullet done — `Note:` lines don't block pruning). A Delta with any `TODO` or `IN PROGRESS` Task stays.

If nothing is prunable, tell the user and stop.

## Step 4: Write the archive file

```bash
mkdir -p {plan root}/archive
date +%Y-%m-%d-%H%M
```

Create `{plan root}/archive/{timestamp}-archive.md` containing:

```markdown
# {Project Name} — Archived Deltas ({YYYY-MM-DD})

Archived from `{plan root}/plan.md`.

{the pruned `## Delta: ...` sections, moved verbatim}
```

Move the sections verbatim — do not reword or summarise.

## Step 5: Update the archive index

Create or update `{plan root}/archive/index.md`:

```markdown
# {Project Name} — Archive Index

- {YYYY-MM-DD}: {Delta names, e.g. Claude Tooling, Copilot Compatibility} → [{timestamp}-archive.md]({timestamp}-archive.md)
```

Append one line per prune run, newest last.

## Step 6: Update the live plan

- Remove the pruned `## Delta: ...` sections from the body
- Remove their rows from the Summary table
- Ensure a single link line sits directly under the Summary table:

```markdown
Archived Deltas: see the [archive index](archive/index.md)
```

Leave checkpoints and `## Implementation Notes` untouched — checkpoints have their own 10-entry trim in `/update-plan`.

## Step 7: Confirm

Tell the user:
- Which Deltas were archived and to which file
- What remains open in the live plan
