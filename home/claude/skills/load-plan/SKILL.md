---
name: load-plan
description: Read doc/planning/plan.md, brief on current state and next step, and suggest continuing
argument-hint: ""
allowed-tools: Read, Bash, Glob, Grep
disable-model-invocation: false
---

# Load Plan

Read the project plan, orient on current state, and brief the user so work can continue.

## Step 1: Find the plan file

Check in order:
1. `doc/planning/plan.md` — preferred location
2. `doc/plan.md` — legacy location

If found at `doc/plan.md`: note it and mention the user may want to run `/update-plan` to migrate it to `doc/planning/`.

If neither exists: tell the user no plan file was found and stop.

## Step 2: Read the What's Next section

The plan file should begin (after the title line) with a `## What's Next` section, followed by a `## Summary` table. Read them to identify:
- The next Delta/Task
- Whether there is a linked sub-document
- The overall shape of progress from the Summary table's Status column

If the plan still uses old terminology ("Phase"/"Action") or the old boxed separators, mention to the user that it's due a migration — `/update-plan` will do it automatically on the next run.

## Step 3: Read the full plan

Read the complete plan file to understand overall progress — how many Deltas exist, how many TODOs remain, what was recently completed (from the latest Checkpoint section).

## Step 4: Read the sub-document for the next step (if any)

If the What's Next section references a sub-document (not "(none)"), read that file too. It will contain the detailed design or breakdown for the next Task.

## Step 5: Brief the user

Give a concise briefing — 3 sections, no waffle:

**Progress:** One sentence on overall state (e.g. "Deltas 1–4 complete, Delta 5 in progress — 3 TODOs remaining").

**Last session:** 2–3 bullets from the most recent Checkpoint summarising what was done.

**Next up:** What the What's Next section says, plus any relevant detail from the sub-doc if you read one.

## Step 6: Suggest continuing

End your response with an autosuggestion prompt (not a question the user has to type — phrase it as a natural continuation offer):

> Shall we continue with {next action title}, or is there a new direction?
