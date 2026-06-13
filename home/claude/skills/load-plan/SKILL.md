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

## Step 2: Read the WHAT'S NEXT pointer

The plan file should begin (after the title line) with a `── WHAT'S NEXT ──` block. Read it to identify:
- The next action
- Whether there is a linked sub-document

## Step 3: Read the full plan

Read the complete plan file to understand overall progress — how many phases exist, how many TODOs remain, what was recently completed (from the latest CHECKPOINT block).

## Step 4: Read the sub-document for the next step (if any)

If the WHAT'S NEXT pointer references a sub-document (not "(none)"), read that file too. It will contain the detailed design or breakdown for the next action.

## Step 5: Brief the user

Give a concise briefing — 3 sections, no waffle:

**Progress:** One sentence on overall state (e.g. "Phases 1–4c complete, Phase 5 in progress — 3 TODOs remaining").

**Last session:** 2–3 bullets from the most recent CHECKPOINT summarising what was done.

**Next up:** What the WHAT'S NEXT pointer says, plus any relevant detail from the sub-doc if you read one.

## Step 6: Suggest continuing

End your response with an autosuggestion prompt (not a question the user has to type — phrase it as a natural continuation offer):

> Shall we continue with {next action title}, or is there a new direction?
