---
name: resume-plan
description: Restart the plan session timer after a pause
allowed-tools: Bash
disable-model-invocation: false
---

# Resume Plan

Restart the plan session clock after `/pause-plan`.

A paused timer stays paused until this runs — including across sessions — so the
break is excluded however long it lasts.

## Step 1: Resume the timer

```
sh ~/.claude/skills/plan-format/plan-timer.sh resume
```

It prints `state=`, `elapsed=` (time banked before the pause) and `break=` (how
long the pause lasted).

If it reports `state=running`, the timer was already going — nothing was paused.
Say so and continue.

If it reports `state=none`, there is no timer at all. Tell the user the session
was not started via `load-plan`, and that running `/load-plan` will start one.

## Step 2: Confirm and stand down

One line: work resumed, the banked time, and the length of the break. Then end
the turn and wait for the user to say what to work on. Do not pick up the
plan's What's Next task, inspect the repo, or begin any work.
