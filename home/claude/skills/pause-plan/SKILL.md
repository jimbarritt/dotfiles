---
name: pause-plan
description: Pause the plan session timer so a break is not counted as working time
argument-hint: "[optional note about why work is pausing]"
allowed-tools: Bash
disable-model-invocation: false
---

# Pause Plan

Stop the plan session clock. Time spent between now and the next prompt is not
counted towards the `Time spent` figure `update-plan` writes into the checkpoint.

## Step 1: Pause the timer

Run the shared helper, passing `$ARGUMENTS` as the note if one was given:

```
sh ~/.claude/skills/plan-format/plan-timer.sh pause "$ARGUMENTS"
```

It prints `state=`, `elapsed=` and, when a note was supplied, `note=`.

If it reports `state=none`, no timer is running — the session was not started via
`load-plan`, or `update-plan` has already consumed the timer. Say so and stop.

## Step 2: Confirm

One line: work is paused, the time banked so far (`elapsed=`), and the note if
there was one. For example:

> Paused at ~2h 15m (note: lunch). `/resume-plan` restarts the clock.

Mention `/resume-plan` — the timer stays paused until it is run, including across
sessions, so an unresumed pause silently undercounts the next session's time.

## Step 3: Stand down

Do not start, continue, or plan any further work in this turn. Pausing means the
user is stopping — answer nothing else and wait.

## Auto-resume (not enabled)

`home/claude/hooks/plan-timer-resume.sh` would restart the clock automatically on
the user's next message, removing the need for `/resume-plan`. It is written and
tested but deliberately not registered in `settings.json` — running it on every
prompt was judged excessive. See the comment at the top of that script for how to
enable it.
