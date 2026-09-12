---
name: plan-format
description: Reference material for the project-plan format. Not invoked directly — the init-plan, load-plan, update-plan, prune-plan, pause-plan and resume-plan skills read PLAN-FORMAT.md and plan-timer.sh from this directory. Only load this if you need the plan format specification itself.
---

# Plan Format

This directory is shared reference material, not a workflow skill.

- `PLAN-FORMAT.md` — the specification for `doc/planning/plan.md`: storage locations,
  Delta/Task structure, the What's Next section, the Summary table, and Checkpoints.
- `plan-timer.sh` — session timer helper (`start`, `pause`, `resume`, `stop`) used by the
  plan skills to record elapsed session time.

If you have been asked about the plan format, read `PLAN-FORMAT.md`. Otherwise prefer
`init-plan`, `load-plan`, `update-plan` or `prune-plan`, which use this material themselves.
