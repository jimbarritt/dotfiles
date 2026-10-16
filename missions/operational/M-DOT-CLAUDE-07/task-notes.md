# M-DOT-CLAUDE-07: task notes

Imported from `doc/planning/plan.md` in the dotfiles repo, heading "Delta: Per-repo Agent Config". The bullets are as written in the plan.

Follow-up to Delta: Claude Instruction Refinements, Task 2. The global git-commit guard
is gone; each repo now needs its own stance, set explicitly rather than left unset by
default.

### T-01: Repo-init skill for autonomy questions
- TODO — Build a skill that runs the first time Claude works in a repo with no local Claude config yet, and asks setup questions, starting with whether to allow autonomous `git commit` (and push) in that repo
- TODO — Decide what else belongs in that first-run question set (e.g. PR-only workflow, destructive-command allowances)
- TODO — Decide where the answers get written: project `CLAUDE.md`, `.claude/settings.json`, or both
