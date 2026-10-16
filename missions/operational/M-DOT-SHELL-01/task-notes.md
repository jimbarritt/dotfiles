# M-DOT-SHELL-01: task notes

Imported from `doc/planning/plan.md` in the dotfiles repo, heading "Delta: Shell Helpers". The bullets are as written in the plan.

### T-01: plan
- ✓ DONE — `plan()` in `home/zshrc` opens the current project's plan in Marq
  - Search order matches the `load-plan` skill: `doc/planning/plan.md`, `doc/plan.md`, `~/.planning/{project}/plan.md`
  - Root from `git rev-parse --show-toplevel` falling back to `$PWD`, so it works from any subdirectory
  - Implemented repo-local-first, diverging from the user's "global dir first" phrasing; flagged, not corrected

### T-02: copypath
- ✓ DONE — `copypath()` in `home/zshrc` copies an absolute path to the clipboard
  - Defaults to `.`; uses `printf '%s'` so no trailing newline reaches the clipboard; echoes the path
  - `realpath` resolves symlinks, so paths under `~/.config` report their dotfiles location

### T-03: readme
- ✓ DONE — `readme()` in `home/zshrc` opens the nearest README, walking upwards
  - Stops at a git root and reports `could not find readme in this git repo`; with no repo anywhere it walks to `/` and reports `cannot find readme`
  - Detects `.git` as a file as well as a directory, covering worktrees and submodules
  - Chosen as a zshrc function rather than a `bin/` script, alongside `plan` and `copypath`
