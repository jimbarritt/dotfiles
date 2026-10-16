# M-DOT-CLAUDE-05: task notes

Imported from `doc/planning/plan.md` in the dotfiles repo, heading "Delta: Permission Config Fixes". The bullets are as written in the plan.

### T-01: Carve am-dotfiles-base out of the bin/ deny rule
- TODO — `home/claude/settings.json` denies `Bash(./bin/*)` and `Bash(bin/*)`, but repo `CLAUDE.md` says `am-dotfiles-base` "exists for Claude to run". Deny rules are absolute and cannot be overridden by an `allow` entry, so the documented exception does not work
- TODO — Preferred fix: move the script out of `bin/` (e.g. `tools/am-dotfiles-base`) and symlink `bin/am-dotfiles-base` to it, so the blanket deny stays intact for genuinely-destructive scripts while Claude invokes the non-denied path
- TODO — Alternative considered and rejected: enumerating denies per-script (30+ scripts, and new ones would default to allowed)
- TODO — Update repo `CLAUDE.md` to point at whichever path Claude should invoke

### T-02: Extend the blocked git command list
- ✓ DONE — `git checkout` added to `home/claude/hooks/pre-tool-use-filter.sh`, in the destructive-git section alongside `reset --hard`, `rebase` and `clean -f`
  - Verified blocked: `git checkout main`, `git checkout -- file`, `git checkout -b`, `cd /tmp && git checkout .`, bare `git checkout`
  - Verified still allowed: `git status`, `git log`, `git diff`
- TODO — Decide whether to also block `git switch` and `git restore` — the modern split of `checkout`, currently an unguarded route to the same destructive behaviour
