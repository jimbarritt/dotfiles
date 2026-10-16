# M-DOT-CLAUDE-03: task notes

Imported from `doc/planning/plan.md` in the dotfiles repo, heading "Delta: Plan Skill Iteration". The bullets are as written in the plan.

### T-01: Tweak plan skill based on continued use
- ✓ DONE — Gather user's specific desired tweaks to the plan skill (update-plan / init-plan / load-plan / plan-format) and apply them

### T-02: Session timer loose ends
- TODO — `home/claude/hooks/plan-timer-resume.sh` (auto-resume on the next prompt) is written and tested but deliberately unregistered in `settings.json` — enable it if manual `/resume-plan` proves annoying; snippet is in the script header
- TODO — Work out why `~/.claude/skills/plan-format` was never symlinked, which broke every plan skill's helper call with exit 127. `do.sh`'s `link_claude` globs `skills/*/` and should have created it — run `./do.sh link-claude` and check the result is consistent
