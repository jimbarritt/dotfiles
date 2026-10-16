# M-DOT-GIT-02: task notes

Imported from `doc/planning/plan.md` in the dotfiles repo, heading "Delta: Git Config Refinements". The bullets are as written in the plan.

### T-01: Exclude bots from git lol
- ✓ DONE — `lol` alias in `home/gitconfig` now filters bot authors via `--perl-regexp` and a negative lookahead
  - `\\[` is required in the config file — git rejects `\[` as a bad escape
  - Excludes any `*[bot]` plus `dependabot`, `renovate`, `github-actions`, `semantic-release`; `Bob Botham` and `Abbot Smith` still show
  - Confirmed the alias contains nothing work-specific, in response to a public-repo scrub question
