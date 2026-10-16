# Claude permissions

Imported from the Implementation Notes of `doc/planning/plan.md`. Used by: M-DOT-CLAUDE-05.

- `git stash` is denied globally — added after Sonnet stashed commits and lost track of them
- `pre-tool-use-filter.sh` runs on every Bash PreToolUse — blocks destructive ops, broad find, git push/commit/add, sudo, pipe-to-shell etc.
- Claude Code permission patterns use gitignore-spec anchoring: a single `/` prefix means "relative to project root", NOT filesystem root. Absolute filesystem paths require `//` (double slash) prefix — e.g. `Read(//tmp/**)`, `Read(//private/tmp/**)`. Bare `**` and `/**` are both project-scoped.
- macOS `/tmp` is a symlink to `/private/tmp`. Allow rules check BOTH the symlink path and the resolved path — both must match. Use `Read(//tmp/**)` + `Read(//private/tmp/**)` (and `Write(...)` equivalents) to cover `/tmp` fully.
