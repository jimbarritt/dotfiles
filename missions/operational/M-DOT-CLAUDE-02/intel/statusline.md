# Statusline

Imported from the Implementation Notes of `doc/planning/plan.md`. Used by: M-DOT-CLAUDE-02.

- Statusline script uses `jq 'has("rate_limits")'` to detect subscription vs API mode
- Rate limit reset timestamps come as Unix timestamps; converted to HH:MM (5h) and DD/MM (7d) via `date -r`
