# M-DOT-CLAUDE-01: task notes

Imported from `doc/planning/archive` in the dotfiles repo, heading "Delta 1: Claude Tooling". The bullets are as written in the plan.

### T-01: Statusline — account-type toggle
- ✓ DONE — Remove token count from statusline
- ✓ DONE — Detect subscription vs API mode via `rate_limits` presence in JSON
- ✓ DONE — Show cost only on API mode, or on subscription when rate limit >= 100% (extra usage)
- ✓ DONE — Replace ctx percentage with absolute token count (`total_input_tokens + total_output_tokens`) with three-level warning (0–149k dim, 150–169k bold, 170k+ `!!value!!`)
- ✓ DONE — Adjust context token warning bands: dim <100k, bold 100–119k, `!!value!!` ≥120k
- ✓ DONE — Switch session cost display from GBP to USD, no decimal places
- Note: verifying the extra usage branch in practice is tracked as Task 3.1 in the live plan

### T-02: Plan skills
- ✓ DONE — Create `update-plan` skill (`home/claude/skills/update-plan/SKILL.md`)
- ✓ DONE — Create `load-plan` skill (`home/claude/skills/load-plan/SKILL.md`)
- ✓ DONE — Create `init-plan` skill (`home/claude/skills/init-plan/SKILL.md`)
- ✓ DONE — Symlink all three into `~/.claude/skills/`
- ✓ DONE — Add `Skill(*)` to global permissions to suppress approval prompt

### T-03: Claude config documentation
- ✓ DONE — Create `home/claude/README.md` with all manual `ln` commands
- ✓ DONE — Move `proofread` and `publish-crate` skill dirs into dotfiles and add symlinks

### T-04: Context rot canary hook
- ✓ DONE — Write `home/claude/hooks/canary-inject.sh` — injects secret word on first message of each session via `UserPromptSubmit` hook
- ✓ DONE — Symlink into `~/.claude/hooks/canary-inject.sh`
- ✓ DONE — Wire `UserPromptSubmit` hook into `home/claude/settings.json` and live `~/.claude/settings.json`
- ✓ DONE — Document in `doc/claude-context-management.md`
- ✓ DONE — Fix jq parse error in canary-inject.sh (add `2>/dev/null` to jq call)

### T-05: Global CLAUDE.md hardening
- ✓ DONE — Sharpen session-start plan instruction ("Before responding to first message")
- ✓ DONE — Add task tracking instruction (use plan.md, not built-in task tools)
- ✓ DONE — Add Projects section (`~/projects/` as default location)
- ✓ DONE — Add filesystem search rule (no broad `find` from home/root)

### T-06: Pre-tool-use filter hook
- ✓ DONE — Copy `pre-tool-use-filter.sh` into `home/claude/hooks/` (was untracked plain file)
- ✓ DONE — Wire into settings.json as Bash PreToolUse hook (was not previously called)
- ✓ DONE — Add broad `find` blocking rule to filter
- ✓ DONE — Fix jq parse error under `set -euo pipefail` (`2>/dev/null || COMMAND=""`)

### T-07: settings.json sync and symlink
- ✓ DONE — Diff live vs dotfiles settings.json; resolve drift (ctx-trakr hooks, OTEL vars removed)
- ✓ DONE — Replace live `~/.claude/settings.json` plain file with symlink to dotfiles
- ✓ DONE — Add `mcp__linear__*` to allow list
- ✓ DONE — Add `Read(/tmp/**)` to allow list (Claude Code glob doesn't match `/tmp` via `/**`)

### T-08: Graphify awareness doc
- ✓ DONE — Added, then reverted, a `## Graphify` section in global `home/claude/CLAUDE.md` — moved instead to a `## At a glance (for Claude)` section at the top of `doc/graphify.md` (covers: when it's worth suggesting, existing-graph usage, staleness caveat, hook compatibility caveat, install commands)
- ✓ DONE — Split old `## Claude Code integration` section in `doc/graphify.md` into standalone `## Installation (once per machine)` and `## Configuring a repo` step-by-step sections
- ✓ DONE — Verified and fixed the Grep/Glob hook-compatibility claim's source (roborhythms.com review — not graphify's own docs) and added inline citations
- ✓ DONE — Reconciled `doc/graphify.md` with a second-hand doc (`doc/installing-graphify.md`, scrubbed of internal work project references): corrected output location to `graphify-out/` (confirmed via web search against graphify's own GitHub/CLI docs — the original root-level `graph.json` claim was wrong), added `graphify hook install` (post-commit auto-rebuild, resolves the staleness caveat), MCP server option (`python -m graphify.serve`), and the `graphify claude install` CLAUDE.md-injection caveat relevant to manually-managed CLAUDE.md files
- ✓ DONE — `doc/installing-graphify.md` deleted by user now its useful content is folded into `doc/graphify.md`
- ✓ DONE — User located the real work `bootstrap.sh` (in `~/Downloads`) that the deleted doc was derived from; read it and cross-checked it corroborates our `doc/graphify.md` content almost verbatim (confirms `graphify-out/`, the CLAUDE.md-injection caveat/workaround, `graphify hook install`, MCP server invocation) and confirms the hook-compatibility risk is live: the hook's matcher is literally `Glob|Grep`, tool names this environment doesn't have (uses `Bash` for search instead)
- ✓ DONE — Created `home/claude/graphify-install.sh`, extracting only the graphify-relevant steps from `bootstrap.sh` (skips unrelated `code-review-graph`/`TrueCourse`/`Superpowers` sections)
- ✓ DONE — User flagged confusion: the CLI + `PreToolUse` hook are genuinely global (`~/.claude/settings.json` applies to every project already) but the graph/`.mcp.json`/post-commit hook are inherently per-repo — split the single script into two: `home/claude/graphify-install.sh` (global, once per machine: pipx install + hook merge) and `home/claude/graphify-configure-repo.sh` (per-repo: `.mcp.json` entry, `graphify hook install`, `.gitignore`)
- ✓ DONE — Rewrote "Installation (once per machine)" and "Configuring a repo" sections in `doc/graphify.md` to match the two-script split, explaining why the split exists
- ✓ DONE — Moved "Installation" and "Configuring a repo" sections to the top of `doc/graphify.md`, right after the title
- ✓ DONE — Fixed `graphify-install.sh` to use `uv tool install graphifyy` instead of `pipx` (copied unreflectively from `bootstrap.sh` — this machine already has `uv`, not `pipx`); doc updated to explain the swap
- ✓ DONE — Discovery decided: repo `CLAUDE.md` rule added — "If asked about graphify, read `doc/graphify.md`"
- ✓ DONE — Concluded the work `bootstrap.sh` was itself out of date: current graphify source (`graphify/__main__.py`, tag `v8`) fixed the `Glob|Grep` hook gap (now dual `Bash` + `Read|Glob` matchers) and `graphify claude install` writes to the *project* CLAUDE.md (small, marker-delimited, reviewable), not the global one
- ✓ DONE — Approach simplified accordingly: both tracked scripts obsoleted; `doc/graphify.md` rewritten — install is `uv tool install graphifyy` once per machine, per-repo config is upstream's `graphify claude install` + review diff; "At a glance" renamed "Notes for Claude"
- ✓ DONE — Deleted the two obsolete untracked scripts (`home/claude/graphify-install.sh`, `graphify-configure-repo.sh`)
- Note: open offer of a `bin/graphify-repo-init` wrapper (`graphify claude install` + `hook install` + `.gitignore` + pbcopy doc path), symmetric with `bin/copilot-repo-init` — not decided
