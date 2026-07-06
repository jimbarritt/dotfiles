# Dotfiles — Implementation Plan

## What's Next

**Next:** Task 2.2 — Update README.md
**Sub-doc:** (none)
**Blockers:** None

## Summary

| Delta | Task | Status |
|-------|------|--------|
| [Delta 1: Claude Tooling](#delta-1-claude-tooling) | [1.1 Statusline — account-type toggle](#task-11-statusline--account-type-toggle) | IN PROGRESS |
| | [1.2 Plan skills](#task-12-plan-skills) | ✓ DONE |
| | [1.3 Claude config documentation](#task-13-claude-config-documentation) | ✓ DONE |
| | [1.4 Context rot canary hook](#task-14-context-rot-canary-hook) | ✓ DONE |
| | [1.5 Global CLAUDE.md hardening](#task-15-global-claudemd-hardening) | ✓ DONE |
| | [1.6 Pre-tool-use filter hook](#task-16-pre-tool-use-filter-hook) | ✓ DONE |
| | [1.7 settings.json sync and symlink](#task-17-settingsjson-sync-and-symlink) | ✓ DONE |
| | [1.8 Graphify awareness doc](#task-18-graphify-awareness-doc) | ✓ DONE |
| [Delta 2: Dotfiles Install Script](#delta-2-dotfiles-install-script) | [2.1 Wire Claude config into do.sh](#task-21-wire-claude-config-into-dosh) | ✓ DONE |
| | [2.2 Update README.md](#task-22-update-readmemd) | TODO |
| [Delta 3: Statusline Refinement](#delta-3-statusline-refinement) | [3.1 Verify extra usage detection](#task-31-verify-extra-usage-detection) | TODO |
| | [3.2 Plan skill refinement](#task-32-plan-skill-refinement) | ✓ DONE |
| [Delta 4: GitHub Copilot Compatibility](#delta-4-github-copilot-compatibility) | [4.1 Copilot CLI integration](#task-41-copilot-cli-integration) | ✓ DONE |
| | [4.2 Repo governance](#task-42-repo-governance) | ✓ DONE |

## Delta 1: Claude Tooling

### Task 1.1: Statusline — account-type toggle
- ✓ DONE — Remove token count from statusline
- ✓ DONE — Detect subscription vs API mode via `rate_limits` presence in JSON
- ✓ DONE — Show cost only on API mode, or on subscription when rate limit >= 100% (extra usage)
- ✓ DONE — Replace ctx percentage with absolute token count (`total_input_tokens + total_output_tokens`) with three-level warning (0–149k dim, 150–169k bold, 170k+ `!!value!!`)
- ✓ DONE — Adjust context token warning bands: dim <100k, bold 100–119k, `!!value!!` ≥120k
- ✓ DONE — Switch session cost display from GBP to USD, no decimal places
- TODO — Verify extra usage branch works in practice (need to hit 100% on 5h or 7d limit) — deferred to Task 3.1

### Task 1.2: Plan skills
- ✓ DONE — Create `update-plan` skill (`home/claude/skills/update-plan/SKILL.md`)
- ✓ DONE — Create `load-plan` skill (`home/claude/skills/load-plan/SKILL.md`)
- ✓ DONE — Create `init-plan` skill (`home/claude/skills/init-plan/SKILL.md`)
- ✓ DONE — Symlink all three into `~/.claude/skills/`
- ✓ DONE — Add `Skill(*)` to global permissions to suppress approval prompt

### Task 1.3: Claude config documentation
- ✓ DONE — Create `home/claude/README.md` with all manual `ln` commands
- ✓ DONE — Move `proofread` and `publish-crate` skill dirs into dotfiles and add symlinks

### Task 1.4: Context rot canary hook
- ✓ DONE — Write `home/claude/hooks/canary-inject.sh` — injects secret word on first message of each session via `UserPromptSubmit` hook
- ✓ DONE — Symlink into `~/.claude/hooks/canary-inject.sh`
- ✓ DONE — Wire `UserPromptSubmit` hook into `home/claude/settings.json` and live `~/.claude/settings.json`
- ✓ DONE — Document in `doc/claude-context-management.md`
- ✓ DONE — Fix jq parse error in canary-inject.sh (add `2>/dev/null` to jq call)

### Task 1.5: Global CLAUDE.md hardening
- ✓ DONE — Sharpen session-start plan instruction ("Before responding to first message")
- ✓ DONE — Add task tracking instruction (use plan.md, not built-in task tools)
- ✓ DONE — Add Projects section (`~/projects/` as default location)
- ✓ DONE — Add filesystem search rule (no broad `find` from home/root)

### Task 1.6: Pre-tool-use filter hook
- ✓ DONE — Copy `pre-tool-use-filter.sh` into `home/claude/hooks/` (was untracked plain file)
- ✓ DONE — Wire into settings.json as Bash PreToolUse hook (was not previously called)
- ✓ DONE — Add broad `find` blocking rule to filter
- ✓ DONE — Fix jq parse error under `set -euo pipefail` (`2>/dev/null || COMMAND=""`)

### Task 1.7: settings.json sync and symlink
- ✓ DONE — Diff live vs dotfiles settings.json; resolve drift (ctx-trakr hooks, OTEL vars removed)
- ✓ DONE — Replace live `~/.claude/settings.json` plain file with symlink to dotfiles
- ✓ DONE — Add `mcp__linear__*` to allow list
- ✓ DONE — Add `Read(/tmp/**)` to allow list (Claude Code glob doesn't match `/tmp` via `/**`)

### Task 1.8: Graphify awareness doc
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

## Delta 2: Dotfiles Install Script

### Task 2.1: Wire Claude config into do.sh
- ✓ DONE — Add `link_claude` function to `do.sh` covering: `settings.json`, `keybindings.json`, `statusline-command.sh`, `CLAUDE.md`, all `skills/*` dirs, all `themes/*` files
- ✓ DONE — Call `link_claude` from `link_all`
- ✓ DONE — Add corresponding `unlink_claude` entries to `unlink_all`

### Task 2.2: Update README.md
- TODO — Rewrite `README.md` to reflect current setup (replace outdated Java/Emacs/Vagrant content)
- TODO — Document the Claude tooling section (skills, statusline, settings)

## Delta 3: Statusline Refinement

### Task 3.1: Verify extra usage detection
- TODO — Confirm whether `rate_limits.five_hour.used_percentage` can exceed 100 in the JSON, or is capped
- TODO — If capped at 100, find alternative signal for "in extra usage" (e.g. a dedicated field)
- TODO — Adjust threshold logic in `statusline-command.sh` if needed

### Task 3.2: Plan skill refinement
- ✓ DONE — Review `update-plan` / `load-plan` / `init-plan` skill behaviour after real use — user found old format hard to read
- ✓ DONE — Create shared `home/claude/skills/plan-format/PLAN-FORMAT.md` — single canonical spec all three skills reference (avoids drift)
- ✓ DONE — New format: "Deltas" replace "Phases", "Tasks" replace "Actions", plain `##` headings for What's Next and Checkpoints (no box-drawing separators), Summary table (Delta | Task | Status) with anchor links at the top
- ✓ DONE — `update-plan` now migrates old-format plans automatically; `load-plan` flags stale format
- ✓ DONE — Review fixes: "Delta {X.Y}" → "Task {X.Y}" label, checkpoint placement contradiction in skeleton, `IN PROGRESS` status defined (some bullets done, some not)
- ✓ DONE — This plan migrated to the new format

## Delta 4: GitHub Copilot Compatibility

### Task 4.1: Copilot CLI integration
- ✓ DONE — YAML frontmatter added to `proofread` and `publish-crate` skills (Copilot requires `name` matching dir + `description`; other five already had it)
- ✓ DONE — `doc/claude-copilot-compatibility.md` created: install, skills, global/per-repo instructions, permissions
- ✓ DONE — `do.sh link-copilot`/`unlink-copilot`: `~/.copilot/copilot-instructions.md` → `home/claude/CLAUDE.md`, all skills → `~/.copilot/skills/`, `~/.copilot/denied-commands` → `home/copilot/denied-commands`
- ✓ DONE — Global permissions via `copilot()` zsh wrapper: `--allow-all` + `--deny-tool` flags from `home/copilot/denied-commands` (mirrors Claude's deny list — keep in sync). Copilot has no global permissions config; per-command/per-kind allows all left residual prompts
- ✓ DONE — Work machine's `permissions-config.json` assessed: per-location + Pleo-specific → not tracked; command list superseded by deny-list approach
- ✓ DONE — `.github/copilot-instructions.md → ../CLAUDE.md` symlink in this repo
- ✓ DONE — AGENTS.md-canonical pattern documented (CLAUDE.md `@AGENTS.md` import; Claude doesn't read AGENTS.md natively) + `bin/copilot-repo-init` for the mechanical half (skill approach tried and rejected — too infrequent to justify per-session context)
- Note: open offer to convert this repo itself to the AGENTS.md pattern — optional, not decided

### Task 4.2: Repo governance
- ✓ DONE — `bin/am-dotfiles-base` + CLAUDE.md rule: changes only upstream (as `jimbarritt`); other machines check via script and pull
- ✓ DONE — Global CLAUDE.md: PR rule (no Claude references in titles/bodies/commits)
- ✓ DONE — `settings.json` synced with work machine: `model: sonnet`, `effortLevel: medium`; graphify `Glob|Grep` hook rejected
- ✓ DONE — `home/claude/README.md` rewritten (was badly stale); zshrc `reload` → help function + `reload-zshrc`

## Checkpoint: Session 2026-06-14

**What was completed this session:**
- Discovered Task 1.3 was already done in a previous session (proofread and publish-crate are in `home/claude/skills/` and symlinked) — plan updated to reflect this
- Updated stale Implementation Notes line about proofread/publish-crate

**State of the project:**
Delta 1 is complete except for the extra-usage branch verification (deferred to Delta 3). All skills are tracked in dotfiles. Delta 2 (do.sh wiring and README rewrite) is next.

**Immediate next priorities:**
1. Task 2.1 — Add `link_claude` function to `do.sh` covering all Claude config
2. Task 2.1 — Call `link_claude` from `link_all`; add `unlink_claude` entries
3. Task 2.2 — Rewrite `README.md` to reflect current setup

## Checkpoint: Session 2026-06-18

**What was completed this session:**
- Task 2.1 confirmed already done — `link_claude` / `unlink_claude` were in `do.sh`; plan updated
- `settings.json` drift resolved: live file was a plain file diverged from dotfiles; replaced with symlink; ctx-trakr hooks and OTEL env vars removed from dotfiles version
- Global `CLAUDE.md` hardened: session-start instruction sharpened; task tracking, projects dir, and filesystem search rules added
- `pre-tool-use-filter.sh` copied into dotfiles, wired into settings.json, broad `find` blocking added, jq `set -e` parse error fixed
- `canary-inject.sh` jq parse error fixed (`2>/dev/null`)
- Statusline: context bands adjusted (100k/120k), session cost switched to USD no decimals, trakr display also no decimals
- `mcp__linear__*` added to allow list; `Read(/tmp/**)` added to address `/tmp` permission prompts

**State of the project:**
Delta 1 and Task 2.1 are complete. Config is now clean and symlinked on both machines. Task 2.2 (README rewrite) is the immediate next item.

**Immediate next priorities:**
1. Task 2.2 — Rewrite `README.md` to reflect current setup
2. Task 3.1 — Verify extra usage detection in statusline
3. Task 3.2 — Review plan skill behaviour after sustained use

## Checkpoint: Session 2026-06-18b

**What was completed this session:**
- Diagnosed why `Read(/tmp/**)` still prompted: macOS `/tmp` is a symlink to `/private/tmp`; the Read tool resolves the symlink before permission matching, so the real path never matched
- Added `"Read(/private/tmp/**)"` to allow list in `home/claude/settings.json`

**State of the project:**
Delta 1 and Task 2.1 are complete. The `/tmp` permission fix is now applied at the real path level. Task 2.2 (README rewrite) remains the immediate next item.

**Immediate next priorities:**
1. Task 2.2 — Rewrite `README.md` to reflect current setup
2. Task 3.1 — Verify extra usage detection in statusline
3. Task 3.2 — Review plan skill behaviour after sustained use

## Checkpoint: Session 2026-06-18c

**What was completed this session:**
- Added `Read(/private/tmp/*)` (single `*`) to allow list in `home/claude/settings.json` alongside the existing `Read(/private/tmp/**)` entry
- Hypothesis: Claude Code's glob engine may not match `**` against files directly in a directory (`dir/**` → only `dir/subdir/file`, not `dir/file`)

**State of the project:**
Delta 1 and Task 2.1 complete. The `/tmp` permission prompt is still being investigated — single-star entry added as a diagnostic step. Next session should re-test `Read(/tmp/test-read.txt)` immediately; if it no longer prompts, the `**` theory is confirmed and `Read(/tmp/*)` should be added for symmetry. If it still prompts, investigate Ghostty TCC permissions or consider switching scratch space to `~/tmp`.

**Immediate next priorities:**
1. Re-test `Read(/tmp/test-read.txt)` at session start — confirm whether single-star fix resolves the prompt
2. If confirmed: add `Read(/tmp/*)` for symmetry; update Implementation Notes
3. If still prompting: investigate Ghostty Full Disk Access or switch to `~/tmp`
4. Task 2.2 — Rewrite `README.md` to reflect current setup

## Checkpoint: Session 2026-06-18d

**What was completed this session:**
- Diagnosed root cause of persistent `/tmp` permission prompts: Claude Code uses gitignore-spec anchoring where a single `/` prefix means "relative to project root", not filesystem root — absolute paths require `//` prefix
- Fixed `settings.json`: `Read(/tmp/**)` → `Read(//tmp/**)`, `Read(/private/tmp/**)` → `Read(//private/tmp/**)`, dropped redundant single-star entry
- Added `Write(//tmp/**)` and `Write(//private/tmp/**)` for symmetry
- Updated implementation notes with `//` anchoring rule

**State of the project:**
Delta 1 and Task 2.1 complete. The `/tmp` permission prompt is now fully resolved — reads and writes to `/tmp` work without prompting. Implementation notes updated with the `//` anchoring rule so the footgun is documented.

**Immediate next priorities:**
1. Task 2.2 — Rewrite `README.md` to reflect current setup
2. Task 3.1 — Verify extra usage detection in statusline
3. Task 3.2 — Review plan skill behaviour after sustained use

## Checkpoint: Session 2026-07-01

**What was completed this session:**
- Added `configure_macos` function to `do.sh` — runs `defaults write com.mitchellh.ghostty NSWindowShadowEnabled -bool false` to disable Ghostty window shadows that bleed onto adjacent tiled windows
- `configure_macos` is now called from `install` alongside `install_zsh_plugins` and `install_git_hooks`

**State of the project:**
Delta 1 and Task 2.1 complete. A new macOS defaults step is now part of `do.sh install`. No existing plan TODOs were completed this session — the Ghostty shadow fix was a new addition not previously tracked.

**Immediate next priorities:**
1. Task 2.2 — Rewrite `README.md` to reflect current setup
2. Task 3.1 — Verify extra usage detection in statusline
3. Task 3.2 — Review plan skill behaviour after sustained use

## Checkpoint: Session 2026-07-01b

**What was completed this session:**
- Investigated graphify setup end-to-end for the dotfiles repo (Task 1.8), starting from `doc/graphify.md` (already existed)
- Attempted adding a `## Graphify` section to global `home/claude/CLAUDE.md`, then reverted it at user's request — moved the content into `doc/graphify.md` instead as a `## At a glance (for Claude)` section
- Verified the Grep/Glob `PreToolUse` hook-compatibility claim's actual source (roborhythms.com review, not graphify's own docs) via web search; added inline citations
- User surfaced a second-hand doc (`doc/installing-graphify.md`) claiming to derive from a work `bootstrap.sh` — initially flagged as suspicious, user clarified it came from another Claude instance's analysis of a real work script
- Reconciled `doc/graphify.md` against that doc and independent web search: corrected the output directory to `graphify-out/`, added `graphify hook install`, the MCP server option, and the CLAUDE.md-injection caveat on `graphify claude install`
- User located and shared the actual `bootstrap.sh` (in `~/Downloads`) — read it, confirmed it corroborates the reconciled doc almost verbatim, and confirmed the hook-compatibility risk is live in this exact environment (hook matcher is `Glob|Grep`, tool names this session doesn't have)
- Removed all references to the internal `north-star` project name from the (now-deleted) second-hand doc at user's request
- Created two tracked scripts: `home/claude/graphify-install.sh` (global) and `home/claude/graphify-configure-repo.sh` (per-repo) — split after user was confused why a "per-repo" script was doing genuinely global work
- Reordered `doc/graphify.md` so "Installation" and "Configuring a repo" are the first two sections
- Fixed `graphify-install.sh` to use `uv tool install graphifyy` instead of `pipx`

**State of the project:**
`doc/graphify.md` is now a reconciled, cited, and corrected reference doc with install/config instructions at the top. Two executable scripts exist but neither has been run yet — graphify is not actually installed on this machine. Neither script is wired into `do.sh`; both are deliberately manual/opt-in.

**Immediate next priorities:**
1. Task 1.8 — Decide how Claude should discover `doc/graphify.md` in future sessions
2. Run `home/claude/graphify-install.sh` (global) when ready, then `graphify-configure-repo.sh` in a specific large-enough repo
3. Task 2.2 — Rewrite `README.md` to reflect current setup
4. Task 3.1 — Verify extra usage detection in statusline
5. Task 3.2 — Review plan skill behaviour after sustained use

## Checkpoint: Session 2026-07-06

**What was completed this session:**
- Graphify concluded (Task 1.8): work bootstrap was itself outdated — upstream fixed the hook gap and its `claude install` writes a small reviewable section to the *project* CLAUDE.md. Both tracked scripts obsoleted, `doc/graphify.md` rewritten (install = `uv tool install graphifyy`; per-repo = `graphify claude install`), discovery via repo CLAUDE.md rule
- Full Copilot CLI integration (new Delta 4): skills frontmatter, `doc/claude-copilot-compatibility.md`, `do.sh link-copilot` (instructions + skills + denylist), `copilot()` zsh wrapper with `--allow-all` + deny flags — verified working after `--allow-tool` shell-list and `--allow-all-tools` both left residual prompts
- AGENTS.md-canonical pattern documented + `bin/copilot-repo-init` (replaced a short-lived skill — too infrequent to justify per-session context cost)
- `bin/am-dotfiles-base` guard + upstream-only rule in repo CLAUDE.md; PR no-Claude-references rule in global CLAUDE.md
- `settings.json` synced with work machine (sonnet/medium); work `permissions-config.json` rejected as Pleo-specific and per-location
- `home/claude/README.md` rewritten; zshrc `reload` split into help + `reload-zshrc`; memory note `project_copilot_integration.md` saved

**State of the project:**
Copilot and Claude Code now share one config surface, all tracked. Copilot permissions verified working end-to-end. User still to run: `./do.sh link-copilot` wherever not yet linked; delete obsolete `home/claude/graphify-*.sh` and `~/Downloads/permissions-config.json`. Work machine will pick everything up on next pull + `link-copilot`.

**Immediate next priorities:**
1. Task 2.2 — Rewrite `README.md` to reflect current setup
2. User: delete the two obsolete graphify scripts and the Downloads permissions file
3. Task 4.1 — Optionally convert this repo to the AGENTS.md pattern
4. Task 3.1 — Verify extra usage detection in statusline

## Checkpoint: Session 2026-07-06b

**What was completed this session:**
- Plan skill format overhaul (Task 3.2): created shared `home/claude/skills/plan-format/PLAN-FORMAT.md` as the single canonical plan spec; `init-plan`, `update-plan`, and `load-plan` all now reference it
- New format: "Deltas" replace "Phases", "Tasks" replace "Actions", What's Next and Checkpoint are plain `##` headings (no box-drawing separators), Summary table (Delta | Task | Status) with anchor links at the top of the plan
- `update-plan` migrates old-format plans automatically; `load-plan` flags stale-format plans
- Review pass fixed three defects: "Delta {X.Y}" mislabel → "Task {X.Y}", checkpoint-placement contradiction in the skeleton, undefined `IN PROGRESS` status (now: some bullets done, some not)
- This plan migrated to the new format (first real exercise of the migration path)
- Obsolete graphify scripts confirmed deleted (`home/claude/graphify-install.sh`, `graphify-configure-repo.sh`)

**State of the project:**
Plan skills and format are now consistent and centrally specified. The `plan-format` directory is not yet symlinked into `~/.claude/skills/` — user needs to run `./do.sh link-claude`. Note: `plan-format/` has no SKILL.md (it's a shared doc, not a skill) — flagged as a possible source of warnings from Claude Code/Copilot skill discovery; leave unless it causes problems.

**Immediate next priorities:**
1. User: run `./do.sh link-claude` to symlink the new `plan-format` directory
2. Task 2.2 — Rewrite `README.md` to reflect current setup
3. Task 4.1 — Optionally convert this repo to the AGENTS.md pattern
4. Task 3.1 — Verify extra usage detection in statusline

---

## Implementation Notes

### Architecture
- All Claude config lives in `home/claude/` and is symlinked into `~/.claude/` via `do.sh link-claude`
- Hooks live in `home/claude/hooks/` — symlinked individually into `~/.claude/hooks/`
- Skills are directory symlinks — symlink the whole dir, not individual files
- All skills in `~/.claude/skills/` are directory symlinks into `home/claude/skills/` — fully tracked in dotfiles
- Plan format is specified once in `home/claude/skills/plan-format/PLAN-FORMAT.md` — the three plan skills reference it; fix format drift there, not in individual skills
- Statusline script uses `jq 'has("rate_limits")'` to detect subscription vs API mode
- Rate limit reset timestamps come as Unix timestamps; converted to HH:MM (5h) and DD/MM (7d) via `date -r`
- `git stash` is denied globally — added after Sonnet stashed commits and lost track of them
- `pre-tool-use-filter.sh` runs on every Bash PreToolUse — blocks destructive ops, broad find, git push/commit/add, sudo, pipe-to-shell etc.
- OTEL env vars (`OTEL_EXPORTER_OTLP_*`) belong in `~/.zshrc_machine` on machines with trakr, not in `settings.json`
- Claude Code permission patterns use gitignore-spec anchoring: a single `/` prefix means "relative to project root", NOT filesystem root. Absolute filesystem paths require `//` (double slash) prefix — e.g. `Read(//tmp/**)`, `Read(//private/tmp/**)`. Bare `**` and `/**` are both project-scoped.
- macOS `/tmp` is a symlink to `/private/tmp`. Allow rules check BOTH the symlink path and the resolved path — both must match. Use `Read(//tmp/**)` + `Read(//private/tmp/**)` (and `Write(...)` equivalents) to cover `/tmp` fully.
