# Dotfiles — Implementation Plan

## What's Next

**Next:** Task 1 — Verify extra usage detection (Delta: Statusline Refinement)
**Sub-doc:** (none)
**Blockers:** None

## Summary

| Delta | Task | Status |
|-------|------|--------|
| [Delta: Statusline Refinement](#delta-statusline-refinement) | [1. Verify extra usage detection](#task-1-verify-extra-usage-detection) | TODO |
| | [2. Plan skill refinement](#task-2-plan-skill-refinement) | ✓ DONE |

Archived Deltas: see the [archive index](archive/index.md)

## Delta: Statusline Refinement

### Task 1: Verify extra usage detection
- TODO — Confirm whether `rate_limits.five_hour.used_percentage` can exceed 100 in the JSON, or is capped
- TODO — If capped at 100, find alternative signal for "in extra usage" (e.g. a dedicated field)
- TODO — Adjust threshold logic in `statusline-command.sh` if needed

### Task 2: Plan skill refinement
- ✓ DONE — Review `update-plan` / `load-plan` / `init-plan` skill behaviour after real use — user found old format hard to read
- ✓ DONE — Create shared `home/claude/skills/plan-format/PLAN-FORMAT.md` — single canonical spec all three skills reference (avoids drift)
- ✓ DONE — New format: "Deltas" replace "Phases", "Tasks" replace "Actions", plain `##` headings for What's Next and Checkpoints (no box-drawing separators), Summary table (Delta | Task | Status) with anchor links at the top
- ✓ DONE — `update-plan` now migrates old-format plans automatically; `load-plan` flags stale format
- ✓ DONE — Review fixes: "Delta {X.Y}" → "Task {X.Y}" label, checkpoint placement contradiction in skeleton, `IN PROGRESS` status defined (some bullets done, some not)
- ✓ DONE — This plan migrated to the new format

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
