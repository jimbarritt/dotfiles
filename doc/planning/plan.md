# Dotfiles — Implementation Plan

## ── WHAT'S NEXT ──────────────────────────────────────────────────────────
**Next:** Action 2.2 — Rewrite README.md
**Sub-doc:** (none)
**Blockers:** None
─────────────────────────────────────────────────────────────────────────────

## Phase 1: Claude Tooling

### Action 1.1: Statusline — account-type toggle
- ✓ DONE — Remove token count from statusline
- ✓ DONE — Detect subscription vs API mode via `rate_limits` presence in JSON
- ✓ DONE — Show cost only on API mode, or on subscription when rate limit >= 100% (extra usage)
- ✓ DONE — Replace ctx percentage with absolute token count (`total_input_tokens + total_output_tokens`) with three-level warning (0–149k dim, 150–169k bold, 170k+ `!!value!!`)
- ✓ DONE — Adjust context token warning bands: dim <100k, bold 100–119k, `!!value!!` ≥120k
- ✓ DONE — Switch session cost display from GBP to USD, no decimal places
- TODO — Verify extra usage branch works in practice (need to hit 100% on 5h or 7d limit)

### Action 1.2: Plan skills
- ✓ DONE — Create `update-plan` skill (`home/claude/skills/update-plan/SKILL.md`)
- ✓ DONE — Create `load-plan` skill (`home/claude/skills/load-plan/SKILL.md`)
- ✓ DONE — Create `init-plan` skill (`home/claude/skills/init-plan/SKILL.md`)
- ✓ DONE — Symlink all three into `~/.claude/skills/`
- ✓ DONE — Add `Skill(*)` to global permissions to suppress approval prompt

### Action 1.3: Claude config documentation
- ✓ DONE — Create `home/claude/README.md` with all manual `ln` commands
- ✓ DONE — Move `proofread` and `publish-crate` skill dirs into dotfiles and add symlinks

### Action 1.4: Context rot canary hook
- ✓ DONE — Write `home/claude/hooks/canary-inject.sh` — injects secret word on first message of each session via `UserPromptSubmit` hook
- ✓ DONE — Symlink into `~/.claude/hooks/canary-inject.sh`
- ✓ DONE — Wire `UserPromptSubmit` hook into `home/claude/settings.json` and live `~/.claude/settings.json`
- ✓ DONE — Document in `doc/claude-context-management.md`
- ✓ DONE — Fix jq parse error in canary-inject.sh (add `2>/dev/null` to jq call)

### Action 1.5: Global CLAUDE.md hardening
- ✓ DONE — Sharpen session-start plan instruction ("Before responding to first message")
- ✓ DONE — Add task tracking instruction (use plan.md, not built-in task tools)
- ✓ DONE — Add Projects section (`~/projects/` as default location)
- ✓ DONE — Add filesystem search rule (no broad `find` from home/root)

### Action 1.6: Pre-tool-use filter hook
- ✓ DONE — Copy `pre-tool-use-filter.sh` into `home/claude/hooks/` (was untracked plain file)
- ✓ DONE — Wire into settings.json as Bash PreToolUse hook (was not previously called)
- ✓ DONE — Add broad `find` blocking rule to filter
- ✓ DONE — Fix jq parse error under `set -euo pipefail` (`2>/dev/null || COMMAND=""`)

### Action 1.7: settings.json sync and symlink
- ✓ DONE — Diff live vs dotfiles settings.json; resolve drift (ctx-trakr hooks, OTEL vars removed)
- ✓ DONE — Replace live `~/.claude/settings.json` plain file with symlink to dotfiles
- ✓ DONE — Add `mcp__linear__*` to allow list
- ✓ DONE — Add `Read(/tmp/**)` to allow list (Claude Code glob doesn't match `/tmp` via `/**`)

## Phase 2: Dotfiles Install Script

### Action 2.1: Wire Claude config into do.sh
- ✓ DONE — Add `link_claude` function to `do.sh` covering: `settings.json`, `keybindings.json`, `statusline-command.sh`, `CLAUDE.md`, all `skills/*` dirs, all `themes/*` files
- ✓ DONE — Call `link_claude` from `link_all`
- ✓ DONE — Add corresponding `unlink_claude` entries to `unlink_all`

### Action 2.2: Update README.md
- TODO — Rewrite `README.md` to reflect current setup (replace outdated Java/Emacs/Vagrant content)
- TODO — Document the Claude tooling section (skills, statusline, settings)

## Phase 3: Statusline Refinement

### Action 3.1: Verify extra usage detection
- TODO — Confirm whether `rate_limits.five_hour.used_percentage` can exceed 100 in the JSON, or is capped
- TODO — If capped at 100, find alternative signal for "in extra usage" (e.g. a dedicated field)
- TODO — Adjust threshold logic in `statusline-command.sh` if needed

### Action 3.2: Plan skill refinement (after first real use)
- TODO — Review `update-plan` / `load-plan` / `init-plan` skill behaviour after using them in anger across projects
- TODO — Tighten checkpoint format or WHAT'S NEXT pointer based on what proves useful

## ── CHECKPOINT: Session 2026-06-14 ──────────────────────────────────────

**What was completed this session:**
- Discovered Action 1.3 was already done in a previous session (proofread and publish-crate are in `home/claude/skills/` and symlinked) — plan updated to reflect this
- Updated stale Implementation Notes line about proofread/publish-crate

**State of the project:**
Phase 1 is complete except for the extra-usage branch verification (deferred to Phase 3). All skills are tracked in dotfiles. Phase 2 (do.sh wiring and README rewrite) is next.

**Immediate next priorities:**
1. Action 2.1 — Add `link_claude` function to `do.sh` covering all Claude config
2. Action 2.1 — Call `link_claude` from `link_all`; add `unlink_claude` entries
3. Action 2.2 — Rewrite `README.md` to reflect current setup

─────────────────────────────────────────────────────────────────────────────

## ── CHECKPOINT: Session 2026-06-18 ──────────────────────────────────────

**What was completed this session:**
- Action 2.1 confirmed already done — `link_claude` / `unlink_claude` were in `do.sh`; plan updated
- `settings.json` drift resolved: live file was a plain file diverged from dotfiles; replaced with symlink; ctx-trakr hooks and OTEL env vars removed from dotfiles version
- Global `CLAUDE.md` hardened: session-start instruction sharpened; task tracking, projects dir, and filesystem search rules added
- `pre-tool-use-filter.sh` copied into dotfiles, wired into settings.json, broad `find` blocking added, jq `set -e` parse error fixed
- `canary-inject.sh` jq parse error fixed (`2>/dev/null`)
- Statusline: context bands adjusted (100k/120k), session cost switched to USD no decimals, trakr display also no decimals
- `mcp__linear__*` added to allow list; `Read(/tmp/**)` added to address `/tmp` permission prompts

**State of the project:**
Phase 1 and 2.1 are complete. Config is now clean and symlinked on both machines. Phase 2.2 (README rewrite) is the immediate next item.

**Immediate next priorities:**
1. Action 2.2 — Rewrite `README.md` to reflect current setup
2. Action 3.1 — Verify extra usage detection in statusline
3. Action 3.2 — Review plan skill behaviour after sustained use

─────────────────────────────────────────────────────────────────────────────

## ── CHECKPOINT: Session 2026-06-18b ─────────────────────────────────────

**What was completed this session:**
- Diagnosed why `Read(/tmp/**)` still prompted: macOS `/tmp` is a symlink to `/private/tmp`; the Read tool resolves the symlink before permission matching, so the real path never matched
- Added `"Read(/private/tmp/**)"` to allow list in `home/claude/settings.json`

**State of the project:**
Phase 1 and 2.1 are complete. The `/tmp` permission fix is now applied at the real path level. Phase 2.2 (README rewrite) remains the immediate next item.

**Immediate next priorities:**
1. Action 2.2 — Rewrite `README.md` to reflect current setup
2. Action 3.1 — Verify extra usage detection in statusline
3. Action 3.2 — Review plan skill behaviour after sustained use

─────────────────────────────────────────────────────────────────────────────

## ── CHECKPOINT: Session 2026-06-18c ─────────────────────────────────────

**What was completed this session:**
- Added `Read(/private/tmp/*)` (single `*`) to allow list in `home/claude/settings.json` alongside the existing `Read(/private/tmp/**)` entry
- Hypothesis: Claude Code's glob engine may not match `**` against files directly in a directory (`dir/**` → only `dir/subdir/file`, not `dir/file`)

**State of the project:**
Phase 1 and 2.1 complete. The `/tmp` permission prompt is still being investigated — single-star entry added as a diagnostic step. Next session should re-test `Read(/tmp/test-read.txt)` immediately; if it no longer prompts, the `**` theory is confirmed and `Read(/tmp/*)` should be added for symmetry. If it still prompts, investigate Ghostty TCC permissions or consider switching scratch space to `~/tmp`.

**Immediate next priorities:**
1. Re-test `Read(/tmp/test-read.txt)` at session start — confirm whether single-star fix resolves the prompt
2. If confirmed: add `Read(/tmp/*)` for symmetry; update Implementation Notes
3. If still prompting: investigate Ghostty Full Disk Access or switch to `~/tmp`
4. Action 2.2 — Rewrite `README.md` to reflect current setup

─────────────────────────────────────────────────────────────────────────────

## ── CHECKPOINT: Session 2026-06-18d ─────────────────────────────────────

**What was completed this session:**
- Diagnosed root cause of persistent `/tmp` permission prompts: Claude Code uses gitignore-spec anchoring where a single `/` prefix means "relative to project root", not filesystem root — absolute paths require `//` prefix
- Fixed `settings.json`: `Read(/tmp/**)` → `Read(//tmp/**)`, `Read(/private/tmp/**)` → `Read(//private/tmp/**)`, dropped redundant single-star entry
- Added `Write(//tmp/**)` and `Write(//private/tmp/**)` for symmetry
- Updated implementation notes with `//` anchoring rule

**State of the project:**
Phase 1 and 2.1 complete. The `/tmp` permission prompt is now fully resolved — reads and writes to `/tmp` work without prompting. Implementation notes updated with the `//` anchoring rule so the footgun is documented.

**Immediate next priorities:**
1. Action 2.2 — Rewrite `README.md` to reflect current setup
2. Action 3.1 — Verify extra usage detection in statusline
3. Action 3.2 — Review plan skill behaviour after sustained use

─────────────────────────────────────────────────────────────────────────────

---

## Implementation Notes

### Architecture
- All Claude config lives in `home/claude/` and is symlinked into `~/.claude/` via `do.sh link-claude`
- Hooks live in `home/claude/hooks/` — symlinked individually into `~/.claude/hooks/`
- Skills are directory symlinks — symlink the whole dir, not individual files
- All skills in `~/.claude/skills/` are directory symlinks into `home/claude/skills/` — fully tracked in dotfiles
- Statusline script uses `jq 'has("rate_limits")'` to detect subscription vs API mode
- Rate limit reset timestamps come as Unix timestamps; converted to HH:MM (5h) and DD/MM (7d) via `date -r`
- `git stash` is denied globally — added after Sonnet stashed commits and lost track of them
- `pre-tool-use-filter.sh` runs on every Bash PreToolUse — blocks destructive ops, broad find, git push/commit/add, sudo, pipe-to-shell etc.
- OTEL env vars (`OTEL_EXPORTER_OTLP_*`) belong in `~/.zshrc_machine` on machines with trakr, not in `settings.json`
- Claude Code permission patterns use gitignore-spec anchoring: a single `/` prefix means "relative to project root", NOT filesystem root. Absolute filesystem paths require `//` (double slash) prefix — e.g. `Read(//tmp/**)`, `Read(//private/tmp/**)`. Bare `**` and `/**` are both project-scoped.
- macOS `/tmp` is a symlink to `/private/tmp`. Allow rules check BOTH the symlink path and the resolved path — both must match. Use `Read(//tmp/**)` + `Read(//private/tmp/**)` (and `Write(...)` equivalents) to cover `/tmp` fully.
