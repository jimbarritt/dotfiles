# Dotfiles — Implementation Plan

## ── WHAT'S NEXT ──────────────────────────────────────────────────────────
**Next:** Action 1.3 — Move proofread and publish-crate skill dirs into dotfiles
**Sub-doc:** (none)
**Blockers:** None
─────────────────────────────────────────────────────────────────────────────

## Phase 1: Claude Tooling

### Action 1.1: Statusline — account-type toggle
- ✓ DONE — Remove token count from statusline
- ✓ DONE — Detect subscription vs API mode via `rate_limits` presence in JSON
- ✓ DONE — Show cost only on API mode, or on subscription when rate limit >= 100% (extra usage)
- ✓ DONE — Replace ctx percentage with absolute token count (`total_input_tokens + total_output_tokens`) with three-level warning (0–149k dim, 150–169k bold, 170k+ `!!value!!`)
- TODO — Verify extra usage branch works in practice (need to hit 100% on 5h or 7d limit)

### Action 1.2: Plan skills
- ✓ DONE — Create `update-plan` skill (`home/claude/skills/update-plan/SKILL.md`)
- ✓ DONE — Create `load-plan` skill (`home/claude/skills/load-plan/SKILL.md`)
- ✓ DONE — Create `init-plan` skill (`home/claude/skills/init-plan/SKILL.md`)
- ✓ DONE — Symlink all three into `~/.claude/skills/`
- ✓ DONE — Add `Skill(*)` to global permissions to suppress approval prompt

### Action 1.3: Claude config documentation
- ✓ DONE — Create `home/claude/README.md` with all manual `ln` commands
- TODO — Move `proofread` and `publish-crate` skill dirs into dotfiles and add symlinks

### Action 1.4: Context rot canary hook
- ✓ DONE — Write `home/claude/hooks/canary-inject.sh` — injects secret word on first message of each session via `UserPromptSubmit` hook
- ✓ DONE — Symlink into `~/.claude/hooks/canary-inject.sh`
- ✓ DONE — Wire `UserPromptSubmit` hook into `home/claude/settings.json` and live `~/.claude/settings.json`
- ✓ DONE — Document in `doc/claude-context-management.md`

## Phase 2: Dotfiles Install Script

### Action 2.1: Wire Claude config into do.sh
- TODO — Add `link_claude` function to `do.sh` covering: `settings.json`, `keybindings.json`, `statusline-command.sh`, `CLAUDE.md`, all `skills/*` dirs, all `themes/*` files
- TODO — Call `link_claude` from `link_all`
- TODO — Add corresponding `unlink_claude` entries to `unlink_all`

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

## ── CHECKPOINT: Session 2026-06-13 ──────────────────────────────────────

**What was completed this session:**
- Fixed `Skill(*)` missing from live `~/.claude/settings.json` (dotfiles had it, live file didn't — divergence from a previous session)
- Added `Bash(git stash:*)` to deny list in both `~/.claude/settings.json` and `home/claude/settings.json` (prompted by Sonnet stashing commits and forgetting about them)

**State of the project:**
No plan TODOs completed; this was a config housekeeping session. The live `~/.claude/settings.json` and dotfiles `home/claude/settings.json` deny lists are now in sync. Skill permission prompts should stop appearing after a Claude Code restart.

**Immediate next priorities:**
1. Action 1.3 — Move `proofread` and `publish-crate` skill dirs into dotfiles and add symlinks
2. Action 2.1 — Add `link_claude` function to `do.sh`
3. Action 2.1 — Call `link_claude` from `link_all` and add `unlink_claude` entries

─────────────────────────────────────────────────────────────────────────────

## ── CHECKPOINT: Session 2026-06-13 (2) ─────────────────────────────────

**What was completed this session:**
- Statusline: replaced ctx percentage with absolute token count (`total_input_tokens + total_output_tokens`), three-level warning (dim / bold / `!!value!!` at 150k / 170k)
- Debugged correct JSON field name via live payload capture (`context_window.total_input_tokens + total_output_tokens`, not `tokens_used`)
- Created `home/claude/hooks/canary-inject.sh` — `UserPromptSubmit` hook that injects secret word `ramalamadingdong` on first message of each session as a context-rot canary
- Wired canary hook into both dotfiles and live `settings.json`; symlinked into `~/.claude/hooks/`
- Appended two new sections to `doc/claude-context-management.md` covering absolute token monitoring and the canary technique

**State of the project:**
Statusline now shows meaningful context rot signal. Canary hook confirmed working (secret word visible in system-reminder this session). Phase 1 is largely complete — only the extra-usage branch verification and moving proofread/publish-crate into dotfiles remain before Phase 2.

**Immediate next priorities:**
1. Action 1.3 — Move `proofread` and `publish-crate` skill dirs into dotfiles and add symlinks
2. Action 2.1 — Add `link_claude` function to `do.sh` (covers hooks dir now too)
3. Action 2.2 — Rewrite `README.md`

─────────────────────────────────────────────────────────────────────────────

---

## Implementation Notes

### Architecture
- All Claude config lives in `home/claude/` and must be manually symlinked into `~/.claude/` until `do.sh` is updated (see `home/claude/README.md` for current `ln` commands)
- Hooks live in `home/claude/hooks/` — symlink individual scripts into `~/.claude/hooks/`
- Skills are directory symlinks — symlink the whole dir, not individual files
- `proofread/` and `publish-crate/` in `~/.claude/skills/` are currently real directories, not tracked in dotfiles
- Statusline script uses `jq 'has("rate_limits")'` to detect subscription vs API mode
- Rate limit reset timestamps come as Unix timestamps; converted to HH:MM (5h) and DD/MM (7d) via `date -r`
- `git stash` is denied globally — added after Sonnet stashed commits and lost track of them
