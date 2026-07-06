---
name: copilot-repo-init
description: Make the current repo compatible with GitHub Copilot agents — establish AGENTS.md as the canonical instructions file, reduce CLAUDE.md to an @AGENTS.md import plus Claude-only extras, and symlink .github/copilot-instructions.md. Use when asked to make a repo Copilot-compatible or set up cross-agent instructions.
---

# Copilot Repo Init

Make the current repository's agent instructions work across Claude Code, GitHub Copilot, and other AGENTS.md-aware tools (Codex, Cursor, Gemini CLI).

## Background

Read `~/Code/github/jimbarritt/dotfiles/doc/claude-copilot-compatibility.md` first if it exists on this machine — it explains the reasoning (why AGENTS.md is canonical and CLAUDE.md imports it, not the reverse).

The short version: Claude Code does not read `AGENTS.md` natively but supports a mechanical `@AGENTS.md` import in `CLAUDE.md`, expanded at session start. Every other major tool reads `AGENTS.md` natively. So the only reliable direction is AGENTS.md canonical, CLAUDE.md importing it.

## Steps

1. **Assess current state** — check the repo root for `CLAUDE.md`, `AGENTS.md`, and `.github/copilot-instructions.md`.

2. **Establish `AGENTS.md` as canonical:**
   - Only `CLAUDE.md` exists: move its tool-agnostic content into a new `AGENTS.md`. Keep anything genuinely Claude-specific (task-tool rules, skill invocation, Claude hooks) in `CLAUDE.md`.
   - Both exist: merge so `AGENTS.md` holds all shared instructions, deduplicated; `CLAUDE.md` keeps only Claude-specific content.
   - Neither exists: create a minimal `AGENTS.md` describing build/test commands and repo conventions.

3. **Rewrite `CLAUDE.md`** so its first line is the import, with Claude-only extras (if any) below:

   ```markdown
   @AGENTS.md

   <!-- Claude-specific instructions below, invisible to other tools -->
   ```

   If there are no Claude-specific extras, the file is just the one import line.

4. **Symlink for Copilot surfaces that only read `.github/copilot-instructions.md`** (code review, the cloud coding agent):

   ```sh
   mkdir -p .github
   ln -sf ../AGENTS.md .github/copilot-instructions.md
   ```

   Copilot CLI reads `AGENTS.md` directly, so this is belt-and-braces for the other surfaces.

5. **Check any repo-shipped skills** — if the repo has agent skills (`.claude/skills/` or `.github/skills/`), every `SKILL.md` needs YAML frontmatter with `name` (exactly matching its directory name) and `description`, or Copilot silently skips it.

6. **Show the user the diff** — do not commit; let them review. Point out anything that moved between files so they can veto the classification of "shared" vs "Claude-specific".
