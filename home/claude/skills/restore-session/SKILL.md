---
name: restore-session
description: Restore a previously saved conversation session to resume work
argument-hint: "[session filename, timestamp, or 'latest']"
allowed-tools: Read, Bash, Glob, Grep
disable-model-invocation: false
model: haiku
---

# Restore Session

Restore context from a previously saved session file so we can continue where we left off.

## How to find the session

Sessions are stored in `.claude-sessions/session-{timestamp}.md` (relative to the repo root).

- If `$ARGUMENTS` is empty or "latest": use the most recent session file (by filename sort)
- If `$ARGUMENTS` is a partial timestamp (e.g. "2026-03-24"): find the closest match
- If `$ARGUMENTS` is a full filename: use it directly
- If multiple matches exist: list them and ask the user to pick

## What to do after reading the session file

1. **Read the entire session file** carefully
2. **Verify the current state** — check that the working directory, git branch, and key files mentioned in the session still exist and haven't changed unexpectedly since the session was saved
3. **Summarise the restored context** to the user:
   - What we were working on
   - What's already done
   - What's left to do
   - How we're going to resume
4. **Ask the user** if they want to continue from where we left off, or if priorities have changed
5. **If continuing**, pick up exactly where the session left off — follow the "How to resume" section from the saved session

## Important

- Don't just read the file and dump it — synthesise it into a clear briefing
- If the codebase has changed since the session was saved (new commits, modified files), flag this to the user
- If any tasks were captured in the session, consider recreating them
- The goal is a seamless handoff — the user should feel like we never left
