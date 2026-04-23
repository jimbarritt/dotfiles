---
name: save-session
description: Save the current conversation session context to a file for later restoration
argument-hint: "[optional description of what we were working on]"
allowed-tools: Read, Write, Bash, Glob, Grep
disable-model-invocation: false
model: haiku
---

# Save Session

Save the current conversation to `.claude-sessions/session-{timestamp}.md` relative to the repo root. Create the directory if it doesn't exist.

## Steps

1. Run `date +%Y-%m-%dT%H-%M-%S%z` to get the timestamp — never infer it.
2. Run `git status --short` and `git log --oneline -3` for repo state.
3. Write the session file using the template below.
4. Confirm the path and show a 3-bullet summary of what was captured.

## Template

```markdown
# Session: {one-line title}

- **Session ID**: ${CLAUDE_SESSION_ID}
- **Saved**: {human-readable timestamp}
- **Working dir**: {cwd}
- **Branch**: {branch}
- **Status**: {clean | dirty — list changed files} (last commit: {hash} "{message}")

---

## What we were doing

{2–4 sentences. Enough for a fresh Claude to understand the goal and context.}

---

## What we've done this session

{Bullet list. File paths, decisions, outcomes. Be specific — include line numbers or function names where they'll save rediscovery time.}

---

## What's left to do

{Ordered bullet list of remaining steps. Flag blockers or open questions.}

---

## Key context

{Non-obvious details: gotchas, constraints, why a decision was made, external references. Skip anything obvious from the code.}

---

## How to resume

{One short paragraph. "Pick up at X by doing Y" — specific, not vague.}
```

## Constraints

- Session file: 400–600 words. Be thorough but cut filler.
- If active tasks exist, capture their state in "What's left to do".
- Tell the user they can restore with `/restore-session`.
