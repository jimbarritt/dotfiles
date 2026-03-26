---
name: save-session
description: Save the current conversation session context to a file for later restoration
argument-hint: "[optional description of what we were working on]"
allowed-tools: Read, Write, Bash, Glob, Grep
disable-model-invocation: false
---

# Save Session

Save the current conversation session so it can be restored later, preserving full working context.

## What to capture

Write a session file to `.claude-sessions/session-{timestamp}.md` (relative to the repo root). To get the timestamp, you MUST run this shell command:

```sh
date +%Y-%m-%dT%H-%M-%S%z
```

Do NOT guess or infer the current time from conversation context — always use the `date` command to get the real local time. Create the `.claude-sessions/` directory if it doesn't exist.

The session file should contain:

### 1. Header
- Session ID: `${CLAUDE_SESSION_ID}`
- Timestamp (human readable)
- Working directory
- Git branch and short status
- User-provided description (from `$ARGUMENTS`, if any)

### 2. What we were doing
A clear, detailed summary of the task(s) in progress. Write this as if briefing a fresh Claude instance that has never seen this conversation. Include:
- The overall goal / user request
- Why we're doing it (motivation, context)
- Any constraints or preferences the user expressed

### 3. What we've done so far
- Files created or modified (with paths)
- Key decisions made and why
- Commands run and their outcomes
- Any problems encountered and how they were resolved

### 4. What's left to do
- Remaining steps, in order
- Any blockers or open questions
- Things we were about to do when the session was saved

### 5. Key context
- Important file paths, function names, or code patterns that would take time to rediscover
- Any non-obvious details that came up during the conversation
- References to external resources (URLs, docs, tickets) mentioned

### 6. How to resume
A short paragraph telling the restoring Claude exactly how to pick up where we left off. Be specific — "continue implementing X by doing Y" not "resume work".

## After saving

- Confirm the file was written and show the path
- Show a brief summary of what was captured
- Tell the user they can restore with `/restore-session`

## Important

- Be thorough — the whole point is that context survives across sessions
- Write in plain language, not shorthand — a fresh instance needs to understand everything
- If there are active tasks (from TaskList), capture their state too
- If there's a plan in progress, capture it in full
