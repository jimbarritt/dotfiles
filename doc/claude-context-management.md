# Claude Code context management

What gets loaded into the context window on every prompt, and how to keep it lean.

## Per-prompt fixed costs

| Source | Tokens (approx) | Controllable? |
|---|---|---|
| System prompt (harness, tools, rules) | ~8,000–10,000 | No |
| Tool schemas (deferred, loaded on use) | varies | No |
| Global `~/.claude/CLAUDE.md` | ~50 | Yes — keep minimal |
| Project `CLAUDE.md` | ~90 | Yes — keep minimal |
| `MEMORY.md` index (first 200 lines) | ~50–100 | Yes — prune regularly |
| Git status snapshot | ~100 | No |

Total fixed overhead: **~8,500–10,500 tokens** before any conversation happens.

## Conversation costs

- Each user message + assistant reply adds to the context.
- Long conversations are the main driver of context growth.
- The harness auto-compresses older messages as context limits approach, but compressed context is lossy.
- Restoring a session (`/restore-session`) loads the session file into the conversation — larger sessions cost more.

## What we control

### CLAUDE.md files

Keep these as terse as possible. Only include:
- Instructions that change Claude's default behaviour (rules, constraints)
- Pointers to where things are (not descriptions of how they work — Claude can read the code)

Avoid: architecture explanations, file trees, code patterns, anything derivable by reading the repo.

Current project CLAUDE.md: ~90 tokens (trimmed 2026-04-12 from ~350).

### MEMORY.md

One-line entries only. Content lives in individual memory files (loaded on demand, not per-prompt). Keep the index under 200 lines.

### Session files

Only loaded when explicitly restored. Cost is proportional to session length. Keep "What's left to do" and "How to resume" sections tight — the detailed history is for human reference, not re-ingestion.

### Conversation length

For long-running work, consider:
- Starting a fresh conversation and restoring a session rather than continuing indefinitely
- Saving a session before context gets compressed (lossy handoff vs clean handoff)

## What we don't control

The system prompt, tool definitions, skill list, and deferred tool catalogue are injected by the Claude Code harness. These are the largest fixed cost (~8k–10k tokens) and can't be reduced from the user side.
