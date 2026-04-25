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

### Built-in Claude Code MCP services

Claude Code auto-injects MCP connectors for Gmail, Google Calendar, Google Drive, and Slack at session start. These consume ~10.7k tokens (~5% of context) even if unused.

**To disable them globally,** set `ENABLE_CLAUDEAI_MCP_SERVERS=false` in `~/.claude/settings.json`:

```json
{
  "env": {
    "ENABLE_CLAUDEAI_MCP_SERVERS": "false"
  }
}
```

**Why `disabledMcpjsonServers` doesn't work:** That setting only blocks servers defined in `.mcp.json` files. These built-in connectors are cloud-injected and require the env var to disable.

**Granularity:** The env var is all-or-nothing; there's no per-server control yet (see [#31249](https://github.com/anthropics/claude-code/issues/31249) for the feature request).

### Conversation length

For long-running work, consider:
- Starting a fresh conversation and restoring a session rather than continuing indefinitely
- Saving a session before context gets compressed (lossy handoff vs clean handoff)

## What we don't control

The system prompt, tool definitions, skill list, and deferred tool catalogue are injected by the Claude Code harness. These are the largest fixed cost (~8k–10k tokens) and can't be reduced from the user side.
