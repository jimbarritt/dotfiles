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

### Tool definitions (disabledTools)

The harness loads ~25k tokens of tool schemas at startup. For projects with a narrow scope (like this dotfiles repo), many tools are never used.

**Safe to disable for dotfiles work:**
- Agent/TeamCreate — no swarms or multi-agent workflows
- NotebookEdit — no Jupyter notebooks
- RemoteTrigger, Monitor, ScheduleWakeup — not automating cloud jobs
- LSP — shell and config work don't need IDE features
- WebFetch, WebSearch — follow `doc/` lookup first, rarely needed
- EnterPlanMode, ExitPlanMode, EnterWorktree — straightforward edits, no complex planning
- TaskCreate, TaskGet, TaskList, TaskUpdate, TaskOutput, TaskStop — no complex task tracking

**To apply,** add to `.claude/settings.json`:

```json
{
  "disabledTools": [
    "Agent",
    "TeamCreate",
    "TeamDelete",
    "NotebookEdit",
    "RemoteTrigger",
    "Monitor",
    "ScheduleWakeup",
    "LSP",
    "WebFetch",
    "WebSearch",
    "EnterPlanMode",
    "ExitPlanMode",
    "EnterWorktree",
    "ExitWorktree",
    "TaskCreate",
    "TaskGet",
    "TaskList",
    "TaskUpdate",
    "TaskOutput",
    "TaskStop"
  ]
}
```

This frees ~10–12k tokens (~5% of startup context). Measure with `/context` before and after.

**Alternative: `/fewer-permission-prompts`**

Scans your transcript for common read-only Bash and MCP tool calls, then builds a targeted allowlist. More conservative than manual disabling — only removes tools you've genuinely never used. Run this after a few typical work sessions to let it learn your patterns.

### Conversation length

For long-running work, consider:
- Starting a fresh conversation and restoring a session rather than continuing indefinitely
- Saving a session before context gets compressed (lossy handoff vs clean handoff)

## Full tool inventory

The harness injects these tools at startup. Each contributes to the ~25k token system tools overhead.

### Core I/O (keep these)
- **Read** — read files
- **Edit** — edit files in-place
- **Write** — write new files
- **Bash** — execute shell commands

### Planning & Orchestration (likely removable for dotfiles)
- **Agent** — spawn subagents for parallel work
- **TeamCreate, TeamDelete** — multi-agent team management
- **SendMessage** — communicate with agents
- **EnterPlanMode, ExitPlanMode** — structured planning mode
- **EnterWorktree, ExitWorktree** — isolated git worktree sessions

### Task Management (likely removable)
- **TaskCreate, TaskGet, TaskList, TaskUpdate** — task tracking
- **TaskOutput, TaskStop** — task lifecycle

### Scheduling (likely removable unless you use recurring work)
- **CronCreate, CronDelete, CronList** — cron-based schedules
- **ScheduleWakeup** — self-paced task resumption

### Web & External (removable for dotfiles)
- **WebFetch** — fetch and process URLs
- **WebSearch** — search the web
- **RemoteTrigger** — trigger remote agents / APIs
- **Monitor** — stream background events

### Code Intelligence (removable for shell/config work)
- **LSP** — language server (IDE features like go-to-def)

### Specialized (removable)
- **NotebookEdit** — edit Jupyter notebooks
- **CronList** — list scheduled crons

### Skills (also loaded, ~1k tokens combined)
- **update-config** — configure settings.json (keep if you tune settings)
- **keybindings-help** — rebind keyboard shortcuts (keep if you customize)
- **simplify** — review code for quality
- **fewer-permission-prompts** — analyze usage and build allowlist
- **loop** — run prompt on recurring interval
- **schedule** — schedule remote agents on cron
- **claude-api** — build/debug Claude API apps
- **restore-session** — restore saved session
- **save-session** — save session context
- **proofread** — proofread text
- **publish-crate** — publish Rust crate
- **init** — initialize CLAUDE.md
- **review** — review pull request
- **security-review** — security review of changes

### Candidates for `disabledTools` in dotfiles project

Safe to drop for shell/config work (not needed):
- Agent, TeamCreate, TeamDelete, SendMessage
- EnterPlanMode, ExitPlanMode, EnterWorktree, ExitWorktree
- TaskCreate, TaskGet, TaskList, TaskUpdate, TaskOutput, TaskStop
- CronCreate, CronDelete, CronList, ScheduleWakeup
- WebFetch, WebSearch, RemoteTrigger, Monitor
- LSP
- NotebookEdit

Skills to keep:
- update-config (for settings tuning)
- keybindings-help (if you rebind)

Skills to drop (not used in dotfiles):
- simplify, fewer-permission-prompts, loop, schedule
- claude-api, restore-session, save-session
- proofread, publish-crate, init, review, security-review

### RTK (Rust Token Killer) — compress Bash output

RTK intercepts every `Bash` tool call via a `PreToolUse` hook and rewrites the command (e.g. `ls` → `rtk ls`) so the output is compressed before it enters the context window. Claims ~89% noise reduction on supported commands (cargo test, git status, grep, find, ls, etc.).

**Install:** already on system via Homebrew (`/opt/homebrew/bin/rtk`).

**Wire up (done — do not re-run):** manually added to `~/.claude/settings.json` alongside the existing `ctx-trakr` hook:

```json
{
  "hooks": {
    "PreToolUse": [
      { "matcher": "*",   "hooks": [{ "type": "command", "command": "ctx-trakr hook tool-use", "timeout": 5 }] },
      { "matcher": "Bash", "hooks": [{ "type": "command", "command": "rtk hook claude",        "timeout": 10 }] }
    ]
  }
}
```

**Monitor savings:** run `rtk gain` in your terminal (not inside Claude Code).

**Update:** `brew upgrade rtk` — `rtk update` is not a valid subcommand.

**Gotcha:** `ls` is aliased to `eza` in `.zshrc`. Aliases don't load in the non-interactive hook shell, so `rtk ls` calls the real `ls` binary internally. This is fine — compression still works.

## What we don't control

The system prompt and core I/O tools are injected by the Claude Code harness and form part of the ~8k–10k token baseline. These can't be reduced from the user side.

## Known Issues

**Agent type display inconsistency:** When spawning agents via the Agent tool, the harness sometimes doesn't display the agent type/model in the tool call UI. This is due to inconsistent harness behavior and parsing bugs, not a feature. Check the agent's output to infer its type, or use the agent ID if visible. Related: #24094, #31898, #20931, #20368.

## Monitoring context size — absolute tokens vs percentage

The statusline shows absolute token count in thousands (`ctx: 29k`), not percentage remaining.

**Why:** Percentage is relative to capacity and doesn't reflect whether the *amount* of context is enough to cause rot. A session at 15% of 200k (30k tokens) behaves very differently to 15% of a future 1M-token model. The absolute number is what matters empirically.

**How it's calculated:** `context_window.total_input_tokens + context_window.total_output_tokens` from the Claude Code statusline JSON. Not just `total_input_tokens` — output tokens accumulate in the conversation history too.

**Three-level warning system:**
- 0–149k: dim text — normal range, no concern
- 150–169k: bold text — approaching danger
- 170k+: `!!ctx:175k!!` bold with exclamation marks — danger zone

## Context rot canary — the secret word test

**Concept:** Place a "canary" string early in the conversation. When context gets long and auto-compaction fires, early turns get summarised lossily. Ask "what's the secret word?" — if Claude doesn't know, context rot has occurred.

**Why not CLAUDE.md:** The global/project CLAUDE.md is part of the system prompt, re-injected at position zero every turn. A canary there would never rot.

**Why not a file:** Claude can always re-read a file. That tests file access, not memory.

**The right place:** Early conversation turns — these are subject to compaction. The canary must be in the conversation history, not the system prompt.

**Implementation:** A `UserPromptSubmit` hook at `home/claude/hooks/canary-inject.sh`:
- Fires on every user message submit
- Uses a `/tmp/claude-canary-<session_id>` marker file to detect the first message of each session
- On first message only: outputs `additionalContext` JSON with the secret word
- `additionalContext` appears as a `<system-reminder>` in the conversation at turn 1, which is included in conversation history and is subject to later compaction
- Subsequent turns: exits 0 silently (no re-injection)

The hook is wired into `settings.json` under `UserPromptSubmit`.

**The secret word is `ramalamadingdong`.**

**Related benchmark:** This is a personal version of the industry-standard **Needle in a Haystack (NIAH)** test — place a "needle" (the canary) early in a long "haystack" (conversation), then probe for it later. Chroma's 2025 research found every frontier model (GPT-4.1, Claude Opus 4, Gemini 2.5) degrades at increasing context lengths: https://www.trychroma.com/research/context-rot
