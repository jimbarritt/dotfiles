# Claude Code: the `advisor` tool

## Short version

`advisor` is an official Claude Code tool that lets Claude escalate to a
stronger reviewer model mid-conversation. It takes no parameters — calling it
forwards the entire conversation transcript (task, tool calls, reasoning) to
the configured advisor model, which reviews and responds.

Claude decides when to call it (before substantive work, when stuck, or before
declaring a task done) — there's no user command needed to trigger it turn by
turn.

## Configuration

Controlled via the `advisorModel` key, settable in any settings scope:

- User: `~/.claude/settings.json`
- Project: `.claude/settings.json`
- Local: `.claude/settings.local.json`

Project settings override user settings.

```json
{
  "advisorModel": "fable"
}
```

## Valid values

Model names such as `fable`, `opus`, `sonnet` (or full model IDs, e.g.
`claude-opus-4-8`). The advisor must be **at least as capable as the main
model** — valid pairings depend on which model Claude Code is running as.

## Turning it on/off

- `/advisor <model>` — set or change the advisor mid-session (writes to user
  settings, same persistence behaviour as `/model`).
- `/advisor off` — disable for the session/going forward.
- `--advisor <model>` — CLI flag, session-only override.
- `CLAUDE_CODE_DISABLE_ADVISOR_TOOL=1` — environment variable to disable the
  tool entirely (e.g. per-shell or per-project).

## Cost

Each advisor call sends the full transcript to the advisor model — billed
separately at that model's rates (API) or counted against plan limits
(subscription). Because it's only invoked at decision points rather than every
turn, pairing a cheap main model with a stronger advisor is typically cheaper
overall than running the stronger model throughout.

## Requirements

- Claude Code v2.1.98+
- Anthropic API (not available via Bedrock/Vertex)

## Source

- <https://code.claude.com/docs/en/advisor.md>
- <https://code.claude.com/docs/en/settings.md>
