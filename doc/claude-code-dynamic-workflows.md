# Claude Code — Dynamic Workflows

Notes captured 2026-05-28 (shipped with Opus 4.8, research preview, Claude Code
v2.1.154+). Source links at the bottom.

## What it actually is

A **dynamic workflow is a JavaScript script that Claude writes to orchestrate
subagents** — and a runtime executes that script in the background while your
session stays responsive. The crucial shift is *who holds the plan*:

|                                  | Subagents              | Skills                | **Workflows**                    |
|----------------------------------|------------------------|-----------------------|----------------------------------|
| What it is                       | a worker Claude spawns | instructions Claude follows | **a script the runtime runs** |
| Who decides what's next          | Claude, turn by turn   | Claude                | **the script**                   |
| Where intermediate results live  | Claude's context       | Claude's context      | **script variables**             |
| Scale                            | a few per turn         | same                  | **dozens–hundreds of agents**    |

The payoff: with subagents, every intermediate result lands back in Claude's
context window (burning tokens, and Claude has to re-decide each step). A
workflow moves the loop, branching, and intermediate state **into code**, so
your context only ever sees the *final* answer. It can also codify a **quality
pattern** — e.g. independent agents adversarially reviewing each other's
findings, or drafting a plan from several angles and weighing them — not just
"run more agents."

## How you trigger it

- **Put the word `workflow` in a prompt** → Claude writes a one-off workflow for
  that task. (`alt+w` to ignore if it triggers by accident.)
- **`/effort ultracode`** → combines `xhigh` reasoning with *automatic* workflow
  planning for every substantive task in the session (one request might become
  several workflows: understand → change → verify). Resets each session; drop
  back with `/effort high`.
- **`/deep-research <question>`** → the bundled workflow: fans out web searches
  across angles, cross-checks sources, votes on each claim, returns a cited
  report with unsupported claims filtered out.
- Watch progress with **`/workflows`** (per-phase agent counts, tokens, elapsed;
  pause/resume/stop individual agents). Save a good run's script with `s` → it
  becomes a `/command`.

## Limits & caveats

- **Up to 1,000 agents total per run, max 16 concurrent** (fewer on low-core
  machines).
- Spawned subagents always run in `acceptEdits` mode and inherit your tool
  allowlist — **file edits auto-approve** regardless of your session's
  permission mode. Add shell/MCP commands to your allowlist beforehand so a long
  run doesn't stall on prompts.
- **No mid-run user input** (only permission prompts can pause it) — for sign-off
  between stages, run each stage as its own workflow.
- **Resumable within the same session**; exit Claude Code and it restarts fresh.
- **Cost:** materially more tokens than doing the task in conversation, since
  every agent pays its own context overhead. Counts toward your plan limits.
  Check `/model` before a big run.
- **Turn off:** toggle in `/config`, or `"disableWorkflows": true` in
  settings.json, or `CLAUDE_CODE_DISABLE_WORKFLOWS=1`.

## Sources

- [Claude Code Docs — Orchestrate subagents at scale with dynamic workflows](https://code.claude.com/docs/en/workflows)
- [Anthropic — Introducing dynamic workflows in Claude Code](https://claude.com/blog/introducing-dynamic-workflows-in-claude-code)
- [TechCrunch — Opus 4.8 with new 'dynamic workflow' tool](https://techcrunch.com/2026/05/28/anthropic-releases-opus-4-8-with-new-dynamic-workflow-tool/)
