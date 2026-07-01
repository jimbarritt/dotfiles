# Superpowers — Agentic Skills Framework for Claude Code

## What it is

Superpowers is an **open-source Claude Code plugin** that injects a structured skills library and a disciplined development methodology into the agent. Not a CLI tool or paid service — it is a set of composable Skills (markdown `SKILL.md` files) plus an orchestration layer that makes the agent consult those skills before acting.

- **Author:** Jesse Vincent (GitHub: `obra`), Prime Radiant (primeradiant.com)
- **Primary repo:** https://github.com/obra/superpowers
- **Official plugin listing:** https://claude.com/plugins/superpowers (in Anthropic's marketplace since ~Jan 2026)
- **Licence:** MIT, free, no paid tier
- **Cross-agent:** also works with Cursor, Gemini CLI, GitHub Copilot CLI, Codex, OpenCode, etc.

Simon Willison endorses it as "a really significant piece" and "absolutely worth spending time on."

## Problem it solves

Coding agents jump straight to writing code, skip planning, skip tests, and drift off-course. Superpowers imposes a **brainstorm → plan → implement → review** discipline:

- **`/brainstorm`** — Socratic questioning to refine requirements before any code is written
- **`/write-plan`** — breaks work into small (2–5 min), precisely-specified tasks
- **`/execute-plan`** — dispatches subagents in isolated git worktrees to do the work, with a two-stage code review gate before anything is "done"
- **TDD enforcement** — strict RED → GREEN → REFACTOR (tests must fail before fixing)
- **Systematic debugging** — multi-phase root-cause methodology before applying any fix
- **Meta-skills** — authoring new skills, extracting memory from past sessions, "pressure-testing" skills

Skills activate automatically when relevant. The "superpowers" meta-skill makes the agent check, on each message, which skills apply.

## Installation

Simplest (official marketplace):
```bash
/plugin install superpowers@claude-plugins-official
```

obra's own marketplace (gets latest + related plugins):
```bash
/plugin marketplace add obra/superpowers-marketplace
/plugin install superpowers@superpowers-marketplace
```

Community skills (separate, community-editable):
- https://github.com/obra/superpowers-skills

## Fit with this setup

The workflow leans heavily on **git worktrees and subagents**, which aligns well with the existing setup (background agents, worktree tooling). Telemetry/write behaviours are disableable via env vars.

## Trade-offs and limitations

- **Token overhead is the main complaint.** Jesse says <2,000 tokens initial footprint with heavy work pushed to subagents; Simon confirms a full plan+implement run at ~100k tokens total. But real issues exist:
  - [Issue #190](https://github.com/obra/superpowers/issues/190): all skills preloaded at startup consuming 22k+ tokens (~11% of context) — only frontmatter should load during discovery
  - [Issue #227](https://github.com/obra/superpowers/issues/227): `write-plan` exceeding the 32k output-token limit; some users disable that skill as a workaround
  - [Issue #832](https://github.com/obra/superpowers/issues/832): an optimisation pass cut all 14 skills from 3,150 → 977 lines (~69%) with no behavioural regression — actively being fixed
- **Benefits are uneven.** An independent 12-session A/B test: ~14% fewer tokens, ~9% lower cost on average — concentrated on medium/complex tasks. Minimal benefit on simple tasks.
- **Friction for quick edits.** The brainstorm → plan → TDD ceremony is overkill for one-line changes. Best for non-trivial features.
- **Contribution model:** core repo does not generally accept new-skill PRs — community skills live in the separate repo.

## Realistic expectations

Gains are most visible on medium/complex features where the agent would otherwise drift. For quick fixes or well-understood tasks, the overhead outweighs the benefit. The "100x productivity" headlines are hype — but the discipline it imposes (plan before code, tests first, self-review) does reduce correction cycles.

## Fit assessment

The skills add the most value for people who don't already have a planning discipline — Superpowers stops them from getting half-baked implementations with no tests. If you're already using `plan.md`, `init-plan`/`update-plan` skills, and asking Claude to plan before coding, the incremental benefit is modest. Claude Code natively supports plan mode, brainstorming on request, and TDD if you ask for it.

The one thing that's hard to replicate natively is parallel subagents in worktrees — running multiple agents simultaneously on independent tasks. That's a throughput multiplier, not a quality one, and it requires committing to the worktree model.

**Verdict:** probably not worth installing unless you hit a project with long parallel workstreams across a large codebase. Revisit then. The planning skills are good, but if you already plan, you're paying token overhead for discipline you're already applying manually.

## Sources

- [Repo](https://github.com/obra/superpowers) — obra/superpowers
- [Author's announcement](https://blog.fsck.com/2025/10/09/superpowers/)
- [Simon Willison endorsement](https://simonwillison.net/2025/Oct/10/superpowers/)
- [Official plugin listing](https://claude.com/plugins/superpowers)
- [Independent benchmark](https://www.mejba.me/blog/superpowers-plugin-claude-code-review)
- GitHub issues: [#190](https://github.com/obra/superpowers/issues/190), [#227](https://github.com/obra/superpowers/issues/227), [#832](https://github.com/obra/superpowers/issues/832)
