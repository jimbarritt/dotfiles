# Claude Code ↔ GitHub Copilot Compatibility

Notes on keeping the Claude Code config in this repo usable from GitHub Copilot (CLI and IDE agents), so the two tools share one set of instructions and skills rather than drifting apart.

## Installing Copilot

The current Copilot CLI is a standalone agentic terminal tool (Claude Code's equivalent), shipping as its own `copilot` binary:

```sh
brew install copilot-cli
```

or via npm: `npm install -g @github/copilot`. Homebrew (and WinGet/install-script) installs auto-update; npm installs don't — see the [official install docs](https://docs.github.com/en/copilot/how-tos/copilot-cli/set-up-copilot-cli/install-copilot-cli).

Don't confuse it with two older things:

- **`gh-copilot`** — the GitHub CLI extension (`gh copilot suggest` / `explain`). Retired January 2026; `gh copilot` now just installs and delegates to the standalone CLI.
- **`@githubnext/github-copilot-cli`** — the original technical-preview npm package. Dead; use `@github/copilot`.

It's the standalone binary that reads the config this doc covers: `~/.copilot/copilot-instructions.md`, `AGENTS.md`, and agent skills.

## Skills: YAML frontmatter is required by Copilot

Both tools read the same `SKILL.md` layout (one directory per skill, instructions in Markdown), but Copilot **requires** YAML frontmatter at the top of each `SKILL.md`:

```markdown
---
name: my-skill
description: What the skill does and when to use it.
---

# My Skill
...
```

- `name` — required, lowercase with hyphens, and **must exactly match the skill's directory name** (`skills/my-skill/` → `name: my-skill`). If it doesn't match, Copilot silently skips the skill.
- `description` — required; what the skill does and when Copilot should invoke it.
- `license` — optional.

Claude Code accepts the same frontmatter, so adding it costs nothing on the Claude side. All skills in `home/claude/skills/` now carry it — **any new skill must include it too**, or it will work in Claude Code but silently fail to load in Copilot.

## Global instructions: one file, two symlinks

Copilot CLI's equivalent of the global `~/.claude/CLAUDE.md` is `~/.copilot/copilot-instructions.md` — a personal instructions file applied to every session in every project.

Rather than maintaining two copies, both are symlinks to the same tracked file:

| Tool | Link | Target |
|---|---|---|
| Claude Code | `~/.claude/CLAUDE.md` | `home/claude/CLAUDE.md` |
| Copilot CLI | `~/.copilot/copilot-instructions.md` | `home/claude/CLAUDE.md` |

The Copilot link is created by `./do.sh link-copilot` (also included in `./do.sh link`).

**Caveat:** `home/claude/CLAUDE.md` contains some Claude-Code-specific instructions (built-in task tools, compaction guidance, skill invocation). Copilot will read these but can't act on all of them — harmless, but if the file grows more Claude-specific machinery, consider splitting the shared parts out.

## Per-repo instructions

The per-repo equivalents differ:

| Claude Code | Copilot |
|---|---|
| `CLAUDE.md` in repo root | `.github/copilot-instructions.md`, or `AGENTS.md` in repo root |

Copilot CLI also reads `AGENTS.md` from directories listed in the `COPILOT_CUSTOM_INSTRUCTIONS_DIRS` environment variable, and scoped `.github/instructions/*.instructions.md` files.

### Recommended pattern: AGENTS.md canonical, CLAUDE.md imports it

For repos that need cross-agent compatibility, make `AGENTS.md` the source of truth and reduce `CLAUDE.md` to an import plus any Claude-only extras:

```markdown
@AGENTS.md

<!-- Claude-specific instructions below, invisible to other tools -->
```

Why this direction and not the reverse (canonical `CLAUDE.md`, with `AGENTS.md` pointing at it):

- **Claude Code does not read `AGENTS.md` natively** (confirmed by Anthropic, May 2026) — but `CLAUDE.md` supports `@path` import syntax, expanded mechanically at session start as if the imported content were written inline. The pointer is a guaranteed preprocessing step, not a suggestion the model may skip.
- **`AGENTS.md` has no import mechanism.** A reverse pointer ("read CLAUDE.md for the real rules") relies on each tool's model *choosing* to follow it — probabilistic, one extra read per session, silently fails if skimmed past. And that gamble repeats across every non-Claude tool, whereas the import solves it once for the one tool with a proper mechanism.
- **Everything else reads `AGENTS.md` natively** — Copilot CLI, Codex, Cursor, Gemini CLI all treat it as the emerging cross-vendor standard, so they need no pointer at all.
- **The import beats a symlink** because `CLAUDE.md` stays a real file: genuinely Claude-specific instructions (task-tool rules, skill invocation notes) can sit under the import line without polluting what other tools see. Imports resolve relative to the importing file and can nest up to four hops deep.

Sources for this section: [official @AGENTS.md import confirmation](https://gist.github.com/yurukusa/d36197848911f025add142abefcde685), [travis.media — CLAUDE.md follows AGENTS.md without a symlink](https://travis.media/blog/claude-md-import-agents-md/), [codex.danielvaughan.com — cross-tool portability](https://codex.danielvaughan.com/2026/05/27/agent-instruction-files-agents-md-claude-md-cross-tool-portability-codex-cli/).

## Sources

- [GitHub Docs — Adding agent skills for Copilot](https://docs.github.com/en/copilot/how-tos/copilot-on-github/customize-copilot/customize-cloud-agent/add-skills) — frontmatter requirements
- [VS Code — Use Agent Skills](https://code.visualstudio.com/docs/agent-customization/agent-skills) — name-must-match-directory rule
- [GitHub Docs — Adding custom instructions for Copilot CLI](https://docs.github.com/en/copilot/how-tos/copilot-cli/customize-copilot/add-custom-instructions) — confirms `$HOME/.copilot/copilot-instructions.md` as the global instructions path
- [GitHub Docs — Installing GitHub Copilot CLI](https://docs.github.com/en/copilot/how-tos/copilot-cli/set-up-copilot-cli/install-copilot-cli) — install methods and auto-update behaviour for the standalone CLI
- [GitHub Changelog — Copilot CLI via the GitHub CLI (Jan 2026)](https://github.blog/changelog/2026-01-21-install-and-use-github-copilot-cli-directly-from-the-github-cli/) — `gh copilot` now delegates to the standalone CLI; old extension retired
