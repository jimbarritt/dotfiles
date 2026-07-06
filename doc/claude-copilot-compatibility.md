# Claude Code ↔ GitHub Copilot Compatibility

Notes on keeping the Claude Code config in this repo usable from GitHub Copilot (CLI and IDE agents), so the two tools share one set of instructions and skills rather than drifting apart.

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

For completeness — the per-repo equivalents differ:

| Claude Code | Copilot |
|---|---|
| `CLAUDE.md` in repo root | `.github/copilot-instructions.md`, or `AGENTS.md` in repo root |

Copilot CLI also reads `AGENTS.md` from directories listed in the `COPILOT_CUSTOM_INSTRUCTIONS_DIRS` environment variable, and scoped `.github/instructions/*.instructions.md` files.

## Sources

- [GitHub Docs — Adding agent skills for Copilot](https://docs.github.com/en/copilot/how-tos/copilot-on-github/customize-copilot/customize-cloud-agent/add-skills) — frontmatter requirements
- [VS Code — Use Agent Skills](https://code.visualstudio.com/docs/agent-customization/agent-skills) — name-must-match-directory rule
- [GitHub Docs — Adding custom instructions for Copilot CLI](https://docs.github.com/en/copilot/how-tos/copilot-cli/customize-copilot/add-custom-instructions) — confirms `$HOME/.copilot/copilot-instructions.md` as the global instructions path
