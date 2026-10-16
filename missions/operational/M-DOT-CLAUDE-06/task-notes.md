# M-DOT-CLAUDE-06: task notes

Imported from `doc/planning/plan.md` in the dotfiles repo, heading "Delta: Claude Instruction Refinements". The bullets are as written in the plan.

### T-01: Prose commentary rule
- ✓ DONE — New `## Prose` section in `home/claude/CLAUDE.md`: default to no editorial commentary when writing docs, READMEs, ADRs, comments and commit messages
  - Lists the five patterns to drop (editorial judgements, predicted reader reactions, meta-remarks about the writing, enthusiasm, needless hedging)
  - Draws the line explicitly: rationale and trade-offs are content and stay; commentary *on* the rationale goes
  - Explicit requests for opinion or critique override the default
- ✓ DONE — Applied retroactively to the go-tutorial docs written this session

### T-02: Remove global git-commit guard
- ✓ DONE — Removed the `## Version control` section from `home/claude/CLAUDE.md` (the "never run `git commit` unless asked" rule). Decided to move this to a per-repo setting instead of a blanket global rule
- Confirmed with current `code.claude.com/docs/en/memory` that a global `~/.claude/AGENTS.md` is not read by Claude Code. The user-level slot is fixed to `~/.claude/CLAUDE.md`; `AGENTS.md` support (v2.1.277+) is project-level only. Decision: keep `~/.claude/CLAUDE.md` as the global file, no AGENTS.md migration
