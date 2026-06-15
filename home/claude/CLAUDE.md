# Global instructions

## Session start

**Before responding to the user's first message:** check whether `doc/planning/plan.md` exists in the current project. If it does, read it immediately and briefly orient on current state and next step. Do this before saying anything else — it is your first action, not an optional one.

## Language

Use **British English** throughout code and documentation (e.g., "initialise", "colour", "organise"). This applies to variable names, function names, comments, and docs.

## Tools

- **Marq** — Native macOS markdown viewer. When asked to "show me the markdown", "preview this", or "open in Marq", run `open -a Marq path/to/file.md`. If Marq is already running, this switches to the new file in the same window. Installed via `brew install --cask jimbarritt/tap/marq`.

## Agents

- Delegate self-contained doc updates to a general-purpose agent rather than doing them inline. "Read this file, make these edits based on X" is always a good agent candidate — it costs ~1% context vs 3-5% inline.
- **Use background agents by default** for iterative work. Send messages to agents (via SendMessage) and let them run in the background while you continue — get notified when done. More efficient than waiting inline for results, especially for multi-turn refinement loops.

## Compaction

When compacting, prioritise preserving:
- Current task state and what's left to do (ordered)
- Files created or modified (with paths)
- Key decisions made and why
- Non-obvious context: gotchas, constraints, workarounds, external references

Discard: exploratory tangents, superseded approaches, verbose tool output, reasoning steps that led to a dead end.
