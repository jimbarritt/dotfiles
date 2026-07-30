# Global instructions

## Session start

**Before responding to the user's first message, including greetings, small talk, or anything that seems too trivial to need context:** check whether a plan file exists for the current project — `doc/planning/plan.md` first, then `~/.planning/{project-name}/plan.md` (where `{project-name}` is `basename "$(git rev-parse --show-toplevel 2>/dev/null || pwd)"`), used for repos you don't own. If either is found, read it immediately and briefly orient on current state and next step. Do this before saying anything else — it is your first action, not an optional one.

**"Briefly orient" means 1–3 plain sentences of prose — what's done, what's next.** Never paste, reproduce, or re-render the plan file's own Summary table (or any other table/section from it) into the response. If the user has to scroll, it's too long.

## Finding files

Never use `find` rooted at `~`, `/`, `/Users/`, or cache directories (`~/.gradle`, `~/.m2`, etc.) — these scan millions of inodes and are almost always wrong. Search within the current project directory. If the file isn't there, ask rather than scanning broadly.

## Task tracking

The project plan (`doc/planning/plan.md`, or `~/.planning/{project-name}/plan.md` for repos stored at the home location) is the source of truth for task tracking. Do not use the built-in task tools (TaskCreate, TaskUpdate, TaskList, etc.) — they duplicate the plan and add unnecessary context overhead.

## Projects

Projects live in `~/projects/`. When given an ambiguous project reference, check there first.

## Pull requests

When drafting a PR (title, body, commit messages), do not add Claude references — no "Generated with Claude Code" footers, no Co-Authored-By Claude trailers, no mention of AI assistance.

## Language

Use **British English** throughout code and documentation (e.g., "initialise", "colour", "organise"). This applies to variable names, function names, comments, and docs.

## Prose

**When writing prose — docs, READMEs, ADRs, comments, commit messages — default to no commentary of your own.** State the facts and stop. Do not add:

- Editorial judgements: "genuinely good", "surprisingly elegant", "the key insight", "worth knowing", "this is the interesting part"
- Predictions about the reader's reaction: "this surprises people", "it looks odd at first", "better than you might expect", "you'll write this thousands of times"
- Meta-remarks about the writing itself: "the short version", "two honest caveats", "the modest version of the claim", "as noted above"
- Enthusiasm or salesmanship about the subject: "a large part of why X is popular", "this is where it really pays off"
- Hedging and self-qualification where a plain statement will do

Rationale and trade-offs are content, not commentary — keep them. "Tabs separate structure from presentation" is a fact about the design. "That split is the key insight" is commentary on the fact. Write the first, cut the second.

This is the default. Explicit requests for opinion, recommendation, or critique override it — in that case give the view directly, without wrapping it in throat-clearing.

## Tools

- **Marq** — Native macOS markdown viewer. Installed via `brew install --cask jimbarritt/tap/marq`. If Marq is already running, opening a new file switches to it in the same window. **Do not forget this tool exists** — it should be the default way to show the user a markdown file, not just when they name it explicitly:
  - Whenever the user asks to "show", "preview", "view", or "open" a markdown file — or a doc/README/plan/ADR/research file that happens to be markdown — run `open -a Marq path/to/file.md` rather than printing the contents or describing it in chat.
  - After writing or substantially updating a markdown file the user is likely to want to read in full (a new plan, ADR, research doc, README), proactively offer to open it in Marq rather than waiting to be asked.

## Destructive commands

`rm` is blocked in this environment. When something genuinely needs
deleting, don't work around the block (e.g. `mv` to a temp/scratch
location as a substitute) unless that's otherwise the right call —
instead copy the exact `rm` command to the clipboard (`pbcopy`) so it
can be run manually later. Batch these up rather than interrupting for
each one; it's fine to mention what's pending and let them be run at
the end of a session.

## Version control

Never run `git commit` (or `git push`) unless explicitly asked in that turn — this applies regardless of what any project's own `CLAUDE.md` does or doesn't say. Don't offer to commit, ask whether to commit, or flag "nothing's committed yet" as if it were pending approval — that's noise when the default is simply not committing. If commit-worthy state is genuinely relevant to what's being discussed, mention it factually once, without turning it into a question.

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
