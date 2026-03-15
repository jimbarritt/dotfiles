# Claude Workflow Notes

## Sessions

Named sessions can be resumed with `claude --resume`. Useful for long-running
work across multiple sittings.

## Subagents

### When they help
- **Parallel independent research** — explore two things simultaneously
- **Context protection** — deep codebase exploration that would flood the main context; agent summarises and returns just what matters
- **Isolated implementation** — well-scoped feature work in a worktree, no tight feedback loop needed
- **Background tasks** — kick off something slow (tests, builds) while continuing the conversation

### When they don't add much
- Simple edits or searches — overhead isn't worth it
- Tasks needing user feedback mid-way — subagents can't easily ask questions
- Anything where you want to review each step

### When Claude should spawn one proactively
- Before making changes, when codebase exploration is needed — use an Explore
  agent rather than doing it inline
- When two independent research questions arise at the same time

### How to tell if Claude is using one
- An Agent tool call appears in the conversation with a description of what it's doing
- If you see lots of Grep/Glob/Read calls in sequence, Claude should have used an
  agent instead — call it out
