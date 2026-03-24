# Claude Code Configuration

This directory is symlinked to `~/.claude/` via the dotfiles repo.

## Session Management

Use `/save-session` and `/restore-session` to preserve and resume conversation context. This is **strongly preferred over letting conversations compact** — saved sessions capture richer, more intentional context than automatic compaction.

- **Before ending a conversation**: run `/save-session` with a brief description of what you were doing
- **When starting a new conversation** that continues previous work: run `/restore-session latest`
- Sessions are stored in `~/.claude/sessions/`

When you notice a conversation is getting long or the user mentions stopping, proactively suggest saving the session.
