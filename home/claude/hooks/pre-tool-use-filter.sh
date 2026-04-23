#!/bin/bash
# pre-tool-use-filter.sh
#
# PreToolUse hook for Claude Code.
# Blocks dangerous Bash commands even in --dangerously-skip-permissions mode.
#
# Receives a JSON payload on stdin with the shape:
#   { "tool_name": "Bash", "tool_input": { "command": "..." } }

set -euo pipefail

INPUT=$(cat /dev/stdin)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty')

deny() {
  local reason="$1"
  echo "{\"hookSpecificOutput\":{\"hookEventName\":\"PreToolUse\",\"permissionDecision\":\"deny\",\"permissionDecisionReason\":\"$reason\"}}"
  exit 0
}

# ---------------------------------------------------------------------------
# Destructive file operations
# ---------------------------------------------------------------------------
if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)rm\s'; then
  deny "rm is blocked — delete files manually if needed"
fi

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)rmdir\s'; then
  deny "rmdir is blocked — remove directories manually if needed"
fi

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)shred\s'; then
  deny "shred is blocked — destructive file wipe"
fi

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)truncate\s'; then
  deny "truncate is blocked — destructive file operation"
fi

# ---------------------------------------------------------------------------
# Git push (all variants) — push manually to retain rollback control
# ---------------------------------------------------------------------------
if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)git\s+push(\s|$)'; then
  deny "git push is blocked — push manually to retain rollback control"
fi

# ---------------------------------------------------------------------------
# Git commit — commit manually to retain control over what gets recorded
# ---------------------------------------------------------------------------
if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)git\s+commit(\s|$)'; then
  deny "git commit is blocked — commit manually to retain control over what gets recorded"
fi

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)git\s+add(\s|$)'; then
  deny "git add is blocked — stage files manually"
fi

# ---------------------------------------------------------------------------
# Git history rewriting / destructive git operations
# ---------------------------------------------------------------------------
if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)git\s+reset\s+--hard'; then
  deny "git reset --hard is blocked — would discard uncommitted changes"
fi

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)git\s+clean\s+-[a-zA-Z]*f'; then
  deny "git clean -f is blocked — would permanently delete untracked files"
fi

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)git\s+rebase(\s|$)'; then
  deny "git rebase is blocked — rewrites commit history"
fi

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)git\s+branch\s+-[a-zA-Z]*D'; then
  deny "git branch -D is blocked — force-deletes branches"
fi

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)git\s+tag\s+-d'; then
  deny "git tag -d is blocked — deletes tags"
fi

# ---------------------------------------------------------------------------
# Privilege escalation
# ---------------------------------------------------------------------------
if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)sudo\s'; then
  deny "sudo is blocked in autonomous mode"
fi

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)su\s'; then
  deny "su is blocked in autonomous mode"
fi

# ---------------------------------------------------------------------------
# Package / artefact publishing
# ---------------------------------------------------------------------------
if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)npm\s+publish(\s|$)'; then
  deny "npm publish is blocked — publish manually"
fi

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)cargo\s+publish(\s|$)'; then
  deny "cargo publish is blocked — publish manually"
fi

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)wrangler\s+(deploy|publish)(\s|$)'; then
  deny "wrangler deploy is blocked — deploy manually"
fi

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)npx\s+wrangler\s+(deploy|publish)(\s|$)'; then
  deny "npx wrangler deploy is blocked — deploy manually"
fi

# ---------------------------------------------------------------------------
# Pipe-to-shell (supply chain / remote code execution)
# ---------------------------------------------------------------------------
if echo "$COMMAND" | grep -qE '\|\s*(bash|sh|zsh|fish|dash)(\s|$|")'; then
  deny "pipe-to-shell is blocked — do not execute remote scripts directly"
fi

if echo "$COMMAND" | grep -qE '(curl|wget).*(bash|sh|zsh)'; then
  deny "fetching and executing remote scripts is blocked"
fi

# ---------------------------------------------------------------------------
# Process destruction
# ---------------------------------------------------------------------------
if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)kill\s+-9'; then
  deny "kill -9 is blocked in autonomous mode"
fi

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)(pkill|killall)\s'; then
  deny "pkill/killall is blocked in autonomous mode"
fi

# ---------------------------------------------------------------------------
# Disk and device operations
# ---------------------------------------------------------------------------
if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)dd\s'; then
  deny "dd is blocked — direct disk writes are too dangerous"
fi

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)mkfs(\.|$|\s)'; then
  deny "mkfs is blocked — filesystem formatting"
fi

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)diskutil\s+erase'; then
  deny "diskutil erase is blocked"
fi

# ---------------------------------------------------------------------------
# Cloud destructive operations
# ---------------------------------------------------------------------------
if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)aws\s+s3\s+rm(\s|$)'; then
  deny "aws s3 rm is blocked — delete S3 objects manually"
fi

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)aws\s+ec2\s+terminate-instances(\s|$)'; then
  deny "aws ec2 terminate-instances is blocked"
fi

# ---------------------------------------------------------------------------
# Crontab wipe
# ---------------------------------------------------------------------------
if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)crontab\s+-r'; then
  deny "crontab -r is blocked — would wipe all scheduled jobs"
fi

# ---------------------------------------------------------------------------
# All clear
# ---------------------------------------------------------------------------
exit 0
