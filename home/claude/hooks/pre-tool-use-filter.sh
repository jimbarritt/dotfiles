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
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty' 2>/dev/null) || COMMAND=""

deny() {
  local reason="$1"
  echo "{\"hookSpecificOutput\":{\"hookEventName\":\"PreToolUse\",\"permissionDecision\":\"deny\",\"permissionDecisionReason\":\"$reason\"}}"
  exit 0
}

# ---------------------------------------------------------------------------
# Destructive file operations
# ---------------------------------------------------------------------------
# rm -rf (any flag spelling combining recursive + force) is always blocked —
# also backstopped as a static permissions.deny pattern in settings.json,
# since a deny rule there always wins over this hook regardless of what this
# hook returns.
#
# Everything else is blocked by default. It's allowed, scoped to the session
# cwd/scratchpad/tmp, only when the user has flipped the switch below — this
# hook never flips it itself:
#   - per session: `export CLAUDE_RM_SCOPED_ALLOW=1` before starting Claude
#   - per repo:    create a file at <repo-root>/.git/claude-rm-allowed
if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)rm\s'; then
  CWD=$(echo "$INPUT" | jq -r '.cwd // empty' 2>/dev/null) || CWD=""
  SCRATCHPAD_DIR=$(echo "$INPUT" | jq -r '.scratchpad_dir // empty' 2>/dev/null) || SCRATCHPAD_DIR=""

  RM_SWITCH_ON=""
  if [ "${CLAUDE_RM_SCOPED_ALLOW:-}" = "1" ]; then
    RM_SWITCH_ON=1
  elif [ -n "$CWD" ] && [ -f "$CWD/.git/claude-rm-allowed" ]; then
    RM_SWITCH_ON=1
  fi

  _rm_old_ifs=$IFS
  IFS='
'
  set -- $(echo "$COMMAND" | grep -oE '(^|[;&|])[[:space:]]*rm[[:space:]][^;&|]*')
  IFS=$_rm_old_ifs

  for _seg in "$@"; do
    [ -z "$_seg" ] && continue

    if echo "$_seg" | grep -qE -- '(^|[[:space:]])-[A-Za-z]*[rR][A-Za-z]*([[:space:]]|$)|--recursive' \
      && echo "$_seg" | grep -qE -- '(^|[[:space:]])-[A-Za-z]*f[A-Za-z]*([[:space:]]|$)|--force'; then
      deny "rm -rf (recursive + force) is blocked regardless of target"
    fi

    if [ -z "$RM_SWITCH_ON" ]; then
      deny "rm is blocked — set CLAUDE_RM_SCOPED_ALLOW=1 or create <repo-root>/.git/claude-rm-allowed to allow scoped deletes, or delete manually"
    fi

    for _tok in $_seg; do
      case "$_tok" in
        -*|rm) continue ;;
      esac
      case "$_tok" in
        /tmp|/tmp/*|/private/tmp|/private/tmp/*) continue ;;
      esac
      if [ -n "$SCRATCHPAD_DIR" ]; then
        case "$_tok" in
          "$SCRATCHPAD_DIR"|"$SCRATCHPAD_DIR"/*) continue ;;
        esac
      fi
      if [ -n "$CWD" ]; then
        case "$_tok" in
          "$CWD"|"$CWD"/*) continue ;;
        esac
      fi
      case "$_tok" in
        /*|\~*)
          deny "rm target '$_tok' is outside the working repo and outside tmp — delete it manually"
          ;;
      esac
    done
  done
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

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)git\s+checkout(\s|$)'; then
  deny "git checkout is blocked — discards uncommitted changes and switches branches"
fi

if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)git\s+switch(\s|$)'; then
  deny "git switch is blocked — switches branches and can discard uncommitted changes"
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

# if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)cargo\s+publish(\s|$)'; then
#   deny "cargo publish is blocked — publish manually"
# fi

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
# Broad filesystem scans — expensive, rarely correct
# ---------------------------------------------------------------------------
if echo "$COMMAND" | grep -qE '(^|\s|\;|\&|\|)find\s+(~|/Users/|/home/|/)(\s|/)'; then
  deny "broad find from home/root is blocked — search within the project or ask instead"
fi

# ---------------------------------------------------------------------------
# All clear
# ---------------------------------------------------------------------------
exit 0
