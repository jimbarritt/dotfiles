#!/bin/sh
# Auto-resume the plan session timer on the user's next message.
#
# NOT REGISTERED. This hook is deliberately absent from settings.json — running
# it on every prompt was judged excessive for the amount of work it saves. The
# supported way to restart a paused timer is the /resume-plan skill.
#
# Kept because it is written and tested. To enable it, add an entry to the
# UserPromptSubmit block in home/claude/settings.json:
#
#   { "type": "command", "command": "sh ~/.claude/hooks/plan-timer-resume.sh", "timeout": 5 }
#
# Behaviour when enabled: /pause-plan stops the clock, and this restarts it as
# soon as the user says anything else, so the pause lasts exactly as long as the
# break. It is a no-op unless that project's timer is currently paused, costing
# one stat() on the overwhelming majority of prompts. The trade-off that put it
# on ice: it resumes on *any* next message, including one unrelated to the work.

input=$(cat)

cwd=$(echo "$input" | jq -r '.cwd // empty' 2>/dev/null)
[ -n "$cwd" ] || cwd=$PWD
[ -d "$cwd" ] || exit 0

project=$(basename "$(git -C "$cwd" rev-parse --show-toplevel 2>/dev/null || echo "$cwd")")
[ -n "$project" ] || exit 0

[ -f "${HOME}/.claude/plan-session-timers/${project}.paused" ] || exit 0

helper="${HOME}/.claude/skills/plan-format/plan-timer.sh"
[ -f "$helper" ] || exit 0

PLAN_TIMER_PROJECT="$project" sh "$helper" resume >/dev/null 2>&1 || exit 0

printf '{"hookSpecificOutput":{"hookEventName":"UserPromptSubmit","additionalContext":"[plan timer resumed automatically after a pause]"}}\n'
