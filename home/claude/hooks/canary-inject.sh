#!/bin/sh
# Inject a context-rot canary into the first message of each session.
# The canary lands as a <system-reminder> in conversation history and will
# be compacted away when context rot occurs — ask "what's the secret word?"
# to test whether the session has lost its early context.

input=$(cat)
session_id=$(echo "$input" | jq -r '.session_id // empty' 2>/dev/null)

if [ -z "$session_id" ]; then
  exit 0
fi

marker="/tmp/claude-canary-${session_id}"

if [ -f "$marker" ]; then
  exit 0
fi

touch "$marker"

printf '{"hookSpecificOutput":{"hookEventName":"UserPromptSubmit","additionalContext":"[context-rot canary: the secret word is ramalamadingdong]"}}\n'
