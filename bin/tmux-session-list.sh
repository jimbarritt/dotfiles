#!/bin/sh
# Called from tmux status-left with the current session name as $1
current="$1"
tmux ls -F '#{session_name}' | while read -r name; do
  if [ "$name" = "$current" ]; then
    printf '● %s  ' "$name"
  else
    printf '○ %s  ' "$name"
  fi
done
