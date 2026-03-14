#!/bin/sh
# Called from tmux status-left with the current session name as $1
current="$1"
tmux ls -F '#{session_name}' | while read -r name; do
  if [ "$name" = "$current" ]; then
    printf '#[bg=#3a7a3a,fg=#000000] %s #[bg=#122912,fg=#4a7c4a] ' "$name"
  else
    printf ' %s  ' "$name"
  fi
done
