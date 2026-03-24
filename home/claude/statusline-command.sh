#!/bin/sh
# Claude Code status line — mirrors the green-tinted zsh theme prompt
# Input: JSON from Claude Code via stdin

input=$(cat)

cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd // ""')
model=$(echo "$input" | jq -r '.model.display_name // ""')
ctx_remaining=$(echo "$input" | jq -r '.context_window.remaining_percentage // empty')

# Shorten home directory to ~
home="$HOME"
short_cwd="$(basename "$cwd")"

# Git branch (skip optional locks, timeout to prevent hangs)
branch=""
if command -v timeout >/dev/null 2>&1; then
  _git_timeout="timeout 2"
else
  _git_timeout=""
fi
if $_git_timeout git -C "$cwd" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  branch=$($_git_timeout git -C "$cwd" \
    -c core.fsmonitor=false \
    -c gc.auto=0 \
    symbolic-ref --short HEAD 2>/dev/null \
    || $_git_timeout git -C "$cwd" \
    -c core.fsmonitor=false \
    rev-parse --short HEAD 2>/dev/null)
fi

# Build output: green path, cyan branch, dim model, context %
green='\033[32m'
cyan='\033[36m'
dim='\033[2m'
reset='\033[0m'

output=""
output="${output}$(printf "${green}%s${reset}" "$short_cwd")"

if [ -n "$branch" ]; then
  output="${output} $(printf "${cyan}[%s]${reset}" "$branch")"
fi

output="${output} - $(printf "${dim}%s${reset}" "$model")"

if [ -n "$ctx_remaining" ]; then
  ctx_int=$(printf '%.0f' "$ctx_remaining")
  output="${output} - $(printf "${dim}ctx:%s%%${reset}" "$ctx_int")"
fi

printf "%b\n" "$output"
exit 0
