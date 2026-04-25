#!/bin/sh
# Claude Code status line — mirrors the green-tinted zsh theme prompt
# Input: JSON from Claude Code via stdin

input=$(cat)

cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd // ""')
model=$(echo "$input" | jq -r '.model.display_name // .model.id // ""')
version=$(echo "$input" | jq -r '.version // empty')
ctx_remaining=$(echo "$input" | jq -r '.context_window.remaining_percentage // empty')
input_tokens=$(echo "$input" | jq -r '.context_window.total_input_tokens // 0')
output_tokens=$(echo "$input" | jq -r '.context_window.total_output_tokens // 0')
cost_usd=$(echo "$input" | jq -r '.cost.total_cost_usd // 0')
rate_5h=$(echo "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty')
rate_7d=$(echo "$input" | jq -r '.rate_limits.seven_day.used_percentage // empty')

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

if [ -n "$version" ]; then
  model_str="${model} - ${version}"
else
  model_str="$model"
fi
output="${output} - $(printf "${dim}%s${reset}" "$model_str")"

bold='\033[1m'
yellow='\033[33m'

if [ -n "$ctx_remaining" ]; then
  ctx_remaining_int=$(printf '%.0f' "$ctx_remaining")
  ctx_used=$((100 - ctx_remaining_int))
  if [ "$ctx_used" -gt 50 ]; then
    output="${output} - $(printf "${bold}ctx:%s%% !!${reset}" "$ctx_used")"
  else
    output="${output} - $(printf "${dim}ctx:%s%%${reset}" "$ctx_used")"
  fi
fi

# Session tokens and cost
total_tokens=$((input_tokens + output_tokens))
if [ "$total_tokens" -gt 0 ]; then
  cost_gbp=$(echo "$cost_usd * 0.74" | bc -l | xargs printf '%.2f')
  output="${output} - $(printf "${dim}%sk tokens / £%s${reset}" "$((total_tokens / 1000))" "$cost_gbp")"
fi

# Rate limits (Pro: 44k/5h, ~320k/7d)
if [ -n "$rate_5h" ] || [ -n "$rate_7d" ]; then
  rate_limits=""
  # Pro plan token caps
  cap_5h=44000
  cap_7d=320000

  if [ -n "$rate_5h" ]; then
    rate_5h_int=$(printf '%.0f' "$rate_5h")
    tokens_5h=$((cap_5h * rate_5h_int / 100))
    tokens_5h_k=$(( (tokens_5h + 500) / 1000 ))  # round to nearest k
    if [ "$rate_5h_int" -gt 80 ]; then
      rate_limits="${rate_limits}5h:${bold}${rate_5h_int}% (${tokens_5h_k}k)${reset} "
    else
      rate_limits="${rate_limits}5h:${dim}${rate_5h_int}% (${tokens_5h_k}k)${reset} "
    fi
  fi
  if [ -n "$rate_7d" ]; then
    rate_7d_int=$(printf '%.0f' "$rate_7d")
    tokens_7d=$((cap_7d * rate_7d_int / 100))
    tokens_7d_k=$(( (tokens_7d + 500) / 1000 ))  # round to nearest k
    if [ "$rate_7d_int" -gt 80 ]; then
      rate_limits="${rate_limits}7d:${bold}${rate_7d_int}% (${tokens_7d_k}k)${reset}"
    else
      rate_limits="${rate_limits}7d:${dim}${rate_7d_int}% (${tokens_7d_k}k)${reset}"
    fi
  fi
  if [ -n "$rate_limits" ]; then
    output="${output} - ${rate_limits}"
  fi
fi

printf "%b\n" "$output"
exit 0
