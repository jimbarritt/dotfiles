#!/bin/sh
# Claude Code status line — mirrors the green-tinted zsh theme prompt
# Input: JSON from Claude Code via stdin

input=$(cat)

cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd // ""')
model=$(echo "$input" | jq -r '.model.display_name // .model.id // ""')
version=$(echo "$input" | jq -r '.version // empty')
ctx_tokens=$(echo "$input" | jq -r '(.context_window.total_input_tokens + .context_window.total_output_tokens) // empty')
cost_usd=$(echo "$input" | jq -r '.cost.total_cost_usd // 0')
rate_5h=$(echo "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty')
rate_7d=$(echo "$input" | jq -r '.rate_limits.seven_day.used_percentage // empty')
reset_5h=$(echo "$input" | jq -r '.rate_limits.five_hour.reset_at // .rate_limits.five_hour.resets_at // empty')
reset_7d=$(echo "$input" | jq -r '.rate_limits.seven_day.reset_at // .rate_limits.seven_day.resets_at // empty')
on_subscription=$(echo "$input" | jq 'has("rate_limits")')

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

# Build output: green path, cyan branch, dim model, context tokens
# `dim` uses bright-black (palette 8 / secondary-text slot) rather than the
# ANSI dim attribute (\033[2m), which washes out to near-invisible on the light
# theme's white background. Palette 8 is a readable mid-tone in both themes.
green='\033[32m'
cyan='\033[36m'
dim='\033[90m'
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

if [ -n "$ctx_tokens" ]; then
  ctx_k=$((ctx_tokens / 1000))
  if [ "$ctx_k" -ge 150 ]; then
    output="${output} - $(printf "${bold}!!ctx:%sk!!${reset}" "$ctx_k")"
  elif [ "$ctx_k" -ge 100 ]; then
    output="${output} - $(printf "${bold}ctx:%sk${reset}" "$ctx_k")"
  else
    output="${output} - $(printf "${dim}ctx:%sk${reset}" "$ctx_k")"
  fi
fi

if [ "$on_subscription" = "true" ]; then
  # Subscription mode (Pro/Max): show rate limits; show cost only when at/over a limit
  rate_5h_int=0
  rate_7d_int=0
  rate_limits=""

  if [ -n "$rate_5h" ]; then
    rate_5h_int=$(printf '%.0f' "$rate_5h")
    rate_limits="${rate_limits}5h:"
    if [ "$rate_5h_int" -gt 80 ]; then
      rate_limits="${rate_limits}${bold}${rate_5h_int}%${reset}"
    else
      rate_limits="${rate_limits}${dim}${rate_5h_int}%${reset}"
    fi
    if [ -n "$reset_5h" ] && [ "$reset_5h" != "null" ]; then
      reset_hhmm=$(date -r "$reset_5h" '+%H:%M' 2>/dev/null)
      if [ -n "$reset_hhmm" ]; then
        rate_limits="${rate_limits}${dim}@${reset_hhmm}${reset}"
      fi
    fi
    rate_limits="${rate_limits} "
  fi

  if [ -n "$rate_7d" ]; then
    rate_7d_int=$(printf '%.0f' "$rate_7d")
    rate_limits="${rate_limits}7d:"
    if [ "$rate_7d_int" -gt 80 ]; then
      rate_limits="${rate_limits}${bold}${rate_7d_int}%${reset}"
    else
      rate_limits="${rate_limits}${dim}${rate_7d_int}%${reset}"
    fi
    if [ -n "$reset_7d" ] && [ "$reset_7d" != "null" ]; then
      reset_ddmm=$(date -r "$reset_7d" '+%d/%m %H:%M' 2>/dev/null)
      if [ -n "$reset_ddmm" ]; then
        rate_limits="${rate_limits}${dim}@${reset_ddmm}${reset}"
      fi
    fi
  fi

  if [ -n "$rate_limits" ]; then
    output="${output} - ${rate_limits}"
  fi

  # Show cost only when in extra usage (at or over a rate limit)
  if [ "$rate_5h_int" -ge 100 ] || [ "$rate_7d_int" -ge 100 ]; then
    cost_gbp=$(echo "$cost_usd * 0.74" | bc -l | xargs printf '%.2f')
    output="${output} - $(printf "${bold}extra: £%s${reset}" "$cost_gbp")"
  fi
else
  # API mode: always show cost, no rate limits
  cost_gbp=$(echo "$cost_usd * 0.74" | bc -l | xargs printf '%.2f')
  output="${output} - $(printf "${dim}£%s${reset}" "$cost_gbp")"
fi

# Monthly spend from trakr (fast DB read — no reconciliation)
trakr_json=""
if command -v trakr >/dev/null 2>&1; then
  trakr_json=$(trakr spend --json 2>/dev/null)
fi
if [ -n "$trakr_json" ]; then
  trakr_spent=$(echo "$trakr_json" | jq -r '.spent_usd')
  trakr_budget=$(echo "$trakr_json" | jq -r '.budget_usd')
  trakr_pct=$(echo "$trakr_json" | jq -r '.pct')
  trakr_pct_int=$(printf '%.0f' "$trakr_pct")
  trakr_str=$(printf '$%.2f/$%.2f' "$trakr_spent" "$trakr_budget")
  if [ "$trakr_pct_int" -ge 100 ]; then
    output="${output} - $(printf "${bold}\033[31m%s${reset}" "$trakr_str")"
  elif [ "$trakr_pct_int" -ge 80 ]; then
    output="${output} - $(printf "${bold}${yellow}%s${reset}" "$trakr_str")"
  else
    output="${output} - $(printf "${dim}%s${reset}" "$trakr_str")"
  fi
fi

printf "%b\n" "$output"
exit 0
