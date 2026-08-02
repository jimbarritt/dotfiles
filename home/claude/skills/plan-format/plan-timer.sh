#!/bin/sh
# Shared session-timer state for the plan skills.
#
# The timer measures wall-clock time spent on a project's plan within a
# session. `update-plan` writes it into the checkpoint as "Time spent".
# Work can be paused and resumed so breaks are not counted.
#
# State lives in ~/.claude/plan-session-timers/, three files per project:
#   {project}.start        unix ts the current running segment began (absent while paused)
#   {project}.accumulated  seconds banked from previously-ended segments (absent = 0)
#   {project}.paused       present while paused; line 1 = pause ts, line 2 = optional note
#
# Usage: plan-timer.sh <start|pause|resume|status|clear> [args]
#   start          begin timing if not already running or paused (idempotent)
#   pause [note]   bank the running segment and stop the clock
#   resume         restart the clock after a pause
#   status         print state, elapsed and any pause note
#   clear          remove all timer state for the project
#
# All commands operate on the project derived from the current directory
# (git root basename, else cwd basename). Override with PLAN_TIMER_PROJECT.

set -eu

DIR="${HOME}/.claude/plan-session-timers"

project() {
  if [ -n "${PLAN_TIMER_PROJECT:-}" ]; then
    printf '%s\n' "$PLAN_TIMER_PROJECT"
    return
  fi
  basename "$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
}

PROJECT=$(project)
START="${DIR}/${PROJECT}.start"
ACC="${DIR}/${PROJECT}.accumulated"
PAUSED="${DIR}/${PROJECT}.paused"

now() { date +%s; }

read_num() {
  # $1 = file; prints its integer contents, or 0 if absent/unparseable
  [ -f "$1" ] || { echo 0; return; }
  n=$(tr -dc '0-9' <"$1")
  [ -n "$n" ] || n=0
  echo "$n"
}

elapsed_secs() {
  banked=$(read_num "$ACC")
  if [ -f "$START" ]; then
    s=$(read_num "$START")
    if [ "$s" -gt 0 ]; then
      banked=$((banked + $(now) - s))
    fi
  fi
  echo "$banked"
}

fmt() {
  # $1 = seconds -> "Xh Ym"
  printf '%dh %dm' $(($1 / 3600)) $((($1 % 3600) / 60))
}

state() {
  if [ -f "$PAUSED" ]; then
    echo paused
  elif [ -f "$START" ]; then
    echo running
  else
    echo none
  fi
}

cmd_start() {
  mkdir -p "$DIR"
  # Already running or deliberately paused: leave the existing timer alone.
  # An earlier start time is more accurate than a fresh one.
  if [ ! -f "$START" ] && [ ! -f "$PAUSED" ]; then
    now >"$START"
  fi
  cmd_status
}

cmd_pause() {
  mkdir -p "$DIR"
  if [ -f "$PAUSED" ]; then
    cmd_status
    return
  fi
  if [ ! -f "$START" ]; then
    echo "state=none"
    echo "message=no timer running for ${PROJECT}; nothing to pause"
    return
  fi
  banked=$(read_num "$ACC")
  s=$(read_num "$START")
  if [ "$s" -gt 0 ]; then
    banked=$((banked + $(now) - s))
  fi
  echo "$banked" >"$ACC"
  rm -f "$START"
  now >"$PAUSED"
  if [ "$#" -gt 0 ] && [ -n "$*" ]; then
    printf '%s\n' "$*" >>"$PAUSED"
  fi
  cmd_status
}

cmd_resume() {
  mkdir -p "$DIR"
  if [ ! -f "$PAUSED" ]; then
    cmd_status
    return
  fi
  paused_at=$(sed -n 1p "$PAUSED" | tr -dc '0-9')
  [ -n "$paused_at" ] || paused_at=$(now)
  break_secs=$(($(now) - paused_at))
  rm -f "$PAUSED"
  now >"$START"
  cmd_status
  echo "break=$(fmt "$break_secs")"
}

cmd_status() {
  st=$(state)
  secs=$(elapsed_secs)
  echo "project=${PROJECT}"
  echo "state=${st}"
  echo "elapsed_secs=${secs}"
  echo "elapsed=$(fmt "$secs")"
  if [ "$st" = paused ]; then
    paused_at=$(sed -n 1p "$PAUSED" | tr -dc '0-9')
    [ -n "$paused_at" ] || paused_at=$(now)
    echo "paused_for=$(fmt $(($(now) - paused_at)))"
    note=$(sed -n 2p "$PAUSED" 2>/dev/null || true)
    if [ -n "$note" ]; then
      echo "note=${note}"
    fi
  fi
}

cmd_clear() {
  rm -f "$START" "$ACC" "$PAUSED"
  echo "project=${PROJECT}"
  echo "state=none"
}

case "${1:-status}" in
  start) cmd_start ;;
  pause) shift; cmd_pause "$@" ;;
  resume) cmd_resume ;;
  status) cmd_status ;;
  clear) cmd_clear ;;
  *)
    echo "usage: plan-timer.sh <start|pause|resume|status|clear> [note]" >&2
    exit 2
    ;;
esac
