#!/bin/sh
# Show a curated cheatsheet for the given command.
# Usage: key-help.sh [command]
# Falls back to default if no cheatsheet exists for the command.

HELP_DIR="${HOME}/.config/key-help"
CMD="${PANE_CMD:-${1:-default}}"
FILE="${HELP_DIR}/${CMD}"

if [ ! -f "$FILE" ]; then
  FILE="${HELP_DIR}/default"
fi

LESS= less +1 "$FILE"
