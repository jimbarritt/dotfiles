#!/usr/bin/env bash
set -uo pipefail

TOP=15
CCN=15
LENGTH=60
TARGET=.

usage() {
  cat <<USAGE
Usage: code-metrics.sh [--top N] [--ccn N] [--length N] [TARGET]

  --top N      how many items to list per section (default ${TOP})
  --ccn N      cyclomatic complexity threshold (default ${CCN})
  --length N   function length threshold in lines (default ${LENGTH})
  TARGET       directory to analyse (default current directory)
USAGE
}

while [ $# -gt 0 ]; do
  case "$1" in
    --top) TOP="$2"; shift 2 ;;
    --ccn) CCN="$2"; shift 2 ;;
    --length) LENGTH="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    -*) echo "unknown option: $1" >&2; usage >&2; exit 2 ;;
    *) TARGET="$1"; shift ;;
  esac
done

if [ ! -d "$TARGET" ]; then
  echo "not a directory: $TARGET" >&2
  exit 2
fi

resolve() {
  if command -v "$1" >/dev/null 2>&1; then
    command -v "$1"
  elif [ -x "$HOME/.local/bin/$1" ]; then
    echo "$HOME/.local/bin/$1"
  fi
}

SCC=$(resolve scc)
LIZARD=$(resolve lizard)

missing=0
if [ -z "$SCC" ]; then
  echo "MISSING: scc — install with 'brew install scc'" >&2
  missing=1
fi
if [ -z "$LIZARD" ]; then
  echo "MISSING: lizard — install with 'uv tool install lizard'" >&2
  echo "         (Homebrew's 'lizard' formula is an unrelated compression tool, not this)" >&2
  missing=1
fi
if [ -n "$LIZARD" ] && ! "$LIZARD" --help 2>&1 | grep -qi cyclomatic; then
  echo "WRONG TOOL: '$LIZARD' is not the complexity analyser." >&2
  echo "            Run 'brew uninstall lizard' then 'uv tool install lizard'." >&2
  LIZARD=
  missing=1
fi
if [ "$missing" -eq 1 ] && [ -z "$SCC" ] && [ -z "$LIZARD" ]; then
  exit 1
fi

EXCLUDES=(
  -x "*/node_modules/*" -x "*/target/*" -x "*/vendor/*" -x "*/.build/*"
  -x "*/build/*" -x "*/dist/*" -x "*/Pods/*" -x "*/.venv/*"
  -x "*/venv/*" -x "*/__pycache__/*" -x "*/.git/*"
)

ABS=$(cd "$TARGET" && pwd)
cd "$ABS" || exit 1

echo "# Code metrics — $(basename "$ABS")"
echo
echo "Target: \`$ABS\`  ·  thresholds: CCN > ${CCN}, length > ${LENGTH} lines  ·  top ${TOP} per section"
echo

if [ -n "$SCC" ]; then
  echo "## Shape"
  echo
  echo '```'
  "$SCC" --no-cocomo . 2>/dev/null
  echo '```'
  echo
  echo "## Largest files by code lines"
  echo
  printf '%8s  %s\n' "CODE" "FILE"
  "$SCC" --by-file --format csv . 2>/dev/null \
    | tail -n +2 \
    | awk -F, 'NF>=8 { print $5 "\t" $2 }' \
    | sort -rn \
    | head -n "$TOP" \
    | awk -F'\t' '{ printf "%8s  %s\n", $1, $2 }'
  echo
fi

if [ -n "$LIZARD" ]; then
  CSV=$(mktemp)
  "$LIZARD" "${EXCLUDES[@]}" -C "$CCN" -L "$LENGTH" --csv . 2>/dev/null > "$CSV"
  total=$(wc -l < "$CSV" | tr -d ' ')

  echo "## Functions over complexity threshold (CCN > ${CCN})"
  echo
  over=$(awk -F, -v t="$CCN" 'NF>=6 && $2+0 > t { print $2 "\t" $1 "\t" $5 "\t" $6 }' "$CSV" | sort -rn)
  if [ -z "$over" ]; then
    echo "None."
  else
    printf '%5s %6s %7s  %s\n' "CCN" "NLOC" "LINES" "FUNCTION"
    echo "$over" | head -n "$TOP" \
      | awk -F'\t' '{ gsub(/"/,"",$4); gsub(/@\.\//,"@",$4); printf "%5s %6s %7s  %s\n", $1, $2, $3, $4 }'
    count=$(echo "$over" | wc -l | tr -d ' ')
    echo
    echo "${count} of ${total} functions exceed CCN ${CCN}."
  fi
  echo

  echo "## Functions over length threshold (${LENGTH} lines)"
  echo
  long=$(awk -F, -v t="$LENGTH" 'NF>=6 && $5+0 > t { print $5 "\t" $1 "\t" $2 "\t" $6 }' "$CSV" | sort -rn)
  if [ -z "$long" ]; then
    echo "None."
  else
    printf '%6s %6s %5s  %s\n' "LINES" "NLOC" "CCN" "FUNCTION"
    echo "$long" | head -n "$TOP" \
      | awk -F'\t' '{ gsub(/"/,"",$4); gsub(/@\.\//,"@",$4); printf "%6s %6s %5s  %s\n", $1, $2, $3, $4 }'
    count=$(echo "$long" | wc -l | tr -d ' ')
    echo
    echo "${count} of ${total} functions exceed ${LENGTH} lines."
  fi
  echo

  echo "## Functions with the most parameters"
  echo
  printf '%6s  %s\n' "PARAMS" "FUNCTION"
  awk -F, 'NF>=6 && $4+0 >= 4 { print $4 "\t" $6 }' "$CSV" | sort -rn | head -n "$TOP" \
    | awk -F'\t' '{ gsub(/"/,"",$2); gsub(/@\.\//,"@",$2); printf "%6s  %s\n", $1, $2 }'
  echo

  rm -f "$CSV"
fi
