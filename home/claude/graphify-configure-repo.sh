#!/bin/bash
# graphify-configure-repo.sh
#
# Per-repo graphify setup. Run from inside the target repository, after
# graphify-install.sh has already been run once on this machine.
#
# A knowledge graph is built from one codebase's source, so unlike the CLI
# install and PreToolUse hook (which are global — see graphify-install.sh),
# this step is inherently per-repo: it has to be run separately for every
# project you want a graph for.

set -euo pipefail

info()    { echo "[*] $*"; }
success() { echo "[+] $*"; }
warn()    { echo "[!] $*"; }
error()   { echo "[x] $*"; exit 1; }

command -v graphify &>/dev/null || error "graphify not found — run graphify-install.sh first"

REPO_ROOT="$(pwd)"

# ── .mcp.json (project-scope MCP server entry, committed to repo) ───────────
MCP_FILE="$REPO_ROOT/.mcp.json"

python3 - "$MCP_FILE" << 'PEOF'
import json, sys

path = sys.argv[1]
try:
    with open(path) as f:
        cfg = json.load(f)
except (FileNotFoundError, json.JSONDecodeError):
    cfg = {}

cfg.setdefault("mcpServers", {})["graphify"] = {
    "command": "sh",
    "args": [
        "-c",
        "test -f graphify-out/graph.json && exec python3 -m graphify.serve graphify-out/graph.json "
        "|| echo '[graphify] Run /graphify . in Claude Code first' >&2"
    ],
    "type": "stdio"
}

with open(path, "w") as f:
    json.dump(cfg, f, indent=2)
PEOF
success ".mcp.json written with graphify entry — commit this file"

# ── post-commit hook (incremental graph rebuild) ─────────────────────────────
graphify hook install 2>/dev/null && success "graphify post-commit hook installed" \
  || warn "graphify hook install failed — run manually: graphify hook install"

# ── .gitignore ────────────────────────────────────────────────────────────────
GITIGNORE="$REPO_ROOT/.gitignore"
touch "$GITIGNORE"

if ! grep -qxF "graphify-out/" "$GITIGNORE" 2>/dev/null; then
  {
    echo "# graphify — generated knowledge graph (rebuild with: /graphify . in Claude Code)"
    echo "graphify-out/"
  } >> "$GITIGNORE"
  info "  Added graphify-out/ to .gitignore"
fi
success ".gitignore updated"

echo ""
echo "Done. Next steps:"
echo "  1. Start a new Claude Code session in this repo (loads the MCP server)"
echo "  2. Build the graph: /graphify ."
echo "  3. Verify the PreToolUse hook actually fires — see doc/graphify.md"
