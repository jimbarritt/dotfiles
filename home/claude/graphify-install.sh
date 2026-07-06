#!/bin/bash
# graphify-install.sh
#
# One-time, once-per-machine setup for graphify (knowledge-graph tool for
# Claude Code). Installs the CLI and merges its PreToolUse hook into
# ~/.claude/settings.json — that file is global, so this applies to every
# Claude Code session on this machine, in every project, from here on.
#
# It does NOT build a graph for any particular repo — a graph is built from
# one codebase's source, so that step is inherently per-repo. See
# graphify-configure-repo.sh and doc/graphify.md "Configuring a repo" for that.
#
# Deliberately skips `graphify claude install`: that command bundles a CLAUDE.md
# directive together with the PreToolUse hook, and CLAUDE.md is managed by hand
# in this dotfiles setup. Instead the hook is merged into ~/.claude/settings.json
# directly, matching the CLAUDE.md-injection caveat documented in doc/graphify.md.

set -euo pipefail

info()    { echo "[*] $*"; }
success() { echo "[+] $*"; }
error()   { echo "[x] $*"; exit 1; }

# ── Prerequisites ────────────────────────────────────────────────────────────
command -v python3 &>/dev/null || error "python3 not found — install it first"
command -v uv &>/dev/null || error "uv not found — install it first (this machine uses uv, not pipx)"

# ── Install graphify ──────────────────────────────────────────────────────────
uv tool install graphifyy --force 2>/dev/null && success "graphify installed" \
  || error "graphify install failed — check manually: uv tool install graphifyy"

# ── PreToolUse hook (skip CLAUDE.md injection) ───────────────────────────────
info "Installing graphify PreToolUse hook into ~/.claude/settings.json..."

CLAUDE_USER_SETTINGS="$HOME/.claude/settings.json"
mkdir -p "$HOME/.claude"

python3 - "$CLAUDE_USER_SETTINGS" << 'PEOF'
import json, sys

path = sys.argv[1]
try:
    with open(path) as f:
        cfg = json.load(f)
except (FileNotFoundError, json.JSONDecodeError):
    cfg = {}

# Checks for graphify-out/GRAPH_REPORT.md in $CLAUDE_PROJECT_DIR and emits a
# reminder to stderr when present. Always exits 0 (non-blocking). Fires per
# project automatically — $CLAUDE_PROJECT_DIR varies per session.
HOOK_CMD = (
    'PROJ="${CLAUDE_PROJECT_DIR:-$PWD}"; '
    'if [ -f "$PROJ/graphify-out/GRAPH_REPORT.md" ]; then '
    'echo "graphify: Knowledge graph exists. Read graphify-out/GRAPH_REPORT.md '
    'for god nodes and community structure before searching raw files." >&2; '
    'fi; exit 0'
)

GRAPHIFY_HOOK = {
    "matcher": "Glob|Grep",
    "hooks": [{"type": "command", "command": HOOK_CMD}]
}

hooks = cfg.setdefault("hooks", {})
pre_tool_use = hooks.setdefault("PreToolUse", [])

def is_graphify_entry(entry):
    if not isinstance(entry, dict):
        return False
    for h in entry.get("hooks", []):
        if isinstance(h, dict) and "graphify:" in h.get("command", ""):
            return True
    return False

pre_tool_use[:] = [e for e in pre_tool_use if not is_graphify_entry(e)]
pre_tool_use.append(GRAPHIFY_HOOK)

with open(path, "w") as f:
    json.dump(cfg, f, indent=2)
PEOF
success "graphify PreToolUse hook installed in $CLAUDE_USER_SETTINGS (applies to all projects)"

# NOTE: the hook matcher is "Glob|Grep" by tool name. On Claude Code versions
# that route search through Bash instead of dedicated Grep/Glob tools, this
# hook silently never fires — verify it actually fires before relying on it.
# See the hook-compatibility caveat in doc/graphify.md.

echo ""
echo "Done. graphify is now installed and its hook is wired in globally."
echo "To actually use it on a specific repo, run graphify-configure-repo.sh"
echo "from inside that repo, then build the graph with: /graphify ."
