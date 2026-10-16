# M-DOT-CLAUDE-04: task notes

Imported from `doc/planning/plan.md` in the dotfiles repo, heading "Delta: Code Metrics for Agents". The bullets are as written in the plan.

Command-line tooling that gives a coding agent structural feedback about an unfamiliar
codebase — size per file, per-function complexity, duplication — as a ranked report the
agent reads and acts on. Research: `doc/code-metrics-for-agents.md`.

### T-01: Survey the tooling
- ✓ DONE — `scc` (brew) verified: per-file JSON with `Code`/`Complexity`/`Location`. Its complexity column is a per-file branch-keyword count, not McCabe
- ✓ DONE — Homebrew's `lizard` formula identified as an unrelated LZ4 compression tool, not the complexity analyser
- ✓ DONE — Real `lizard` installed via `uv tool install lizard`; verified per-function CCN, thresholded Warnings block, `--csv`, and non-zero exit on breach
- ✓ DONE — `codelimit` (brew) rejected: unit length only, no CCN, no working Swift support
- TODO — Evaluate `jscpd` for the duplication layer
- ✓ DONE — `doc/code-metrics-for-agents.md` written and indexed

### T-02: Decide packaging
- ✓ DONE — Decided: no own formula. The skill documents `brew install scc` and `uv tool install lizard` as prerequisites, and a wrapper script checks for them and prints install instructions when missing

### T-03: Build the skill
- ✓ DONE — `home/claude/skills/code-metrics/` — `SKILL.md` plus `code-metrics.sh`. Picked up automatically by `do.sh link-claude`
- ✓ DONE — Wrapper resolves `lizard` via `$HOME/.local/bin` fallback, because `uv tool install` puts it somewhere absent from a non-interactive PATH — without this the prerequisite check reports it missing immediately after a successful install
- ✓ DONE — Wrapper distinguishes the real lizard from Homebrew's compression tool of the same name by grepping `--help` for `cyclomatic`. Version strings do not discriminate: the compression tool prints a longer banner and the analyser prints a bare semver
- ✓ DONE — lizard's non-zero exit on threshold breach is a finding, not an error; `set -e` is deliberately not used
- ✓ DONE — Sections: shape, largest files, functions over CCN, functions over length, most parameters. Each ranked worst-first, capped by `--top`, with the threshold stated and an "N of M" count
- ✓ DONE — Report goes to stdout only. Never written into the target tree, which is usually not the repo being worked in
- ✓ DONE — No `-l` language mapping. lizard defaults to every language it knows; the translation layer was dropped as unnecessary
- ✓ DONE — Explicit `-x` excludes for `node_modules`, `target`, `vendor`, `.build`, `build`, `dist`, `Pods`, `.venv`, `__pycache__`. scc respects `.gitignore`, lizard does not
- ✓ DONE — Pure awk/sort over `scc --format csv` and `lizard --csv`; no jq or python dependency
- ✓ DONE — Verified on `~/projects/ledgr` (Rust) and `~/projects/tilr` (Swift), plus the degraded path with an empty PATH
- ✓ DONE — Skill symlinked into `~/.claude/skills/code-metrics`; verified registered and smoke-tested end to end against `~/projects/quiq`
