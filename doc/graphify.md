# Graphify — Knowledge Graph for Large Codebases

## Installation (once per machine)

Install the CLI once per machine:

```bash
uv tool install graphifyy
```

or, with pipx:

```bash
pipx install graphifyy
```

This only installs the CLI — it doesn't configure anything for Claude Code. That's done per repo — see "Configuring a repo" below.

## Configuring a repo

Run inside the repo you want to configure:

```bash
graphify claude install
```

This writes a `## graphify` section into that repo's `CLAUDE.md` and installs the `PreToolUse` hook into its `.claude/settings.json` (project-scoped, not global). **Review the diff before committing** — it's a normal file edit, treat it like any other change.

Add `graphify-out/` to `.gitignore`, then build the initial graph: `/graphify .` — produces `graphify-out/graph.json` (queryable graph), `graphify-out/graph.html` (interactive visualisation), and `graphify-out/GRAPH_REPORT.md` (orientation doc).

Official integration guide: https://graphify.net/graphify-claude-code-integration.html

## Notes for Claude

- **When to suggest it:** projects with 300+ files, high cross-file interdependency, and frequent "where does X happen" exploratory work. Not worth it under ~100–150 files — grep/glob is cheaper there.
- **If a project already has a graph** (`graphify-out/graph.json` / `graphify-out/GRAPH_REPORT.md`): read `GRAPH_REPORT.md` first for orientation, then query the graph before falling back to broad grep/glob for navigation questions.
- **Staleness:** the graph does not auto-update. Unless `graphify hook install` (post-commit incremental rebuild) is wired up, treat it with suspicion on actively-changing branches — verify findings against the actual code rather than trusting the graph blindly.
- **Hook history:** older Claude Code versions removed the dedicated `Grep`/`Glob` tools and routed search through `Bash`, which broke early versions of graphify's `PreToolUse` hook (it only matched `Glob|Grep`). Confirmed fixed in the current release: `graphify/__main__.py` (tag `v8`) now installs two hooks, one matching `Bash` and one matching `Read|Glob`.
- **Installing:** `uv tool install graphifyy` (or `pipx install graphifyy`) once per machine, then `graphify claude install` per repo — writes the CLAUDE.md section and hook. Build the graph with `/graphify .`.

## What it is

Graphify is an open-source Python CLI tool that builds a **queryable knowledge graph** of your codebase, which an AI coding assistant (Claude Code, Cursor, Codex, etc.) consults instead of repeatedly reading raw source files into context. It is not AST compression or symbol diffing — it is a pre-built index of your repo's structure and semantics that the LLM navigates via small subgraph queries.

- Package: `graphifyy` (note double-y) — install via `uv tool install graphifyy`
- Repo: https://github.com/safishamsi/graphify
- Site: https://graphify.net
- Author: Safi Shamsi (Y Combinator S26)
- Licence: MIT

**Origin:** Andrej Karpathy described a "raw/ folder → LLM-compiled wiki of .md files" workflow on 2 April 2026 and said "there is room here for an incredible new product." Graphify was built as a direct response within ~48 hours.

## How it works

Three-pass architecture:

1. **Deterministic AST extraction (local, no LLM)** — tree-sitter parses your code to extract functions, classes, imports, call graphs, docstrings. Source never leaves the machine.
2. **Local transcription** — `faster-whisper` transcribes any audio/video locally.
3. **Semantic enrichment (LLM)** — only docs, comments, and diagrams (not raw source) are sent to the LLM to extract semantic relationships. Relationships tagged `EXTRACTED` / `INFERRED` / `AMBIGUOUS`.

Leiden community detection (via NetworkX) clusters related concepts. Output lands in a `graphify-out/` directory (not the repo root):
- `graphify-out/graph.json` — the queryable graph
- `graphify-out/graph.html` — interactive visualisation
- `graphify-out/GRAPH_REPORT.md` — orientation doc: "god nodes", surprising connections, suggested queries
- `graphify-out/cache/` — SHA256-based incremental cache (only changed files get reprocessed on rebuild)
- Optional, flag-dependent: `graphify-out/obsidian/` (Obsidian vault), `graphify-out/wiki/` (Wikipedia-style navigable articles, via `--wiki`), `graphify-out/converted/` (Markdown sidecars)

Supported graph backends: NetworkX (default, file-based), Neo4j, FalkorDB.

`graphify-out/` is generated — add it to `.gitignore`, don't commit it.

## Language support

Tree-sitter grammars: Python, JS, TS, Go, Rust, Java, C, C++, C#, Ruby, Kotlin, Scala, PHP, Swift, Lua, Zig, PowerShell, Elixir, Objective-C, Julia, SQL, shell, Terraform, Salesforce Apex, plus manifests (`pyproject.toml`, `go.mod`, `pom.xml`). Also: Markdown, PDF, Office/Google Workspace docs, images, audio/video.

## Token savings (honest numbers)

The headline **71x** figure is an upper-bound benchmark on a large monorepo (~123k tokens naive vs ~1,700 with Graphify per query). More realistic estimates:

| Codebase size | Typical savings |
|---|---|
| < 100 files | Not worth it (overhead exceeds savings) |
| 100–500 files | ~6x–15x |
| 500+ files | ~30x–49x |
| Large monorepos | Up to ~71–79x |

## Trade-offs and limitations

- **Staleness:** the graph does not auto-update. Silently degrades as code changes unless you run `graphify hook install` to wire an incremental rebuild into `.git/hooks/post-commit`.
- **Small repos:** under ~30–100 files the overhead exceeds the savings. This dotfiles repo is **not a good candidate**.
- **Claude Code hook compatibility (historical):** Claude Code ~v2.1.117 (May 2026) removed the dedicated `Grep`/`Glob` tools and routed search through `Bash` instead, silently breaking hooks that matched on tool names `Glob|Grep` — documented by [roborhythms.com's review](https://www.roborhythms.com/graphify-review/), not by graphify itself at the time. **Confirmed fixed in the current release**: `graphify/__main__.py` in the source repo (tag `v8`) now installs two `PreToolUse` hooks — one matching `Bash` (inspects the command string for `grep`/`rg`/`find`/`fd`/`ack`/`ag`) and one matching `Read|Glob` (nudges on raw file reads outside `graphify-out/`). Still worth a quick check after installing that the hook fires as expected.
- **Graph noise:** legacy Java/common-import-heavy codebases produce noisy Leiden clusters (files grouped by shared `Logger`, not semantic relationship).
- **Token trap:** the architecture-overview query can return up to ~131k chars (~33k tokens) if used carelessly.
- **Visualisation limits:** graphs over ~5,000 nodes are hard to render in-browser.
- **Star count skepticism:** Reddit notes "shady growth tactics around the GitHub star count" — treat the 69k stars / YC framing with caution.

## When to adopt

**Worth it:** 500+ file project, heavy Claude Code usage, willing to automate graph rebuilds via git hook, and able to verify `PreToolUse` hook fires on your current Claude Code version.

**Not worth it:** small repos, projects with rapidly changing structure (staleness problem), or if you haven't verified hook compatibility with your Claude Code version.

## Fit assessment

The token savings are driven by two things: **codebase size** and **cross-file navigation frequency**. The more Claude needs to discover where things live before doing anything, the more Graphify pays off.

Rough break-even point is around **100–150 files with moderate interdependencies** — below that, a targeted grep or glob is cheaper than querying a graph. The savings start becoming meaningful around **300–500 files**, especially if the codebase has tangled imports or you're regularly doing architectural-scale work (refactors, cross-cutting changes) rather than focused feature additions.

File count is a proxy though. What actually matters:

- **High interdependency** — lots of cross-file imports, shared utilities, layered abstractions. Graphify's Leiden clustering pays off here.
- **Exploratory work** — "where does X happen?", "what calls Y?" queries that would otherwise require repeated grep/glob tool calls eating context.
- **Stability** — rapidly changing codebases suffer from graph staleness. A graph built on Monday is quietly wrong by Friday unless you automate rebuilds.

A typical startup-scale app (~200–500 files, a few layers of abstraction) sits in the 6–15x savings range — real but not dramatic. The 71x headline requires a monorepo with hundreds of thousands of tokens of context if read naively.

**Verdict:** worth evaluating when a project hits ~300+ files and you notice Claude spending significant context budget on discovery before doing actual work. Not the right tool for focused single-feature work or small repos.

## Sources

- [graphify.net](https://graphify.net/) — official site and integration docs
- [graphify.net — CLI command reference](https://graphify.net/graphify-cli-commands.html) — confirms `graphify-out/` output structure
- [github.com/safishamsi/graphify](https://github.com/safishamsi/graphify) — canonical repo
- [github.com/safishamsi/graphify — `graphify/__main__.py`, tag `v8`](https://github.com/safishamsi/graphify/blob/v8/graphify/__main__.py) — read directly to confirm the current dual `Bash` + `Read|Glob` hook matchers, `graphify claude install`'s exact CLAUDE.md write, and the skill-registration path; supersedes the roborhythms.com-documented hook gap below
- [roborhythms.com/graphify-review](https://www.roborhythms.com/graphify-review/) — best critical review (caveats, now partly superseded — see above)
- [stevescargall.com — 79x benchmark](https://stevescargall.com/blog/2026/05/graphify--memmachine-79-token-reduction-zero-vector-database/)
- [Karpathy LLM Wiki gist](https://gist.github.com/karpathy/442a6bf555914893e9891c11519de94f) — origin inspiration
- [analyticsvidhya.com — Graphify guide](https://www.analyticsvidhya.com/blog/2026/04/graphify-guide/)
