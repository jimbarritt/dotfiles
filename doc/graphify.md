# Graphify — Knowledge Graph for Large Codebases

## What it is

Graphify is an open-source Python CLI tool that builds a **queryable knowledge graph** of your codebase, which an AI coding assistant (Claude Code, Cursor, Codex, etc.) consults instead of repeatedly reading raw source files into context. It is not AST compression or symbol diffing — it is a pre-built index of your repo's structure and semantics that the LLM navigates via small subgraph queries.

- Package: `graphifyy` (note double-y) — install via `uv tool install graphifyy` or `pipx install graphifyy`
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

Leiden community detection (via NetworkX) clusters related concepts. Output:
- `graph.json` — the queryable graph
- `graph.html` — interactive visualisation
- `GRAPH_REPORT.md` — orientation doc: "god nodes", surprising connections, suggested queries

Supported graph backends: NetworkX (default, file-based), Neo4j, FalkorDB.

## Language support

Tree-sitter grammars: Python, JS, TS, Go, Rust, Java, C, C++, C#, Ruby, Kotlin, Scala, PHP, Swift, Lua, Zig, PowerShell, Elixir, Objective-C, Julia, SQL, shell, Terraform, Salesforce Apex, plus manifests (`pyproject.toml`, `go.mod`, `pom.xml`). Also: Markdown, PDF, Office/Google Workspace docs, images, audio/video.

## Claude Code integration

```bash
uv tool install graphifyy
graphify install                 # registers /graphify skill
graphify claude install          # writes CLAUDE.md directive + PreToolUse hook
/graphify .                      # build graph for current repo
```

The `PreToolUse` hook nudges Claude to query the graph before file-search tool calls, so it navigates to the right files via the graph rather than scanning. The `CLAUDE.md` directive reinforces this.

Official integration guide: https://graphify.net/graphify-claude-code-integration.html

## Token savings (honest numbers)

The headline **71x** figure is an upper-bound benchmark on a large monorepo (~123k tokens naive vs ~1,700 with Graphify per query). More realistic estimates:

| Codebase size | Typical savings |
|---|---|
| < 100 files | Not worth it (overhead exceeds savings) |
| 100–500 files | ~6x–15x |
| 500+ files | ~30x–49x |
| Large monorepos | Up to ~71–79x |

## Trade-offs and limitations

- **Staleness:** the graph does not auto-update. Silently degrades as code changes unless you wire a rebuild into a git pre-commit hook.
- **Small repos:** under ~30–100 files the overhead exceeds the savings. This dotfiles repo is **not a good candidate**.
- **Claude Code hook compatibility:** Claude Code ~v2.1.117 (May 2026) removed the dedicated `Grep`/`Glob` tools. The `PreToolUse` redirect hook may silently become a no-op on current versions — verify before relying on it.
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
- [github.com/safishamsi/graphify](https://github.com/safishamsi/graphify) — canonical repo
- [roborhythms.com/graphify-review](https://www.roborhythms.com/graphify-review/) — best critical review (caveats)
- [stevescargall.com — 79x benchmark](https://stevescargall.com/blog/2026/05/graphify--memmachine-79-token-reduction-zero-vector-database/)
- [Karpathy LLM Wiki gist](https://gist.github.com/karpathy/442a6bf555914893e9891c11519de94f) — origin inspiration
- [analyticsvidhya.com — Graphify guide](https://www.analyticsvidhya.com/blog/2026/04/graphify-guide/)
