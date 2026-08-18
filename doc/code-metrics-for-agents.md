# Code metrics tooling for agents

Survey of command-line tools that can give a coding agent structural feedback about
an unfamiliar codebase — size per file, per-function complexity, duplication — as a
ranked, machine-readable report the agent reads and acts on.

Requirements: multi-language, scriptable, easily installed, ranked output rather than
a full dump.

## Findings

Everything below was run against `~/projects/tilr` (Swift, ~4k NLOC) on 2026-08-18.

### scc — size and shape

`brew install scc`. Go binary, very fast, handles every file type including markdown,
JSON and YAML.

```
scc --by-file --format json2 <dir>
```

Emits per-file `Code`, `Lines`, `Blanks`, `Comments`, `Complexity`, `Location`.

Its `Complexity` column is a **count of branch keywords per file**, not McCabe and not
per function. It is a usable ranking proxy within one language, and not comparable
across languages. Documented as an estimate.

Role: the size and hotspot layer. Cheap enough to run over a whole repo unconditionally.

### lizard — real per-function cyclomatic complexity

**Not the `lizard` in Homebrew core.** That formula is an unrelated LZ4-family
compression tool with the same name; installing it and running it against source
produces `Incorrect parameters`. The analyser is the Python package.

```
uv tool install lizard
```

Covers cpp, java, csharp, javascript, typescript, python, objectivec, ttcn, ruby, php,
swift, scala, GDScript, go, lua, rust and more.

```
lizard -l swift <dir>
```

Default output ends in a Warnings block — already thresholded and ranked, which is
exactly the shape an agent needs:

```
!!!! Warnings (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100) !!!!
  NLOC    CCN   token  PARAM  length  location
     126     28    916      1     162 handleResize@106-267@Sources/Tilr/Layouts/SidebarResizeObserver.swift
      42     37    240      1      42 parseKey@107-148@Sources/Tilr/HotKeyManager.swift
```

`--csv` gives one row per function: `nloc,ccn,token,param,length,long_name,file,name,signature,start,end`.
Thresholds are settable (`-C` CCN, `-L` length, `-a` args) and it exits non-zero when
any are breached, so it works as a gate as well as a report.

Role: the correctness-risk layer. The thing that names a specific function to refactor.

### codelimit — rejected

`brew install codelimit`. Measures unit length only, no cyclomatic complexity, and
reported tilr as "100% maintainable" while lizard found six functions over CCN 15 in
the same tree — no working Swift support. Its output is a coarse three-band verdict
with no per-unit ranking.

### Not yet evaluated

- `jscpd` (brew) — copy-paste detection, JSON reporter. The obvious third layer.
- `pmd` (brew) — includes CPD plus rulesets; rules are largely JVM-oriented.
- `ast-grep` (brew) — structural search, for project-specific style rules rather than metrics.
- `semgrep` (brew) — pattern rules, heavier.
- Language-specific: `golangci-lint` (gocognit), `swiftlint` (`cyclomatic_complexity`), `ruff` (C901).

## Shape of the deliverable

A skill that runs the tools and writes one report. Layers, in order of value:

1. scc — repo shape and the largest files
2. lizard — functions over CCN and length thresholds
3. duplication — pending, likely jscpd

The report must be **thresholded and ranked**, not a per-file table. State the limit,
list the N items over it, sort worst first. A full dump costs agent context and yields
no action.

## Decision

No own Homebrew formula. The skill documents `brew install scc` and `uv tool install lizard`
as prerequisites, and `home/claude/skills/code-metrics/code-metrics.sh` checks for both,
printing install instructions for whichever is missing and running with whichever is present.

Three things the wrapper has to handle that are not obvious:

- `uv tool install` puts `lizard` in `$HOME/.local/bin`, which `home/zshrc` adds to PATH but
  a non-interactive shell does not inherit. Without an explicit fallback the prerequisite
  check reports lizard missing immediately after a successful install.
- The two lizards cannot be told apart by `--version` — the compression tool prints
  `Lizard command line interface 64-bit 2.1.0` and the analyser prints a bare `1.23.0`.
  Grepping `--help` for `cyclomatic` discriminates.
- lizard exits non-zero when a threshold is breached. That is the finding, so `set -e` would
  abort the script exactly when it has something to say.
