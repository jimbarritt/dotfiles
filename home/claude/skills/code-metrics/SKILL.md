---
name: code-metrics
description: Produce a ranked structural report on a codebase — size per file, functions over cyclomatic complexity and length thresholds, parameter counts — so refactoring targets can be named specifically. Use when asked about code quality, complexity, structure, hotspots, technical debt, or where to start refactoring in an unfamiliar codebase.
---

# Code metrics

Run `code-metrics.sh` and read its report. Do not gather these numbers by hand.

## Running it

```
~/.claude/skills/code-metrics/code-metrics.sh [--top N] [--ccn N] [--length N] [TARGET]
```

Defaults: `--top 15`, `--ccn 15`, `--length 60`, target is the current directory.

The report goes to stdout as markdown. It is already ranked and thresholded — read it
whole. Never write it into the target repository; that repository is usually not the
one being worked in.

Raise `--ccn` and `--length` on a codebase where the defaults flag most of it. Lower
them when looking for a shortlist on already-tidy code.

## Prerequisites

The script checks for these and prints install instructions for any that are missing.
It runs with whichever are present, so a partial report is normal rather than an error.

- `scc` — `brew install scc`
- `lizard` — `uv tool install lizard`

Homebrew has a formula called `lizard` that is an unrelated LZ4 compression tool. If
the script reports the wrong tool is installed, `brew uninstall lizard` then install
via `uv`.

## Reading the report

**Shape** and **largest files** locate the codebase. Large markdown or generated files
appearing high in the list are not findings.

**Functions over complexity threshold** is the section that names work. Cyclomatic
complexity is McCabe, measured per function by lizard. A function at CCN 30+ has more
independent paths than a test suite is likely to cover.

**Functions over length threshold** overlaps heavily with the complexity list. A
function that appears in both is a stronger candidate than one appearing in either.

**Most parameters** points at functions taking a bag of loosely related arguments.

Locations are `name@startLine-endLine@path`, so a named function can be opened directly.

## Turning the report into suggestions

Propose the smallest number of specific changes, each naming a function from the report.
Read a function before proposing anything about it — the metrics say where to look, not
what is wrong. High complexity in a parser or a dispatch table is often correct.

Do not propose a repo-wide refactor off the back of a metrics run, and do not report
totals as if they were findings.
