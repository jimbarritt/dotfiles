# Source tree directory conventions

Naming scheme for top-level directories in a source tree.

## Layout

```
src/        one source tree
bin/        commands on PATH
scripts/    development and build tooling
docs/       documentation, including docs/adr
ops/        infrastructure and deployment
tests/      tests, if the language does not place them elsewhere
build/      build output, ignored by git
```

## Rule

A directory that holds one tree or one product takes the singular. A
directory that holds a set of separate, independently named items takes
the plural. `bin` and `lib` are fixed exceptions, because the names come
from Unix.

## Rationale

### `docs`, not `doc`

The directory holds many separate named files, so the plural fits the
same pattern as `tests` and `examples`.

GitHub Pages accepts only the repository root or `/docs` as a publish
source. No other folder name is offered.

MkDocs, Docusaurus and most static site generators default to `docs`.

The Unix argument does not apply here. `/usr/share/doc` is an install
path in a system layout, not a source tree. `src`, `tests` and `scripts`
are not in the FHS at all.

### `bin` and `scripts`, split by audience

- `bin/` — commands the user runs by name, on `PATH`. This matches
  `/usr/bin`.
- `scripts/` — development and build tooling that the project runs, not
  the user. Test helpers, release steps, code generation.

Build output does not belong in `bin/`. Use `build/` or `target/` for
output.

### `ops` for infrastructure

`ops` is short, has no plural question, and covers deployment,
infrastructure and operational tooling in one name.

`deploy` and `deployments` name one activity, then end up holding
monitoring and runbooks too.

Subdirectories of `ops` take the tool or target name: `ops/terraform`,
`ops/k8s`, `ops/ansible`.

## Evidence

### Unix and FHS

The FHS lists these subdirectories of `/usr/share`: `man` (required),
`misc` (required), `doc`, `info`, `locale`. All are singular. Manual
section directories are `man1`, `man2`, `man8`.

`/usr/bin`, `/usr/sbin`, `/usr/lib`, `/usr/local`, `/etc`, `/var`,
`/opt` are all singular or abbreviations. `/usr/share/doc` is singular
`doc`.

The FHS describes a system install layout, not a source tree. It gives
no rule about plurals. The singular names are abbreviations of mass
nouns: "documentation" and "binaries" shortened to `bin`, "library" to
`lib`.

### Documentation directory naming, across projects

Plural `docs`:

- MkDocs: `docs_dir` defaults to `docs`.
- Docusaurus and Read the Docs projects: `docs` by convention.
- Standard Go Project Layout: `/docs` — "Design and user documents".
- GitHub Pages: the publish source can only be the repository root or
  `/docs`. No other folder name is offered.
- Most large modern repositories: Django, React, Kubernetes, Rust
  crates.

Singular `doc`:

- CPython: `Doc/`.
- The Go repository itself: `doc/`.
- GNU projects, by the GNU coding standards habit.
- `adr-tools`: the default ADR path is `doc/adr`.

Other:

- Linux kernel: `Documentation/`.

The plural form leads by a wide margin in code written after about
2012.

### `bin` and `scripts`, across sources

Sources disagree on the split:

- Standard Go Project Layout has `/scripts` for "build, install,
  analysis" operations, and no `/bin`. It adds `/build` for packaging
  and CI, and `/tools` for supporting tools.
- Node and JavaScript layout guides use `bin/` for executables shipped
  or built by the module, and `scripts/` for development utilities.
- Some guides invert this: `scripts/` holds installable programs for
  `/usr/local/bin`, and `bin/` holds developer convenience commands.
- GitHub's "Scripts to Rule Them All" uses a singular `script/`
  directory with fixed names: `script/bootstrap`, `script/setup`,
  `script/update`, `script/test`, `script/server`.

Two facts are stable across all sources:

1. `bin` maps to the meaning of `/usr/bin`: a program a user runs by
   name.
2. Many build tools and `.gitignore` templates treat `bin/` as build
   output. This is a real risk if `bin/` also holds hand-written
   scripts.

### Infrastructure directory naming, across projects

No convention holds. Observed names: `infra`, `infrastructure`, `ops`,
`deploy`, `deployments` (Standard Go Project Layout), `terraform`,
`envs`, `build`, `init` (Standard Go Project Layout, for systemd and
supervisor configs).

`ops` is an abbreviation of "operations", a mass noun. It carries no
plural question.

### The singular/plural pattern, tested

Test the pattern against the Cargo package layout, which is enforced
by the tool:

| Directory  | Form     | Holds                        |
|------------|----------|------------------------------|
| `src`      | singular | one source tree              |
| `src/bin`  | singular | executables                  |
| `tests`    | plural   | many separate test files     |
| `examples` | plural   | many separate programs       |
| `benches`  | plural   | many separate benchmarks     |

The pattern holds here. `src` is one tree with one root. `tests` and
`examples` are sets of independent items, each with its own name.

`bin` is the exception. It holds many separate programs but stays
singular, because the name comes from `/usr/bin` and from the word
"binaries".

STE: checked
