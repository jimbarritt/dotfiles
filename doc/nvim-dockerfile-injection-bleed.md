# Dockerfile keywords losing highlighting (bash injection bleed)

Diagnosis and fix for Dockerfile instructions rendering as plain text in Neovim.
The cause is nvim-treesitter's dockerfile injections query, not the dockerfile
grammar.

## Symptom

In a multi-stage Dockerfile, some instructions highlight as keywords and some do
not. The split is positional rather than per-instruction: `FROM`, `WORKDIR` and
the first `COPY` are coloured, then everything from partway down the file is
plain, including further `COPY`, `FROM` and `RUN` lines. A later `USER` may be
coloured again.

## Ruling out the parser

The dockerfile parser is installed and the file parses cleanly — querying for
`(ERROR)` nodes returns nothing:

```vim
:lua local p = vim.treesitter.get_parser(0)
:lua local q = vim.treesitter.query.parse("dockerfile", "(ERROR) @e")
```

Dumping the captures at the start of each line shows the keywords are still
captured, but not alone:

```
 8 | keyword                     | COPY go.mod go.sum* ./
 9 | function.call,keyword       | RUN go mod download
11 | keyword,variable.parameter  | COPY cmd/ cmd/
13 | keyword,variable.parameter  | RUN CGO_ENABLED=0 go build ...
15 | keyword,variable.parameter  | FROM alpine:3
17 | keyword,variable.parameter  | RUN adduser -D -H backendify
18 | keyword                     | USER backendify
```

The unhighlighted lines are exactly those carrying a second `@variable.parameter`
capture, which is applied over `@keyword`.

## Cause

Walking the injected trees shows where that capture comes from:

```
TREE lang=dockerfile root=(1,0)-(19,0)
TREE lang=bash       root=(9,4)-(17,28)
  @variable.parameter  word  (9,11)-(13,17)   "download\n\nCOPY cmd/ cmd/\n\nRUN CGO_ENABLED"
  @variable.parameter  word  (13,46)-(17,11)  "./cmd/backendify\n\nFROM alpine:3\n\nRUN addu"
```

A single bash tree spans from the first `RUN` body to the last, and individual
bash `word` nodes run across the Dockerfile lines in between. Those lines are
inside the bash tree, so they get bash's captures on top of dockerfile's.

The directive responsible is in nvim-treesitter's
`queries/dockerfile/injections.scm`:

```scheme
((shell_command
  (shell_fragment) @injection.content)
  (#set! injection.language "bash")
  (#set! injection.combined))
```

`injection.combined` asks for every shell fragment in the file to be parsed as
one bash script. It exists so that a `RUN foo && \` continuation — which the
grammar splits into several `shell_fragment` nodes — parses as a single script
rather than as disconnected lines. The combined region is then parsed as one
contiguous span, so nodes are free to extend across the gaps between fragments.

Instructions above the first `RUN` fall outside the span, which is why the top of
the file highlights correctly.

## Fix

`config/nvim/after/queries/dockerfile/injections.scm` restates upstream's three
patterns with `injection.combined` removed.

The file does **not** start with `;; extends`. Query files found later on
`runtimepath` are appended to earlier ones when they carry that marker and
replace them when they do not. The fix removes a directive, and appending cannot
remove anything, so a full replacement is required.

Without `injection.combined`, each shell fragment is parsed as its own region.
Bash highlighting inside `RUN` bodies is retained, and multi-line `RUN ... && \`
continuations still highlight, since each fragment stands alone.

After the fix, every instruction captures as `keyword` with nothing layered over
it, and `apk` in a `RUN apk add ...` body still captures as `function.call`.

`dockerfile` was also added to `ensure_installed` in
`config/nvim/lua/plugins/treesitter.lua`; it had previously been arriving only
via `auto_install`.

## Maintenance

Because the override is a full replacement, it pins that query at the version it
was copied from. Changes upstream — new patterns, renamed nodes — will not be
picked up. Re-check after a `:TSUpdate` that pulls new dockerfile queries; if the
bleed is fixed upstream, delete `config/nvim/after/queries/dockerfile/`.

## Diagnostic technique

The same three steps apply to any "highlighting stops partway down the file"
report:

| Step | Command | Rules out |
|------|---------|-----------|
| Check for parse errors | query for `(ERROR) @e` | broken grammar, malformed file |
| Dump captures per line | `vim.treesitter.get_captures_at_pos()` | wrong capture vs. no capture |
| Walk injected trees | `parser:parse(true)` then `parser:for_each_tree()` | injection ranges bleeding |

Two captures on a token means a priority or layering question. One capture that
is simply the wrong group means a query pattern question. No capture at all means
a grammar or parser question.
