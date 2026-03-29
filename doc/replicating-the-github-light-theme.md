# Replicating the GitHub Light Theme in Neovim

A narrative account of building a pixel-accurate replica of GitHub's
syntax highlighting in Neovim, and the surprisingly deep rabbit holes
along the way.

## The starting point

The goal was simple: make Kotlin code in Neovim look exactly like it
does on github.com. GitHub uses a system called PrettyLights — about
15 colour slots fed by TextMate grammar scopes. Six colours do most of
the work: grey for comments, red for keywords, purple for declarations
and user types, blue for stdlib types, dark blue for strings, and black
for everything else.

We mapped these colours to our slot system
(see [colorscheme-slot-system-design.md](colorscheme-slot-system-design.md))
and got most tokens right on the first pass. The full token-by-token
specification lives in
[github-light-token-colours.md](github-light-token-colours.md).

Most of it just worked. Then came the three cases that didn't.

## Problem 1: Type annotation colons

In `val user: String`, GitHub colours the `:` red (as an operator).
Neovim's treesitter gives it `@punctuation.delimiter` — which maps to
black text in our system.

The fix was straightforward: custom treesitter queries in
`after/queries/kotlin/highlights.scm` that capture the colon as
`@operator` inside specific parent nodes:

```scheme
(variable_declaration ":" @operator)
(parameter ":" @operator)
(function_declaration ":" @operator)
```

One pitfall: `property_declaration ":" @operator` looks like it should
work but doesn't — the colon isn't a direct anonymous child of that
node in Kotlin's grammar. It silently matches nothing. This kind of
invisible failure is the recurring theme of treesitter query debugging.

## Problem 2: Constructor calls

`User()` should be purple (it's constructing a type) but the Kotlin LSP
sends `@lsp.type.method` for both constructor calls and regular method
calls like `.greet()`. At priority 125, the LSP wins over treesitter's
priority 100, so both end up black.

The LSP can't help us here — it has no constructor token type for
Kotlin. But treesitter can: constructor calls start with an uppercase
letter. A `#match?` predicate with `#set! priority 200` beats the LSP:

```scheme
(call_expression
  (simple_identifier) @constructor
  (#match? @constructor "^[A-Z]")
  (#set! priority 200))
```

This was the first time we needed to understand the priority system.
Treesitter and LSP semantic tokens share the same extmark priority
namespace. Default treesitter is 100, LSP tokens range from 125 to 127.
Setting priority 200 on our custom capture wins cleanly.

## Problem 3: String interpolation

This was the hardest one, and the one that nearly broke everything else.

On GitHub, `${user.firstName}` inside a string renders as solid purple.
This works because TextMate grammars are hierarchical — the interpolation
inherits the string scope, and GitHub maps the `meta.template.expression`
scope to entity (purple). The grammar *knows* the expression is inside
a string.

Neovim's two highlighting systems don't have this hierarchy. Treesitter
gives `@string` to the whole literal but also `@variable` and `@none`
to the interpolated content — flat, not nested. Then the LSP fires
semantic tokens on `user.firstName`: `@lsp.type.parameter` at priority
125, `@lsp.mod.readonly` at 126, `@lsp.typemod.parameter.readonly` at
127. The LSP has no concept that these tokens are inside a string. It
just knows the symbol's type.

The fix mirrors the constructor approach — re-capture the interpolation
parts at priority 200:

```scheme
(string_literal
  "$" @string.interpolation
  (interpolated_identifier) @string.interpolation
  (#set! priority 200))
(string_literal
  "${" @string.interpolation
  (interpolated_expression) @string.interpolation
  "}" @string.interpolation
  (#set! priority 200))
```

Getting here required discovering that:

- **`string_interpolation` doesn't exist** in Kotlin's treesitter
  grammar. The correct nodes are `interpolated_identifier` and
  `interpolated_expression`. Invalid node names cause silent parse
  errors that prevent the entire query file from loading — taking your
  colon and constructor fixes with it.
- The patterns must be anchored inside `string_literal` to match the
  base nvim-treesitter query structure.
- A full Neovim restart is needed after changing `after/queries/` files
  — the combined query is cached and `:colorscheme` reload alone won't
  pick up new `.scm` files.

## The Kotlin LSP detour

Midway through debugging, the Kotlin LSP stopped starting entirely.
Turned out brew had updated the `kotlin-lsp` package and changed the
binary layout — from `libexec/bin/kotlin-lsp` to `libexec/kotlin-lsp.sh`.
The `kotlin.nvim` plugin only auto-detects the bundled JRE for Mason
installs, not when using `KOTLIN_LSP_DIR`. The fix was to resolve the
JRE path explicitly and pass `jre_path` in the plugin setup.

## What made it hard

The difficulty wasn't in any single change. Each fix is a few lines of
treesitter query plus a palette entry. What made it hard was:

1. **Silent failures** — invalid node names, impossible patterns, and
   query parse errors all fail silently. Your query just doesn't match
   and you don't know why.

2. **Shared priority namespace** — understanding that treesitter (100),
   LSP (125-127), and custom queries (200) compete in the same extmark
   priority system was essential.

3. **Blast radius** — a broken query file kills all captures in that
   file. When we added string interpolation with a wrong node name,
   it silently broke the colon and constructor fixes too. The lesson:
   commit working changes before adding the next thing.

4. **The TextMate gap** — GitHub's highlighting works because TextMate
   scopes are hierarchical. Neovim's treesitter + LSP is flat. Bridging
   that gap requires explicit priority overrides for every case where
   context (being inside a string) should beat identity (being a
   parameter).

## The result

All three cases now work. The theme is a picture-perfect replica of
GitHub's syntax highlighting for Kotlin. The full technical details of
each custom query are in
[additional-customisation-of-nvim-treesitter.md](additional-customisation-of-nvim-treesitter.md).

## Files involved

- `config/nvim/after/queries/kotlin/highlights.scm` — all three custom captures
- `config/nvim/after/queries/typescript/highlights.scm` — TypeScript equivalents
- `config/nvim/lua/colorscheme/mapping.lua` — `string_interpolation` and `constructor` slots
- `config/nvim/lua/colorscheme/palettes/github-light.lua` — purple for both new slots
- `config/nvim/lua/colorscheme/palettes/green-dark.lua` — green-dark equivalents
- `config/nvim/lua/colorscheme/palettes/green-light.lua` — green-light equivalents
