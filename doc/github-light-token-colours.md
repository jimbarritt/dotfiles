# GitHub Light — Target Token Colours

Reference file: `App.kt` from the dotfiles Kotlin project.

## Colour Palette

| Name | Hex | Used for |
|------|-----|----------|
| Grey | `#59636e` | Comments |
| Red | `#cf222e` | Keywords |
| Purple | `#8250df` | User type names, function declarations |
| Blue | `#0550ae` | System/stdlib types, system functions, package names |
| Dark blue | `#0a3069` | Strings |
| Black | `#1f2328` | Variables, operators, punctuation, user function calls |

## Token-by-Token Specification

Status: `[ ]` = not yet verified, `[x]` = verified correct, `[!]` = known limitation

| Token | Context | Colour | Status |
|-------|---------|--------|--------|
| `/* */` `* comment text` | comments | grey `#59636e` | [x] |
| `package` | keyword | red `#cf222e` | [x] |
| `org.example` | package name | purple `#8250df` (via `@lsp.type.namespace`) | [x] |
| `class` `fun` `val` `return` | keywords | red `#cf222e` | [x] |
| `App` (line 9, declaration) | user type decl | purple `#8250df` | [x] |
| `App` (line 1, constructor) | user type | purple `#8250df` | [!] `@lsp.type.method` ambiguity — shows black |
| `User` | user type ref | purple `#8250df` | [x] |
| `String` | system type | blue `#0550ae` | [x] |
| `greet` (line 8, declaration) | function decl | purple `#8250df` | [x] |
| `main` (line 3, declaration) | function decl | purple `#8250df` | [x] |
| `greet` (line 1, call) | user function call | black `#1f2328` | [x] |
| `println` | system function | blue `#0550ae` | [x] |
| `user` | variable | black `#1f2328` | [x] |
| `.` `,` `(` `)` `{` `}` | punctuation/brackets | black `#1f2328` | [x] |
| `=` `+` `-` etc. | operators | red `#cf222e` | [x] |
| `:` (param type, e.g. `user: String`) | type annotation colon | red `#cf222e` | [!] TS gives `@punctuation.delimiter` for both param and return type colons — can't distinguish |
| `"Jim"` `"Barritt"` `"Hello..."` | strings | dark blue `#0a3069` | [x] |
| `${user.firstName}` | string interpolation | purple `#8250df` | [!] LSP overrides treesitter inside strings — see note below |

## Known LSP Limitations

### `@lsp.type.method` ambiguity

The Kotlin LSP sends `@lsp.type.method` (priority 125) for **both** constructor
calls like `User()` and regular method calls like `.greet()`. These cannot be
distinguished at the highlight group level.

- `User()` should be **purple** (it's a constructor — user type)
- `.greet()` should be **black** (it's a user function call)

The LSP provides no `constructor` token type or modifier for Kotlin. Both are
just `method`.

**TODO**: Revisit this. Possible approaches:
1. Accept compromise (both purple or both black)
2. Post-processing with autocommand that inspects the token text (e.g. capitalised = constructor)
3. Check if a future Kotlin LSP version adds constructor modifiers

Diagnostic data:
```
'User' (constructor call):
  TS: @variable.kotlin               pri=100
  TS: @function.call.kotlin          pri=100
  SEM: @lsp.type.method.kotlin → @lsp.type.method  pri=125

'.greet' (method call):
  TS: @variable.kotlin               pri=100
  TS: @variable.member.kotlin        pri=100
  TS: @function.call.kotlin          pri=100
  SEM: @lsp.type.method.kotlin → @lsp.type.method  pri=125
```

The only treesitter difference is `@variable.member` on `.greet()` (it's accessed
via `.`), but both get identical LSP tokens. LSP at pri=125 wins over treesitter
at pri=100, so the treesitter difference is invisible.

### String interpolation: LSP vs treesitter context

**The core problem**: GitHub uses TextMate grammars which are scope-based — when
you're inside a string, everything inherits the string scope unless explicitly
overridden. An interpolation like `${user.firstName}` gets a scope like
`meta.template.expression` which GitHub maps to entity (purple). The key point is
that TextMate scoping is *hierarchical* — the grammar knows the expression is
*inside a string*.

Neovim's two highlighting systems don't have this hierarchy:

- **Treesitter** (pri=0): correctly gives `@string` to the whole literal, but also
  gives `@variable` and `@none` to the interpolated content. These are flat, not nested.
- **LSP semantic tokens** (pri=125-127): fire on `user.firstName` with
  `@lsp.type.parameter` (pri=125), `@lsp.mod.readonly` (pri=126), and
  `@lsp.typemod.parameter.readonly` (pri=127). The LSP has **no concept** that
  these tokens are inside a string — it only knows the symbol's type.

Because LSP priority (125-127) always beats treesitter (0), the string context is
invisible to the winning highlight. The result: `user.firstName` renders as black
(via `@lsp.mod.readonly` → constant → text fallback) instead of purple.

**Possible fix**: An autocommand or decoration provider that inspects the treesitter
tree at each LSP extmark position. If the token sits inside a `string_content` or
`string_template_content` treesitter node, override it with the entity colour. This
would restore the hierarchical context that TextMate grammars provide natively.

This is likely the same class of problem behind other LSP-vs-context mismatches —
anywhere the LSP token type is "correct" for the symbol but the *visual context*
(string, comment, etc.) should take precedence.

### Type annotation colon vs return type colon

Kotlin treesitter emits `@punctuation.delimiter.kotlin` for **both** the colon in
parameter type annotations (`user: String`) and return type annotations (`fun greet(): String`).

- `user: String` colon should be **red** (GitHub treats it as keyword-like syntax)
- `fun greet(): String` colon should be **black** (punctuation)

These can't be distinguished with current treesitter captures. The real fix would be
for the Kotlin treesitter grammar to assign a different capture (e.g. `@keyword.type`
or `@type.annotation`) to the parameter type colon. Left as black for now since it
also affects `,` between parameters which should stay black.

## Diagnostic Tool

Run `:luafile ~/.config/nvim/lua/colorscheme/diagnose.lua` with cursor on any
line to dump treesitter captures, LSP semantic tokens, and resolved colours for
every token on that line.
