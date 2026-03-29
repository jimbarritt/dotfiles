# Colorscheme Slot System — Addendum: Custom Tree-sitter Queries

Extends the slot system with three custom captures that go beyond what
LSP/default tree-sitter provides. Each case requires two things:
a query file in `after/queries/{lang}/highlights.scm`, and a mapping
entry in `mapping.lua`.

## Note on query syntax

Tree-sitter queries use S-expression syntax — the same parenthesised prefix
notation used by Lisp. The `.scm` file extension stands for Scheme (a Lisp
dialect). It's not actually executable Scheme — it's a pattern-matching DSL
that borrows the syntax because S-expressions map naturally to tree pattern
matching: you're literally writing the shape of the AST subtree you want to
match, with nested parentheses mirroring the tree structure.

## Mapping additions (mapping.lua)

```lua
M.string_interpolation = {
  "@string.interpolation",
}

M.constructor = {
  "@constructor",
}
```

The `@operator` group already exists in the operator slot — the colon
queries just capture `:` as `@operator` which routes through the
existing slot.

Palette colours (github-light):
- `string_interpolation = "#8250df"` — purple, like entity
- `constructor = "#8250df"` — purple, like entity
- `operator = "#cf222e"` — red (already existed)

---

## Case 1: Type annotation colon → @operator

**Goal:** `:` in `val user: String` should be red (operator) not black (text).

No priority boost needed — LSP ignores punctuation so treesitter wins by default.

### Kotlin

```scheme
(variable_declaration ":" @operator)
(parameter ":" @operator)
(function_declaration ":" @operator)
```

**Important:** `property_declaration ":" @operator` does NOT work — the colon
is not a direct anonymous child of that node in Kotlin's grammar (it's an
"impossible pattern" that silently matches nothing).

### TypeScript

```scheme
(type_annotation ":" @operator)
```

### Java

Not needed — Java doesn't use `:` for type annotations.

---

## Case 2: String interpolation → @string.interpolation

**Goal:** `${user.firstName}` inside strings should be one solid colour block
instead of being broken up by LSP semantic tokens.

Needs `#set! priority 200` because LSP semantic tokens go up to priority 127
(`@lsp.typemod.parameter.readonly` at 127, `@lsp.mod.readonly` at 126,
`@lsp.type.parameter` at 125). Priority 200 beats all of them.

### Kotlin

The correct grammar node names are `interpolated_identifier` (for `$name`)
and `interpolated_expression` (for `${expr}`). **NOT** `string_interpolation`
— that node does not exist in Kotlin's treesitter grammar. Invalid node names
cause silent query parse errors.

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

Both patterns are anchored inside `string_literal` to match the structure of
the base nvim-treesitter query (which uses the same parent node).

### TypeScript

Not yet implemented — template literal interpolation (`${...}`) could use
`template_substitution` nodes but hasn't been added yet.

---

## Case 3: Constructor vs function call → @constructor

**Goal:** `User()` and `App()` should be purple (constructor/entity) instead
of black (regular function call).

Uses `#match?` on uppercase first letter. Needs `#set! priority 200` because
LSP classifies these as `@lsp.type.method` (priority 125).

### Kotlin

```scheme
(call_expression
  (simple_identifier) @constructor
  (#match? @constructor "^[A-Z]")
  (#set! priority 200))
```

### TypeScript

```scheme
;; Explicit new expressions
(new_expression
  constructor: (identifier) @constructor)

;; Uppercase function calls (React components, factory functions)
(call_expression
  function: (identifier) @constructor
  (#match? @constructor "^[A-Z]")
  (#set! priority 200))
```

### Java

Not yet implemented — could use `object_creation_expression` with
`type_identifier`.

---

## Key lessons learned

1. **Node names must match the actual grammar** — use `:InspectTree` or
   `vim.treesitter.language.inspect()` to find real node names. Invalid names
   fail silently.

2. **`#set! priority 200` beats LSP semantic tokens** — treesitter and LSP
   share the same extmark priority namespace. LSP tops out at ~127; priority
   200 wins cleanly.

3. **`property_declaration ":"` is impossible in Kotlin** — the colon isn't a
   direct child. Only `variable_declaration`, `parameter`, and
   `function_declaration` work for colon capture.

4. **Restart Neovim fully** after changing `after/queries/` files — the
   combined query is cached and `:colorscheme` reload alone won't pick up
   new `.scm` files.

## Verification

For each case, use `:Inspect` on the token to confirm the correct capture
group won and the colour is right. The diagnostic command
`:luafile ~/.config/nvim/lua/colorscheme/diagnose.lua` shows all captures
with priorities on a whole line.
