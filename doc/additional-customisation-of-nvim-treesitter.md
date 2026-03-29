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
-- Type annotation operators (colon in type positions)
M.operator = {
  -- ... existing entries ...
  "@operator",
}

-- String interpolation delimiters
M.string_interpolation = {
  "@string.interpolation",
}

-- Constructor calls (distinguished from regular function calls)
M.constructor = {
  "@constructor",
}
```

Palette notes:
- `string_interpolation`: falls back to `string` → `text` if undefined.
  GitHub-light could map this to `keyword` (red). Green-dark could use
  `keyword` or `bracket_top`.
- `constructor`: falls back to `type` or `text`. GitHub-light maps to
  `type` (blue). Green-dark maps to `type` (blue-green).

---

## Case 1: Type annotation colon → @operator

### after/queries/kotlin/highlights.scm

```scheme
;; extends

;; Type annotation colon as operator
(variable_declaration ":" @operator)
(parameter ":" @operator)
(function_declaration ":" @operator)
(property_declaration ":" @operator)
```

### after/queries/typescript/highlights.scm

```scheme
;; extends

;; Type annotation colon as operator
(type_annotation ":" @operator)
```

### after/queries/java/highlights.scm

Not needed — Java doesn't use `:` for type annotations.

---

## Case 2: String interpolation delimiters → @string.interpolation

### after/queries/kotlin/highlights.scm (append to same file)

```scheme
;; String interpolation delimiters
(string_interpolation "${" @string.interpolation)
(string_interpolation "}" @string.interpolation)
(string_interpolation "$" @string.interpolation)
```

### after/queries/typescript/highlights.scm (append to same file)

```scheme
;; Template literal interpolation delimiters
(template_substitution "${" @string.interpolation)
(template_substitution "}" @string.interpolation)
```

### Fallback

`@string.interpolation` falls back to `@string` automatically via
Neovim's capture group resolution. Colourschemes that don't define it
get safe default behaviour.

---

## Case 3: Constructor vs function call → @constructor

Uses `#match?` on uppercase first letter. Works for Kotlin, TypeScript,
Java, Rust, Python.

### after/queries/kotlin/highlights.scm (append to same file)

```scheme
;; Constructor calls — uppercase first letter = type constructor
(call_expression
  (simple_identifier) @constructor
  (#match? @constructor "^[A-Z]")
  (#set! priority 200))
```

### after/queries/typescript/highlights.scm (append to same file)

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

### after/queries/java/highlights.scm

```scheme
;; extends

;; Constructor calls
(object_creation_expression
  type: (type_identifier) @constructor)
```

### Priority note

`#set! priority 200` beats LSP semantic tokens (125). Only needed for
case 3 because LSP classifies the identifier; cases 1 and 2 target
punctuation which LSP ignores.

---

## Verification steps

For each case, open a real file and run `:InspectTree` to confirm:

1. The `:` in `val user: User` — parent node should be `variable_declaration` or similar
2. The `${` in `"hello ${name}"` — parent node should be `string_interpolation` or `template_substitution`
3. `User` in `User()` — parent node should be `call_expression`

Then `:Inspect` on the token to confirm the correct capture group won.
If the node names differ from what's written above, adjust the query
patterns to match.
