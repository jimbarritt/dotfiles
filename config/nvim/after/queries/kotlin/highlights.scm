;; extends

;; Type annotation colon → operator
(variable_declaration ":" @operator)
(parameter ":" @operator)
(function_declaration ":" @operator)

;; String interpolation — whole block as one colour (pri 200 beats LSP 127)
(string_literal
  "$" @string.interpolation
  (interpolated_identifier) @string.interpolation
  (#set! priority 200))
(string_literal
  "${" @string.interpolation
  (interpolated_expression) @string.interpolation
  "}" @string.interpolation
  (#set! priority 200))

;; Backtick-quoted test names → string colour (pri 200 beats LSP 127)
(function_declaration
  (simple_identifier) @string
  (#match? @string "^`")
  (#set! priority 200))

;; Constructor calls — uppercase first letter = type constructor
(call_expression
  (simple_identifier) @constructor
  (#match? @constructor "^[A-Z]")
  (#set! priority 200))
