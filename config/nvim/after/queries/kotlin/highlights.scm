;; extends

;; Type annotation colon → operator
(variable_declaration ":" @operator)
(parameter ":" @operator)
(function_declaration ":" @operator)

;; Constructor calls — uppercase first letter = type constructor
(call_expression
  (simple_identifier) @constructor
  (#match? @constructor "^[A-Z]")
  (#set! priority 200))
