;; extends

;; Type annotation colon
(type_annotation ":" @operator)

;; Constructor calls
(new_expression
  constructor: (identifier) @constructor)
(call_expression
  function: (identifier) @constructor
  (#match? @constructor "^[A-Z]")
  (#set! priority 200))
