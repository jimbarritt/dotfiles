; Deliberately NOT `;; extends` — this replaces nvim-treesitter's dockerfile
; injections query rather than adding to it.
;
; Upstream sets `injection.combined` on the bash injection, which merges every
; RUN body in the file into a single bash tree. Neovim then parses that region
; as one contiguous span, so bash nodes run straight through the Dockerfile
; lines sitting between two RUN instructions. Those nodes capture as
; @variable.parameter, which outranks the @keyword capture on COPY / FROM / RUN
; and leaves them rendered as plain text.
;
; Dropping `injection.combined` parses each shell fragment on its own, which
; keeps the injected bash highlighting without letting it bleed across
; instructions.
((comment) @injection.content
  (#set! injection.language "comment"))

((shell_command
  (shell_fragment) @injection.content)
  (#set! injection.language "bash"))

((run_instruction
  (heredoc_block) @injection.content)
  (#set! injection.language "bash")
  (#set! injection.include-children))
