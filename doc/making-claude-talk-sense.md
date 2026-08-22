# Making Claude talk sense: the STE rule, and why it slipped

## Short version

The global CLAUDE.md has a standing rule: write all output in ASD-STE100
Simplified Technical English. Claude wrote a doc that broke the rule while
the rule sat in context. The fix is not more memory. It is a write-time
check with a visible pass mark.

## The problem

A rule loaded once at session start is a policy, not a trigger. It competes
with the task at hand. A technical debrief pulls the model's output toward
technical-debrief prose, and the STE rule loses that pull unless something
ties it to the act of writing.

"Always on" states a permanent condition. It does not name a moment when
Claude must act on it. A rule with no trigger binds to nothing.

## The fix

Asked Fable (`claude-fable-5`) for a structural fix, not a rewording. Fable's
answer: turn the policy into a write-time procedure with a forced, visible
step.

Three changes went into the CLAUDE.md `## STE` section:

1. **A write-time gate.** Before any prose — a reply, a doc, a commit
   message — run a check on the draft. Do this every time, not once per
   session.
2. **Five test questions, each with a bad/good example.** A test is
   something a check can run. A category label ("active voice") is not.
3. **A required output token.** End a reply that has prose, or a doc just
   written or edited, with the line `STE: checked`. Do not write that line
   unless the check ran. A skipped check is now visible in the reply
   itself, not just in the document.

## What this does not fix

A rule inside the same channel that failed cannot fully fix an attention
failure. Fable named a deterministic backstop: a Claude Code PostToolUse
hook on Write/Edit for `.md` files, running a script that flags `-ing`
verbs, banned words, and long sentences.

Decision: hold off. Try the write-time check first, since it costs no
extra infrastructure. Build the lint hook only if the check keeps failing
in practice.

## Source

Consult transcript: Jim's portfolio project, 22 August 2026, following a
non-compliant exec-summary doc in the Fonoa debrief folder.

STE: checked
