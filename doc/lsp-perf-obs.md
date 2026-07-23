# LSP setup: token/context observation log

Tracking whether fixing the rust-analyzer Claude Code plugin (and the doc-then-memory bootstrap pattern in `doc/claude-lsp-integration.md`) measurably reduces token spend in Rust sessions, comparing before/after **2026-07-18 14:37 BST**.

**Baseline marker: 2026-07-18 14:37 BST.**

## Two separate mechanisms — don't conflate them when comparing

1. **rust-analyzer actually working (the big one).** Before this session, `~/.cargo/bin/rust-analyzer` errored on invocation — every LSP-backed navigation call (go-to-definition, find-references, diagnostics) silently failed. Rust sessions before this point were almost certainly falling back to `Grep`/`Read`-based exploration — reading whole files or broad-greping to trace a symbol — where a working LSP returns just the match locations directly. This is the mechanism expected to actually move the needle on token cost, and it applies to every Rust session going forward, not just new-machine setup.
2. **Doc-then-memory bootstrap (the small one).** Only saves the one-off *setup* discovery cost (this session's rustup/mise/PATH debugging) on a new machine or fresh memory dir. Doesn't affect steady-state per-session cost once a machine has already bootstrapped.

## Why a naive "total tokens this week vs last week" comparison will be noisy

Task mix varies session to session — how much of a session is navigation-heavy vs writing new code vs running tests changes independently of whether LSP works. A clean read needs either broadly similar tasks in both windows, or a narrower metric than total tokens, e.g.:

- Tokens spent specifically on navigation/exploration turns (not the whole session)
- `Read`/`Grep` call counts per session, as a proxy for LSP-avoidance — should drop post-fix
- Whether LSP-tool calls (go-to-def, find-references) appear in the transcript at all — they couldn't succeed before this fix, so their presence itself is a marker

## Investigated and ruled out: sed/script-editing as an LSP-fixable cost

A separate hypothesis was raised: that Claude sessions lean on throwaway Python scripts and heavy `sed` usage for edits, and that a working LSP would reduce this. Checked by grepping the full session transcript history (~29 sessions) of an unrelated, actively-worked repo with substantial history, counting `sed -i` calls, `Write` calls to editing-script files, and `python3 -c`/inline-edit invocations, against plain `Edit`/`MultiEdit` tool-call counts for the same sessions.

**Result: not an LSP-fixable cost, and not really a problem.**

- No throwaway Python editing scripts found at all — zero `.py` files written, zero `MultiEdit` calls. The only `python3 -c` calls were read-only data inspection, not editing.
- `sed -i`: ~3.5% of edit-equivalent tool calls (a few dozen, against 800+ plain `Edit` calls) — a small minority, not a dominant pattern.
- Every `sed` use traced to one of two genuinely mechanical scenarios, never to an `Edit` call failing to match: (a) the identical literal change repeated across many files at once (a rename or added-argument sweep), where `sed` collapses what would be dozens of `Edit` calls into one; (b) deleting an already-confirmed, precisely-bounded line range in a large file (e.g. archiving a completed section of a planning doc), where matching hundreds of lines exactly as an `Edit` "old string" is impractical.

Since LSP only provides diagnostics, navigation, and type/doc info (see `claude-lsp-integration.md`) — no rename/refactor/write action — it has no mechanism to affect this pattern either way. Doesn't need tracking as part of the LSP token-cost comparison.

## Log

- **2026-07-18 14:37 BST** — baseline set. `doc/claude-lsp-integration.md` and the rust-analyzer-lsp plugin fixed up this session (mise install, absolute-path fix for `~` non-expansion, `lsp-doctor` extended for rust-analyzer). No machine has yet gone through the new bootstrap-from-doc-into-memory flow.
- **2026-07-18 15:07 BST** — sed/script-editing hypothesis investigated and ruled out (see above); not a factor in the LSP comparison.
