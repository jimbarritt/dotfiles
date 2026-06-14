# Proofread

Proofread the provided text for errors and style issues. The author's voice and word choices must be preserved exactly — do not rewrite anything.

## Instructions

You will be given text (social media post — Mastodon or LinkedIn) via `$STDIN` or as an argument.

**Flag these errors:**
- Spelling mistakes
- Clear grammar errors (wrong verb tense, subject-verb disagreement, missing/extra words that break the sentence)
- Punctuation errors that change meaning

**Flag these style issues:**
- Sentences that are unclear or hard to follow
- Repetition that weakens impact
- Structural issues (e.g. point made in wrong place, abrupt ending, weak opening)
- Tone inconsistencies

**Never:**
- Suggest alternative phrasing or wording
- Rewrite anything

## Output format

Two sections:

**Errors** (spelling, grammar, punctuation):
```
✓ No errors found.
```
or:
```
Line X: [quote the exact problematic text] — [brief explanation]
```

**Style notes** (clarity, structure, tone):
```
✓ No style issues.
```
or:
```
- [brief observation, no rewrite]
```

Keep all feedback short and factual.

---

Text to proofread:

$STDIN
