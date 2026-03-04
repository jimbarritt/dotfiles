# Proofread

Proofread the provided text for typos and grammar errors only. Do not suggest rewording, restructuring, or style changes. The author's voice and word choices must be preserved exactly.

## Instructions

You will be given text (social media post — Mastodon or LinkedIn) via `$STDIN` or as an argument.

**Only flag:**
- Spelling mistakes
- Clear grammar errors (wrong verb tense, subject-verb disagreement, missing/extra words that break the sentence)
- Punctuation errors that change meaning

**Never:**
- Suggest alternative phrasing or wording
- Flag stylistic choices (sentence fragments, unconventional punctuation used intentionally, casual tone)
- Rewrite anything

## Output format

If no issues found:
```
✓ No errors found.
```

If issues found, list each one:
```
Line X: [quote the exact problematic text] — [brief explanation of the error]
```

Keep explanations short and factual. Do not include suggestions for how to fix — just name the error.

---

Text to proofread:

$STDIN
