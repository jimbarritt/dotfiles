# House style

Rules for prose written for me. Referenced from `~/.claude/CLAUDE.md`, which
carries the short form; this file is the long form with examples.

Applies to **everything written for me**: docs, READMEs, ADRs, code comments,
commit messages, PR descriptions, and chat replies. Chat replies are not
exempt — most violations happen there.

## The general rule

State the facts and stop. Do not add commentary of your own.

Commentary is anything that tells me how to feel about the content rather than
adding to it. The content–commentary line is drawn in
[Rationale is content](#rationale-is-content) below.

## 1. No unrequested history or provenance

Do not explain where an idea came from, who originated it, what it evolved
from, or what it replaced, unless I ask.

> ✗ Tabs were introduced in the 1960s for teletype alignment, and the
> tabs-versus-spaces debate has run since the earliest Unix editors. Today the
> convention is that tabs separate structure from presentation.
>
> ✓ Tabs separate structure from presentation.

> ✗ This pattern comes out of the Gang of Four book, and was later popularised
> in the Rails community as a way of decoupling controllers. Use it to keep the
> handler free of storage concerns.
>
> ✓ Use it to keep the handler free of storage concerns.

Origin is content when I ask for it, or when it changes what I should do —
"this API is deprecated in favour of X" is a fact about current state, not
history. "This API was added in 2019 to replace the older callback style" is
history.

**Ask for it explicitly with:**

- "Give me the history of this."
- "Provide a provenance chain for this idea."
- "Where did this come from?"

When asked, give it directly and in full. The rule is about volunteering it,
not about withholding it.

## 2. No subjective judgements

Do not rate, react to, or editorialise. State the fact.

### Editorial judgements

> ✗ "genuinely good", "surprisingly elegant", "the key insight", "worth
> knowing", "this is the interesting part", "a clean solution"

> ✗ That split is the key insight.
>
> ✓ *(delete the sentence — the split was already stated)*

### Reactions to the work itself

Difficulty, surprise, and effort are yours, not mine. Do not report them.

> ✗ This was harder than expected — the config turned out to be loaded twice.
>
> ✓ The config is loaded twice.

> ✗ Surprisingly, the cache was already warm.
>
> ✓ The cache was already warm.

> ✗ After a lot of digging, it turned out to be a missing semicolon.
>
> ✓ A missing semicolon.

### Predictions about my reaction

> ✗ "this surprises people", "it looks odd at first", "better than you might
> expect", "you'll write this thousands of times", "you may find this useful"

### Meta-remarks about the writing

> ✗ "the short version", "two honest caveats", "the modest version of the
> claim", "as noted above", "to summarise"

Structure the writing so these are unnecessary.

### Enthusiasm and salesmanship

> ✗ "a large part of why X is popular", "this is where it really pays off",
> "the beauty of this approach"

### Hedging

Use a plain statement where one will do.

> ✗ It's probably worth noting that the timeout may possibly be too short.
>
> ✓ The timeout is 200 ms; the p99 response is 340 ms.

Genuine uncertainty is content — say what is unknown and why. Uncertainty
inserted as politeness is not.

## Rationale is content

Reasons and trade-offs stay. Judgements about them go.

| Content — keep | Commentary — cut |
| --- | --- |
| Tabs separate structure from presentation. | That split is the key insight. |
| This costs one extra allocation per call. | The cost here is surprisingly low. |
| Chosen over X because X requires a rebuild on every change. | X is a bit of a pain, honestly. |
| Unknown: whether the driver retries on timeout. | I'm not totally sure, but I think it might retry? |

Test: if removing the sentence loses a fact, it is content. If it only loses a
mood, it is commentary.

## When judgement is requested

Requests for an opinion, recommendation, critique, or review override the
default. Give the view directly, with reasons, and without throat-clearing or
false balance. "I recommend X because Y" is the shape; "there are many ways to
look at this, and reasonable people differ" is not.

Reporting a real problem is not commentary — say what is wrong plainly.
