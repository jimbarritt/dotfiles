# Alternatives to ASD-STE100

Research notes on ASD-STE100 copyright, open-source alternatives, and controlled
language options for software text. Written 2026-08-22.

## The copyright problem

ASD-STE100 Issue 9 (2025-01-15) is owned by the Aerospace, Security and Defence
Industries Association of Europe (ASD). The PDF states:

> "No reproduction or publication of it, in whole or in part, shall be made
> without the written authority of an officer of ASD."

Free reproduction is granted only to eight listed categories: ASD member
associations and their member companies, AIA/AIAC members, ICCAIA members,
their customers, defence ministries of ASD/AIA/AIAC member states, Airlines
for America members, airworthiness authorities, and universities for
educational use.

A personal, public GitHub repo does not fall into any of these categories.
The standard also states: "Unauthorized distribution of ASD-STE100, direct or
through different websites or portals, is strictly prohibited without written
permission from the STEMG."

Result: this repo must not commit or push the ASD-STE100 PDF, an extracted
text copy, or (most likely) a close reproduction of its word list and rule
text. A local copy is kept outside the repo, at `~/Documents/ASD-STE/`.

## Existing open-source projects

Several public GitHub projects package ASD-STE100 as an AI agent skill:

- `AminBlg/SimpleEnglish` — an agent skill that forces LLM output into
  ASD-STE100 Simplified Technical English
- `danyuchn/asd-ste100-skill` — a Claude Code skill that rewrites
  ambiguous agent-facing text using ASD-STE100 rules
- `nuelcyoung/asd-ste100` — a Claude Code skill for writing, rewriting, and
  checking text against ASD-STE100 Issue 9

None of these are an independent alternative. Each repackages ASD-STE100's own
rule text and word list, so each carries the same copyright exposure found
above.

## Caterpillar Technical English

Caterpillar Technical English is a second controlled-language standard, used
by the heavy machinery company. It is proprietary to Caterpillar Inc., not
open, and not a general substitute for STE.

## Controlled language for software text

No controlled natural language standard exists for software text with
STE's structure — a fixed approved-word dictionary paired with numbered
grammar rules. The closest matches are prose style guides, which set
tone and structure rules but do not restrict vocabulary to a fixed list:

- **Microsoft Writing Style Guide** — plain language, active voice,
  accessibility, a user-first tone. Covers documentation, apps, UI text,
  and error messages.
- **Google developer documentation style guide** — editorial rules for
  developer-facing docs: clarity, structure, code samples, and
  internationalisation.

One academic paper proposes a controlled natural language for source-code
documentation ("Verifiable Source Code Documentation in Controlled Natural
Language", arXiv 1311.2702). It did not become a used standard.

## Conclusion

No free, ready-made equivalent to ASD-STE100 exists, for general text or for
software text specifically. A software-specific controlled language —
approved-word list plus numbered rules, written from scratch rather than
copied from ASD's text — would fill the gap and avoid the copyright problem,
since it would express STE's method in original wording rather than ASD's
specific word list and rule text.

## References

- [ASD-STE100 home page](https://www.asd-ste100.org/)
- [ASD-STE100 Issue 9 PDF](https://www.asd-ste100.org/assets/files/ASD-STE100_ISSUE9.pdf)
- [ASD-STE100 downloads page](https://www.asd-ste100.org/STE_downloads.html)
- [ASD-STE100 FAQ](https://www.asd-ste100.org/STE_faq.html)
- [ASD Europe — Simplified Technical English](https://www.asd-europe.org/standards-specifications/simplified-technical-english/)
- [Simplified Technical English — Wikipedia](https://en.wikipedia.org/wiki/Simplified_Technical_English)
- [S1000D — Wikipedia](https://en.wikipedia.org/wiki/S1000D)
- [GitHub — AminBlg/SimpleEnglish](https://github.com/AminBlg/SimpleEnglish)
- [GitHub — danyuchn/asd-ste100-skill](https://github.com/danyuchn/asd-ste100-skill)
- [GitHub — nuelcyoung/asd-ste100](https://github.com/nuelcyoung/asd-ste100)
- [Acrolinx — A Guide to Simplified Technical English](https://www.acrolinx.com/blog/a-guide-to-simplified-technical-english-improving-your-technical-documentation/)
- [Google for Developers — Technical writing resources](https://developers.google.com/tech-writing/resources)
- [Google developer documentation style guide](https://developers.google.com/style)
- [Microsoft Manual of Style — Wikipedia](https://en.wikipedia.org/wiki/Microsoft_Manual_of_Style)
- [Write the Docs — Style Guides](https://www.writethedocs.org/guide/writing/style-guides/)
- [Verifiable Source Code Documentation in Controlled Natural Language — arXiv 1311.2702](https://arxiv.org/pdf/1311.2702)
- [Are Style Guides Controlled Languages? The Case of Koenig & Bauer AG — arXiv 1406.3460](https://arxiv.org/pdf/1406.3460)
