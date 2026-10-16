# M-DOT-GIT-01: task notes

Imported from `doc/planning/plan.md` in the dotfiles repo, heading "Delta: Diff Viewer Readability". The bullets are as written in the plan.

git-delta rendering on the green-tinted themes. Research: `doc/gitconfig.md`.

### T-01: Retune diff colours for both themes

- ✓ DONE — Root cause: `home/gitconfig` hardcoded dark-only delta colours, and `bin/presentation-mode` switched Ghostty, tmux and Neovim but never delta
- ✓ DONE — Light overlay `~/.gitconfig-presentation`, written and removed by `bin/presentation-mode`, included from `home/gitconfig` after the `[delta]` block and before the `~/.gitconfig-work` include
- ✓ DONE — Dark colours retuned. The old backgrounds sat within 1-4 luminance points of the terminal background `#0a1f0a`, so whole added and removed lines were invisible
- ✓ DONE — New dark values: minus 40, minus-emph 65, plus 61, plus-emph 80 against a background of 22. Additions are brighter than deletions because green on green blends
- ✓ DONE — `plus-emph` capped at 80 to stay clear of Monokai Extended's dimmest foreground `#f92672` at 109
- ✓ DONE — Verified by measuring the colours delta emits, in both modes
- TODO — Light mode is verified by number only. Check it on screen at the next `presentation-mode` switch

### T-02: Fix the hunk header

- ✓ DONE — `hunk-header-style = syntax` hides the line number. The number is the first line of the hunk and the text is the context from a line above it, so the pair misleads
- ✓ DONE — Confirmed delta accepts only `file` and `line-number` as special attributes. A range such as `14->21` is not possible
- ✓ DONE — `home/gitattributes` added, linked as `~/.gitattributes` by `do.sh`, with `core.attributesFile` set
- ✓ DONE — `*.md` uses git's built-in `markdown` driver, so the header shows the enclosing heading instead of a body sentence
- ✓ DONE — `*.txt` uses a `[diff "nocontext"]` driver with `xfuncname = "$^"`, which never matches. Delta then drops the header box
- TODO — Decide whether to enable git's built-in drivers for go, rust, python, java and cpp

### T-03: Brighten the gutter line numbers

- ✓ DONE — `line-numbers-zero-style` set to `#5a755a`, luminance 105. The delta default `#444444` at 68 is a neutral grey and reads flat on a green background
- ✓ DONE — `line-numbers-minus-style` and `line-numbers-plus-style` set to bold `#ff7b81` and bold `#7ee787`, luminance 163 and 188
- ✓ DONE — Verified in the escape codes delta emits
- TODO — Document the gutter styles in `doc/gitconfig.md` once the user confirms the values
- TODO — Decide whether the light overlay needs the same gutter treatment
