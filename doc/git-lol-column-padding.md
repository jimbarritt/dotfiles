# git lol column padding

`git lol` printed a blank line under most commits:

```
 98a2a4a  AutoBot     26-07-28 19:34 Welcome, candidate! Have fun :-) [skip ci]

 1a84530  Jim Barritt 26-07-28 21:21 First push to check the pipeline is working

 165fc96  Jim Barritt 26-07-29 18:55 Adding adr dir

```

The blank lines are the tail of each commit line wrapping, not extra commits.

## Cause

The original alias in `home/gitconfig`:

```
lol = !git log --reverse --format='%C(yellow) %h %Creset %<(10)%an %<(12)%cd %<(80,trunc)%s' --date=format:'%y-%m-%d %H:%M' -15
```

`%<(N)` sets a **minimum** field width, padding with trailing spaces when the
content is shorter. `%<(80,trunc)%s` therefore emits 80 columns for every
subject, however short, and those trailing spaces are real characters in the
output.

Measured with colour stripped, every line was exactly 117 columns:

| Segment | Pad | Actual | Over |
|---------|-----|--------|------|
| ` %h ` plus separators | — | 11 | — |
| `%<(10)%an` | 10 | `Jim Barritt` = 11 | +1 |
| `%<(12)%cd` | 12 | `26-07-28 19:34` = 14 | +2 |
| `%<(80,trunc)%s` | 80 | padded to 80 | — |
| **Total** | | **117** | |

`%<()` truncates on overflow only when passed `trunc`. The subject had it; the
author and date did not, so both silently ran past their columns — the
alignment the pads were there to guarantee was not guaranteed either. The date
overflowed on every commit, because `%y-%m-%d %H:%M` is always 14 characters
against a 12-column pad.

In a terminal narrower than 117 columns each line occupies two visual rows. At
80 columns the second row is the remaining 37, and since the subject starts at
column 38 that remainder is subject characters 44 onwards. A subject of 43
characters or fewer leaves the second row as nothing but padding.

This is why it looked intermittent — in this repo:

- `Adding instruction to copy rm commands to the clipboard` (55 chars) wrapped with visible text
- `Update the planning format` (26 chars) produced a blank row

Every subject in the backendify repo was 43 characters or fewer, so every line
there got a blank one.

## Fix

```
lol = !git log --reverse --format='%C(yellow) %h %Creset %<(12,trunc)%an %cd %s' --date=format:'%y-%m-%d %H:%M' -15
```

- The subject is the last field, so it needs no padding — nothing follows it to
  align against. Removing `%<(80,trunc)` is what stops the wrapping; lines are
  now as wide as their content.
- `%<(12,trunc)%an` truncates authors longer than 12 characters instead of
  pushing the following columns out of line.
- `%cd` needs no pad: `%y-%m-%d %H:%M` is fixed at 14 characters.

Long subjects still wrap in a narrow terminal, but only when there is real text
to wrap.

## Rule

When aligning `git log` columns with `%<()`:

- Never pad the final field.
- Pair every pad with `trunc`, or the field overflows and breaks the alignment.
- Size pads against the widest real value — check fixed-width fields such as
  `--date=format:` strings by counting the format's output, not the format.
