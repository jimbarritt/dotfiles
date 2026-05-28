# Green Tinted Light Theme

Custom light theme for Ghostty, Neovim, and Claude Code, designed as the
companion to the `green-tinted` dark theme.

## Why not an off-the-shelf theme?

- **GitHub Light High Contrast** — looks different in the terminal than on the
  website. The ANSI palette mapping doesn't reproduce the same feel; colours
  end up too stark and blue-heavy.
- **Catppuccin Latte** — too low contrast, especially in the terminal. Text is
  hard to read.
- Every third-party theme has built-in assumptions about colour hierarchy that
  clash with our cognitive-load design.

## Design principles

The dark `green-tinted` theme uses a brightness hierarchy to reduce cognitive
load during code navigation. This light variant inverts that hierarchy while
keeping the same philosophy:

| Layer              | Dark theme      | Light theme      | Purpose                        |
|--------------------|-----------------|------------------|--------------------------------|
| Structural anchors | Brightest       | Darkest/most sat | Keywords, fn declarations      |
| Navigation         | Medium          | Medium           | Fn calls, types, params        |
| Content / noise    | Dimmest         | Lightest/least sat | Variables, operators, punct  |

### Colour choices

- **Background** `#f4f8f4` — warm off-white with a subtle green tint. Not
  clinical white, not tinted enough to look odd.
- **Foreground** `#1a2b1a` — dark green-black for comfortable reading.
- **Keywords** `#1a7a1a` — dark green, the strongest visual anchor.
- **Function declarations** `#5a6a00` — dark olive, distinct from keywords.
- **Function calls** `#1a6a7a` — teal, clearly different from declarations.
- **Types** `#1a5a8a` — blue-green, navigational.
- **Variables** `#4a6a4a` — muted sage, background noise.
- **Comments** `#8a9e8a` — gray-green, italic, stays out of the way.

## Files

| File | Purpose |
|------|---------|
| `config/ghostty/themes/green-tinted-light` | Ghostty terminal palette |
| `config/nvim/colors/green-tinted-light.lua` | Neovim colorscheme |
| `home/claude/themes/green-tinted-light.json` | Claude Code custom theme (deploys to `~/.claude/themes/`) |
| `bin/presentation-mode` | Toggles between dark and light |

## Switching

Run `presentation-mode` to toggle. It updates both Ghostty (via config
overlay + SIGUSR2 reload) and all running Neovim instances (via socket).

## Tweaking

Both themes use the same colour names and hierarchy. To adjust a colour,
change it in both files to keep them in sync. The Ghostty theme maps colours
to the 16 ANSI slots; the Neovim theme applies them to highlight groups
directly.

## Claude Code (custom theme)

Claude Code is themed **independently of the terminal ANSI palette** via a
custom theme. This is the proper solution and it supersedes the palette-8
workaround below for everything *inside* Claude Code (most importantly the
submitted-prompt highlight bar).

**File:** `home/claude/themes/green-tinted-light.json` → deploys to
`~/.claude/themes/green-tinted-light.json`. The filename (minus `.json`) is the
slug; selecting it stores `theme: "custom:green-tinted-light"` in
`~/.claude.json`. Requires Claude Code ≥ v2.1.118.

```json
{
  "name": "Green Tinted Light",
  "base": "light",
  "overrides": { "token": "#rrggbb", ... }
}
```

- **`base`** is `dark` or `light` — you inherit sensible defaults and override
  only the tokens you care about.
- **Colour formats:** `#rrggbb`, `#rgb`, `rgb(r,g,b)`, `ansi256(n)`, or
  `ansi:<name>` (the 16 ANSI names, e.g. `cyanBright`).
- Because overrides are **RGB values, not ANSI slots**, Claude's colours no
  longer depend on the terminal palette — this is what breaks the palette-8
  double-duty conflict (see below).

Key tokens we override:

| Token | Controls | Value |
|-------|----------|-------|
| `text` | default foreground text | `#111811` |
| `background` | surface fill | `#ffffff` |
| `userMessageBackground` | the submitted-prompt highlight bar | `#eef4ee` |
| `inactive` | dim/secondary text (hints, timestamps) | `#6a766a` |
| `subtle` | faint borders / de-emphasised text | `#9bb09b` |
| `claude` | brand accent (spinner, assistant label) | `#1a7a1a` |
| `success` / `error` / `warning` | status indicators | `#0a5a0a` / `#8a3a3a` / `#7a6a00` |
| `promptBorder` | input box border | `#5a8a5a` |
| `diffAdded` / `diffRemoved` | diff line backgrounds | `#d8ecd8` / `#f2dada` |

**The prompt-bar fix:** `userMessageBackground` is its own RGB token, so it can
sit at `#eef4ee` (a whisper-subtle green that blends into the white background —
effectively no bar) *while* dim text stays readable via the separate `inactive`
token. The old conflict — where one ANSI slot had to be both the bar background
and the dim-text foreground — simply doesn't exist here. Set
`userMessageBackground` to `#ffffff` for a fully invisible bar, or bump it for
more delineation.

**Activation / discovery:** Claude Code scans `~/.claude/themes/` at startup, so
a newly-added theme file only appears in `/theme` after a fresh session. Once
loaded, *editing* the JSON hot-reloads live — handy for tuning tokens.

> **Deploy gap:** `do.sh` does not sync `home/claude/` to `~/.claude/` (it only
> links `home/{zshrc,gitconfig,…}` and `config/*`). The tracked theme file is a
> mirror; on a fresh machine it must be copied into `~/.claude/themes/`
> manually until a deploy step is added.

The terminal-palette notes below are retained for the parts of the stack that
*do* still use the ANSI palette (other CLI tools, and Claude Code itself if you
ever fall back to an `*-ansi` theme).

### Palette 8 (bright black / secondary text)

> **Largely superseded for Claude Code by the custom theme above.** With the
> custom theme active, Claude Code no longer uses palette 8 at all, so this slot
> now only serves *other* tools' dim text (test runners, etc.) and the
> `*-ansi` fallback themes. The history below is kept for that reason.

`palette = 8` ("bright black") does **double duty**, which is why it can't
just be darkened:

1. **As a foreground** for secondary/dim text — wants to be *dark* so it reads
   on the white background:
   - Claude Code's truncated-output hints (`… +6 lines (ctrl+o to expand)`)
   - Claude Code statusline secondary fields
   - Test runner output (test names, timing in parentheses)
2. **As a background** — Claude Code draws the *submitted prompt* in a box
   whose background is ANSI 8, with the prompt text in the foreground colour
   (`#111811`). This wants palette 8 to be *light* so the dark text reads.

These pull in opposite directions, so no single value is perfect — the goal is
a medium grey that clears ~4:1 contrast in *both* roles.

> **Why Claude Code uses ANSI 8 here, and why we stay on `dark-ansi`.**
> Claude Code's theme maps the submitted-prompt highlight
> (`userMessageBackground`) to a palette slot that depends on the chosen theme:
> `dark-ansi` → `ansi:blackBright` (**palette 8**); `light-ansi` →
> `ansi:white` (**palette 7**). Our palette deliberately *inverts* the
> black/white slots (palette 7 = `#111811` dark, palette 0 = `#dce4dc` light)
> so that `dark-ansi`'s bright-white text renders dark on the light bg. That
> inversion is why `light-ansi` looks wrong — it grabs palette 7 (`#111811`)
> for the prompt box and turns it into a near-black bar. So keep Claude Code on
> **`dark-ansi`** with this terminal theme; do *not* switch it to `light-ansi`.
>
> To remove the prompt highlight entirely you'd set palette 8 = `#ffffff` (=
> background), but that re-breaks dim grey foreground text in other tools — the
> double-duty conflict is irreducible on a single slot. We chose readable dim
> text (`#6a766a`) over a hidden bar.

| Value | Dim text on white | Prompt-box text | Verdict |
|-------|-------------------|-----------------|---------|
| `#c4d0c4` | ~1.3:1 — invisible | good | box fine, hints gone |
| `#353b35` | good | ~1.5:1 — unreadable | hints fine, box inverted |
| `#6a766a` | ~4.5:1 | ~4.0:1 | **Current** — both readable |

Don't push darker than `#6a766a`: it re-breaks the submitted-prompt box
(dark-on-dark). If dim text needs more presence, increase contrast on the
white background another way rather than darkening this slot further.

#### The dark theme has the same slot

The same `userMessageBackground` → palette 8 mapping applies to the dark
`green-tinted` theme — there the bar is a *lighter* lift on the near-black
background instead of a darker one. It's tuned to `#1d401d` (down from a
brighter `#2d5c2d`): a gentle lift that keeps the prompt bar subtle while dim
grey-green text stays readable on the `#0a1f0a` background. Same trade-off,
opposite direction — pull it toward the background to hide the bar, away from
it for more readable dim text.
