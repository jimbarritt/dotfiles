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
| `home/claude/themes/green-tinted-light.json` | Claude Code custom theme — light, **in use** (deploys to `~/.claude/themes/`) |
| `home/claude/themes/green-tinted-dark.json` | Claude Code custom theme — dark, **kept but not used** (see below) |
| `bin/presentation-mode` | Toggles between dark and light |

For Claude Code, **dark mode uses the built-in `dark-ansi` theme**; only the
light/presentation case uses a custom theme. The dark custom theme file is kept
as a reference/starting point but isn't the active dark theme — see "Why dark
mode stays on dark-ansi" below for the reason.

## Switching

Run `presentation-mode` to toggle. It updates both Ghostty (via config
overlay + SIGUSR2 reload) and all running Neovim instances (via socket).

## Tweaking

Both themes use the same colour names and hierarchy. To adjust a colour,
change it in both files to keep them in sync. The Ghostty theme maps colours
to the 16 ANSI slots; the Neovim theme applies them to highlight groups
directly.

## Claude Code (light: custom theme; dark: dark-ansi)

The two modes are handled differently, for a hard-won reason:

- **Dark mode → built-in `dark-ansi`.** It already gives a subtle prompt bar
  *and* readable dim text (palette 8 = `#2d5c2d`). A custom dark theme can't
  beat it — see "Why dark mode stays on dark-ansi" below. A
  `green-tinted-dark.json` custom theme is kept as a reference/starting point
  but is **not** the active dark theme.
- **Light mode → custom theme** `custom:green-tinted-light`. The white-background
  conflict is real here and the built-in `light-ansi` can't be fixed (it grabs
  an inverted palette slot), so a custom RGB theme is the only clean solution.

**Files:** `home/claude/themes/*.json` → deploy to `~/.claude/themes/`. The
filename (minus `.json`) is the slug; selecting it stores `theme:
"custom:<slug>"` in `~/.claude.json`. Requires Claude Code ≥ v2.1.118. Switch
via `/theme`. The light theme is the one actually in use.

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

| Token | Controls | Light | Dark |
|-------|----------|-------|------|
| `text` | default foreground text | `#111811` | `#a8d5a8` |
| `background` | surface fill | `#ffffff` | `#0a1f0a` |
| `userMessageBackground` | the submitted-prompt highlight bar | `#eef4ee` | `#122812` |
| `inactive` | dim/secondary text (hints, timestamps) | `#6a766a` | `#6a9e6a` |
| `subtle` | faint borders / de-emphasised text | `#9bb09b` | `#3a5a3a` |
| `claude` | brand accent (spinner, assistant label) | `#1a7a1a` | `#85cc85` |
| `success` | status indicator | `#0a5a0a` | `#66b366` |
| `error` | status indicator | `#8a3a3a` | `#d08a8a` |
| `warning` | status indicator | `#7a6a00` | `#ccbb7a` |
| `promptBorder` | input box border | `#5a8a5a` | `#3a6a3a` |
| `diffAdded` / `diffRemoved` | diff line backgrounds | `#d8ecd8` / `#f2dada` | `#153015` / `#301515` |

**The prompt-bar fix (light):** `userMessageBackground` is its own RGB token, so
on the light theme it sits at `#eef4ee` — a whisper-subtle tint that blends into
the white background — *while* dim text stays readable via the separate
`inactive` token. The old conflict (one ANSI slot being both the bar background
and the dim-text foreground) doesn't exist for the light custom theme.

> **Gotcha — Claude auto-adjusts low-contrast RGB backgrounds (the dark-theme
> trap).** You *cannot* make `userMessageBackground` blend into a dark
> background by setting it close to the background colour. Claude Code applies a
> legibility floor: an RGB value too close to the base background is lightened
> until it's visibly distinct. On the dark theme, `#122812` (almost equal to the
> `#0a1f0a` background) gets boosted to a medium/bright green bar — not the
> invisible bar you'd expect. A wildly different colour (we tested `#ff00ff`)
> passes through untouched, which is how we confirmed the adjustment. This floor
> is why a custom dark theme can't reproduce the ultra-subtle `dark-ansi` bar,
> and why dark mode stays on `dark-ansi` (ANSI slots bypass the RGB
> adjustment — Claude never sees their hex, so it can't "correct" them).

**Activation / discovery:** Claude Code scans `~/.claude/themes/` at startup, so
a newly-added theme file only appears in `/theme` after a fresh session. Once
loaded, *editing* the JSON hot-reloads live — handy for tuning tokens.

> **Deploy gap:** `do.sh` does not sync `home/claude/` to `~/.claude/` (it only
> links `home/{zshrc,gitconfig,…}` and `config/*`). The tracked theme file is a
> mirror; on a fresh machine it must be copied into `~/.claude/themes/`
> manually until a deploy step is added.

The terminal-palette notes below still matter: **dark mode runs on `dark-ansi`,
which uses the ANSI palette** (including palette 8), and other CLI tools use it
in both modes. Only the light custom theme escapes the palette.

### Palette 8 (bright black / secondary text)

> **Scope:** in **light** mode the custom theme handles Claude's prompt bar, so
> palette 8 there only serves *other* tools' dim text. In **dark** mode Claude
> runs on `dark-ansi`, so palette 8 = `#2d5c2d` is doing real work for *both*
> Claude's prompt bar *and* dim text — and threads both happily because on a
> dark background a medium green is simultaneously a subtle lift and readable
> text (the conflict that's acute on white simply isn't, here).

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

#### The dark theme: palette 8 is set for dim-text readability

The dark `green-tinted` theme's `palette = 8` is **`#2d5c2d`** — a clear lift
above the `#0a1f0a` background so dim grey-green text (e.g. `just test` output:
test names, `N passing (…ms)`) stays readable in the raw terminal.

History worth remembering: we briefly lowered it to `#1d401d` to subdue the
Claude Code prompt bar in `dark-ansi`, but that *also* dimmed test-runner output
into the background (same double-duty conflict). Now that Claude Code uses the
**custom dark theme** (where `userMessageBackground` is an independent RGB
token), palette 8 no longer needs to compromise for the bar — so it's restored
to `#2d5c2d` purely for dim-text readability. Keep Claude on
`custom:green-tinted-dark` (not `dark-ansi`) to get the subtle bar *and*
readable test output at the same time.
