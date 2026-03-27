# Green Tinted Light Theme

Custom light theme for Ghostty and Neovim, designed as the companion to the
`green-tinted` dark theme.

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
| `bin/presentation-mode` | Toggles between dark and light |

## Switching

Run `presentation-mode` to toggle. It updates both Ghostty (via config
overlay + SIGUSR2 reload) and all running Neovim instances (via socket).

## Tweaking

Both themes use the same colour names and hierarchy. To adjust a colour,
change it in both files to keep them in sync. The Ghostty theme maps colours
to the 16 ANSI slots; the Neovim theme applies them to highlight groups
directly.
