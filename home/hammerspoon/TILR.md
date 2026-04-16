# Tilr

A bespoke workspace manager for macOS, built on [Hammerspoon](https://www.hammerspoon.org).

Tilr manages named spaces (groups of apps) across multiple displays using the hide/unhide model — no native macOS Spaces involved. Switch spaces instantly, tile apps automatically, and move things around without touching a mouse.

---

## Installation

1. **Install Hammerspoon**

   ```
   brew install --cask hammerspoon
   ```

2. **Link the config**

   The dotfiles install process symlinks `home/hammerspoon/` to `~/.hammerspoon/`. If you're setting up manually:

   ```
   ln -sf /path/to/dotfiles/home/hammerspoon ~/.hammerspoon
   ```

3. **Grant Accessibility permission**

   Hammerspoon needs Accessibility access to move and resize windows.
   System Settings → Privacy & Security → Accessibility → enable Hammerspoon.

4. **Load**

   Launch Hammerspoon. Tilr loads automatically from `~/.hammerspoon/init.lua`.
   You should see a "Tilr — home-desk" (or "Tilr — laptop-only") notification.

5. **Config file**

   Edit `~/.hammerspoon/config.toml` to define your spaces, profiles, and hotkeys.
   Reload at any time with `cmd+alt+r`.

---

## Concepts

### Profiles

A profile maps display roles (LEFT, CENTRE, LAPTOP) to physical screens by UUID. Tilr detects which profile matches the currently connected screens on every reload and monitor change.

Built-in profiles:
- **home-desk** — two external monitors + builtin
- **laptop-only** — builtin only

### Spaces

A space is a named group of apps. Only one space is active per display at a time. Switching to a space unhides its apps and hides everything else (except exempt apps: Finder).

Each space has a `role` (LEFT / CENTRE / LAPTOP) — its default display. This can be overridden at runtime for the session.

### Sidebars layout

A space can declare a `sidebars` layout with a `main` app and a split `ratio`. When the space is active:

- The `main` app takes the left portion at `ratio`
- Every other visible app in the space shares the right portion (stacked, use alt-tab to switch between them)
- Drag either side to resize — the other side fills the remaining space
- The adjusted ratio is remembered for the session

### Session overrides

Two kinds of runtime override, both cleared on Hammerspoon reload:

- **App move** (`opt+shift+N`) — moves the focused app into a different space for this session
- **Space move** (`cmd+shift+1/2/3`) — moves a space to a different display for this session

---

## Hotkeys

| Key | Action |
|-----|--------|
| `cmd+alt+0` | Switch to Scratch |
| `cmd+alt+1` | Switch to Coding |
| `cmd+alt+2` | Switch to Reference |
| `cmd+alt+3` | Switch to Meetings |
| `cmd+alt+4` | Switch to Schedule |
| `cmd+alt+a` | Switch to Slack |
| `cmd+alt+space` | Toggle status overlay |
| `cmd+alt+r` | Reload config |
| `cmd+alt+z` | Dump diagnostics to Hammerspoon console |
| `opt+shift+0..4,a` | Move focused app → that space |
| `cmd+shift+1` | Move active space → LEFT display |
| `cmd+shift+2` | Move active space → CENTRE display |
| `cmd+shift+3` | Move active space → LAPTOP display |

---

## config.toml reference

### Profiles

Profiles map display roles to physical screens. The value is either `"builtin"` for the internal panel or the macOS-assigned UUID for an external monitor.

**Laptop-only (single screen):**
```toml
[profiles.laptop-only]
LEFT   = "builtin"
CENTRE = "builtin"
LAPTOP = "builtin"
# All roles collapse to the same physical screen
```

**Two-monitor home desk:**
```toml
[profiles.home-desk]
LEFT   = "6203E2B2-F23E-4A39-A42E-0DA93E56D196"   # DELL U2419HC (left)
CENTRE = "C8F12309-1B94-4BC8-86DB-9F61C1F15087"   # DELL U2723QE (centre)
LAPTOP = "builtin"                                 # Retina XDR
```

Find UUIDs in the Hammerspoon console:
```lua
for _, s in ipairs(hs.screen.allScreens()) do print(s:name(), s:getUUID()) end
```

Profile matching uses the set of unique UUID values, so `laptop-only` matches when exactly one screen (builtin) is present.

### Spaces

```toml
# Space with a sidebars layout
[spaces.Coding]
role = "CENTRE"
apps = ["com.mitchellh.ghostty", "com.jimbarritt.marq"]

[spaces.Coding.layout]
type  = "sidebars"
main  = "com.mitchellh.ghostty"
ratio = 0.70

# Space with no layout (apps just appear wherever they were last)
[spaces.Meetings]
role = "CENTRE"
apps = [
  "us.zoom.xos",
  "com.apple.facetime",
  "com.microsoft.teams2",
]
```

`apps` is the list of bundle IDs managed by this space. Apps not in any space's list are never hidden by Tilr.

#### Finding bundle IDs

```lua
-- Single app
hs.application.get("Safari"):bundleID()

-- All running apps
for _, app in ipairs(hs.application.runningApplications()) do
  print(app:name(), app:bundleID())
end
```

#### The Scratch pattern

Scratch is the escape-hatch space — apps that don't belong anywhere else. The Hammerspoon console lives here. No layout block, role is CENTRE (or LAPTOP if you want it on the small screen):

```toml
[spaces.Scratch]
role = "CENTRE"
apps = ["org.hammerspoon.Hammerspoon"]
```

### Defaults

```toml
[defaults.home-desk]
CENTRE = "Coding"
LAPTOP = "Slack"

[defaults.laptop-only]
LAPTOP = "Coding"
```

Applied at startup and whenever the display profile changes. Omit a role to leave that display empty on profile switch.

### Hotkeys

```toml
[[hotkeys]]
mods  = "cmd alt"
key   = "1"
space = "Coding"
```

---

## Spaces

| Space | Display (default) | Apps | Hotkey |
|-------|-------------------|------|--------|
| Scratch | CENTRE | — | `cmd+alt+0` |
| Coding | CENTRE | Ghostty, Marq | `cmd+alt+1` |
| Reference | CENTRE | Notion, Zen, Chrome | `cmd+alt+2` |
| Meetings | CENTRE | Zoom, FaceTime, Teams, Meet | `cmd+alt+3` |
| Schedule | CENTRE | Google Calendar | `cmd+alt+4` |
| Slack | LAPTOP | Slack | `cmd+alt+a` |

---

## Future ideas

- **Status overlay enhancements** — show current display assignment per space, not just active space; make the overlay a proper picker
- **Reset to config defaults** — a hotkey to clear session overrides without a full reload
- **Multiple windows per app** — currently uses `app:mainWindow()`; could support tiling secondary windows
- **Per-space ratio persistence** — optionally write adjusted ratios back to config.toml so they survive reloads
- **More layout types** — e.g. `stacked` (vertical split), `thirds`, or fully custom grid
- **Homebrew distribution** — package Tilr as a Hammerspoon Spoon, installable via `brew install --cask jimbarritt/tap/tilr`. The cask can declare `depends_on cask: "hammerspoon"` so Homebrew installs Hammerspoon automatically if not present. A `caveats` block in the formula prints the two-line setup instructions after install:
  ```
  Add to ~/.hammerspoon/init.lua:
    hs.loadSpoon("Tilr")
    spoon.Tilr:start()
  ```
- **Status bar indicator** — show active space name in the macOS menu bar
- **Space history** — `cmd+alt+tab` to cycle back through recently visited spaces
