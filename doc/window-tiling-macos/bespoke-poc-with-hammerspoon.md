# Bespoke workspace manager — PoC design with Hammerspoon

Design research for replacing FlashSpace with a bespoke Hammerspoon-based workspace manager,
optionally paired with a Swift companion ("Twins") for window tiling.

---

## 1. How FlashSpace controls windows

FlashSpace uses a single macOS API at its core:

```swift
// WorkspaceManager.swift
NSRunningApplication.hide()    // hide entire app process
NSRunningApplication.unhide()  // show entire app process
```

That's it. There is no window-level control. Activating a workspace means:
1. `unhide()` every `NSRunningApplication` assigned to the target workspace.
2. `hide()` every other app currently on that display.

**Consequences:**

- **App-granularity only.** Every window of an app moves together. You cannot show
  one Zen Browser window and hide another. "Move this window to another workspace"
  doesn't exist — only "move this app's home workspace" via `assignAppShortcut`.

- **All workspaces share one native Space.** FlashSpace does not touch Mission Control.
  `hs.spaces.focusedSpace()` and `hs.spaces.windowSpaces()` see the same space ID
  regardless of which FlashSpace workspace is active. Any guard based on those APIs
  is a no-op. The correct visibility check is `hs.window:isVisible()` (Hammerspoon)
  or `NSRunningApplication.isHidden` (Swift).

- **Multi-monitor display detection is fragile.** FlashSpace identifies monitors by
  `NSScreen.localizedName` only — no EDID or serial fallback. Rename, unplug, or swap
  a monitor and workspaces silently fail to activate. There is no recovery path.

- **App positions drift after sleep.** Because all windows sit on the same native Space,
  macOS window restoration after sleep can drift positions. FlashSpace has no layout
  state to restore from.

- **The wipe-on-decode bug.** `ProfilesRepository.loadFromDisk()` uses `try?` when
  deserialising `profiles.toml`. Any decode failure — a missing required field, a bad
  value — returns `nil`, indistinguishable from a missing file. The code then calls
  `createDefaultProfile()` and **overwrites the file**, destroying all workspace config.
  This is not hypothetical: adding a `[[profiles.workspaces]]` block with no `apps`
  entries triggers it because `apps: [MacApp]` is non-optional.

---

## 2. How AeroSpace works differently

AeroSpace is an i3-inspired tiling window manager. Its workspace emulation is
mechanically different from FlashSpace's:

- **Windows are moved off-screen, not hidden.** Inactive workspace windows are
  repositioned to an off-screen corner (bottom-right/left). macOS won't let you move
  a window entirely off-screen, so a thin pixel strip remains visible — but the window
  is functionally invisible to the user. Switching workspaces repositions windows back
  into the display area.

- **AeroSpace owns window layout.** It actively resizes and repositions windows according
  to a tree-based tiling model (containers, splits, floating). FlashSpace has no layout
  concept — it only controls visibility.

- **Native Spaces are not used.** AeroSpace explicitly does not use macOS Spaces, for
  the same reasons FlashSpace doesn't: animations, instability, SIP requirements for CLI
  control, phantom spaces created by full-screen apps, no stable identity.

- **`hs.spaces` is equally useless against AeroSpace.** Same underlying reason — the
  CoreGraphics spaces layer isn't involved.

**What AeroSpace is good at:**
- Per-window membership (a window, not an app, belongs to a workspace)
- Automatic tiling via layout rules
- `move-node-to-workspace` — move this window, not its whole app
- CLI-first, plain-text config

**What AeroSpace can't do (without significant config):**
- Named, semantically stable workspaces that survive monitor changes gracefully
- Per-workspace app-launch rules (`openAppsOnActivation`)
- Composing with a separate tiling tool — it is the tiling tool

**The key difference in one sentence:** AeroSpace owns window *layout*; FlashSpace
owns app *visibility*. They are solving different problems with the same workspace-naming
metaphor.

### Why the off-screen model causes flicker

The move-off-screen mechanism explains the visual instability often seen in AeroSpace
(flickering, brief wrong-display appearances, app redraws on workspace switch):

- **Move = repaint.** Every workspace switch repositions N windows by thousands of
  pixels. Each move triggers AX notifications, window geometry changes, and app
  re-layouts. Apps that respond to resize/move events (browsers, Electron, Slack)
  redraw on every switch.
- **Tiling re-applies on focus.** With tiling rules active, AeroSpace recomputes layout
  when a workspace becomes active — more moves, more repaints.
- **Off-screen ≠ hidden from the app.** The app still thinks it is visible and on-screen.
  Background work (animations, polling, WebSocket updates) keeps firing and occasionally
  paints.
- **Multi-monitor crosstalk.** When a window transitions between the off-screen stash
  and a real display, it sometimes briefly appears on the wrong display mid-move.
- **Mission Control weirdness.** Off-screen windows pile in a corner of Mission Control,
  and surfacing them via Mission Control can yank them back into unexpected places.

The hide/unhide model sidesteps all of this. A hidden app is off the compositor's
hot path entirely — no geometry changes, no AX events, no repaint. The tradeoff is
app-granularity only, which is the constraint we are choosing to live with.

### The multi-display asymmetry

The app-granularity tradeoff bites hardest on multi-monitor. `NSRunningApplication.hide()`
is **process-global** — it hides every window of an app across every display at once.
There is no "hide this app on display A only".

AeroSpace avoids this because it operates on windows, not apps. Chrome's CENTRE window
can stay at real coordinates (visible) while Chrome's LAPTOP window is parked off-screen
(stashed). Chrome the *process* is never touched. Each window has its own frame; each
window can be stashed or restored independently.

FlashSpace hits the wall here. Slack assigned to "Meet" on CENTRE means activating "Meet"
calls `Slack.unhide()` — which unhides Slack *globally*, including any Slack window on
LAPTOP you did not want to disturb. Switching LAPTOP's workspace away calls `Slack.hide()`
— which hides the CENTRE Slack window you just made visible. This is not a bug in
FlashSpace; it is a mechanical consequence of using process-level hide.

FlashSpace's implicit rule is therefore **"one app belongs to at most one display's
workspace set"**. Violate it and visibility fights itself.

### Visualising the constraint

Two displays can each have their own active workspace simultaneously. The per-display
axis is fine. The constraint is that **an app** (not a display) is the unit of
hide/unhide — so an app can only belong to one workspace across the whole system.

**Physical desk layout (desk-A).** LEFT and CENTRE sit side by side; LAPTOP is below
LEFT. A workspace can contain multiple apps; how those apps are laid out depends on
the layout mode for that workspace (see "Within-workspace layout" below):

```
┌──────────────────────────────┐  ┌────────────────────────────────────────┐
│  LEFT (DELL U2419HC)         │  │  CENTRE (DELL U2723QE)                 │
│  Active: "Reference"         │  │  Active: "Coding"                      │
│  (stacked)                   │  │  (tiled 75/25 pair)                    │
│                              │  │                                        │
│  ┌┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┐        │  │  ┌────────────────────┐  ┌───────────┐ │
│  ┊   Notion         ┊        │  │  │                    │  │           │ │
│  ┊   (behind)       ┊        │  │  │      Ghostty       │  │   Marq    │ │
│  ┊  ┌──────────────────┐     │  │  │       75%          │  │    25%    │ │
│  ┊  │                  │     │  │  │                    │  │           │ │
│  └┈┈│   Zen Browser    │     │  │  │                    │  │           │ │
│     │   (front)        │     │  │  │                    │  │           │ │
│     │                  │     │  │  │                    │  │           │ │
│     └──────────────────┘     │  │  └────────────────────┘  └───────────┘ │
└──────────────────────────────┘  └────────────────────────────────────────┘
┌──────────────────────────────┐
│  LAPTOP (Retina XDR)         │
│  Active: "Chat"              │
│  (fill)                      │
│                              │
│  ┌──────────────────────────┐│
│  │                          ││
│  │          Slack           ││
│  │      (fills screen)      ││
│  │                          ││
│  └──────────────────────────┘│
└──────────────────────────────┘

Assignments:
  LEFT:    "Reference"   → Notion, Zen Browser     (stacked)
           "Diagramming" → Miro, Excalidraw        (stacked)
  CENTRE:  "Coding"      → Ghostty, Marq           (tiled 75/25)
           "Meetings"    → Zoom, FaceTime          (stacked)
  LAPTOP:  "Chat"        → Slack                   (fill)

Each app appears in exactly one workspace. All three displays run independently
— CENTRE switching from "Coding" to "Meetings" does not touch LEFT or LAPTOP.
```

### Within-workspace layout

A workspace can hold more than one app. How those apps are arranged on the display
is controlled by a small set of layout modes, applied after `activateSpace()`
finishes unhiding:

| Mode | Behaviour | Example |
|---|---|---|
| **Floating** (default) | Window keeps whatever size and position macOS last gave it. No tiling code runs. Just appears where the OS put it | Anything not explicitly configured |
| **Fill** (override) | Window is resized to fill the display frame | Slack in "Chat" (explicit) |
| **Stacked** (override) | Multiple apps all visible at their current sizes; macOS z-order handles which is frontmost. CMD-TAB cycles them. Free from the OS — no tiling code runs | Notion + Zen in "Reference" (explicit) |
| **Tiled pair / twinning** (override) | Two specific apps get a declared split ratio (75/25, 60/40, etc.) on the same display. Only fires when both members of the pair are present and visible on that display | Ghostty + Marq (75/25) in "Coding" |

**Defaults and overrides:**

- **The default is floating.** An app whose bundle ID has no layout entry in config
  appears at whatever size/position macOS remembers for it. This is the "do nothing"
  case and it is the majority of apps.
- **Fill, stacked, and twinning are all explicit overrides.** They have to be recorded
  per-app (or per-pair, for twinning) in config. There is no automatic inference from
  "how many apps are visible" — if you want Slack to fill its display when active, say
  so in the config.
- **Twinning is conditional.** A pair rule fires only when both members are present
  and visible in the active workspace on the same display. If Marq is not in the
  active workspace, Ghostty falls back to whatever its own layout mode is (fill,
  stacked, or floating).

This keeps the tiling engine small. Only apps that declare an override run through
layout logic; everything else is left alone by macOS.

**Pairing pseudocode:**

```lua
local appLayouts = {
  -- only apps with explicit overrides appear here
  ["com.slack.Slack"]       = "fill",
  ["com.mitchellh.ghostty"] = "fill",    -- falls back to this if no pair matches
  ["com.jimbarritt.marq"]   = "fill",
}

local pairings = {
  -- pairKey is sorted bundleIds joined by "+"
  ["com.mitchellh.ghostty+com.jimbarritt.marq"] = {
    left = "com.mitchellh.ghostty", ratio = 0.75,
  },
  ["app.zen-browser.zen+com.mitchellh.ghostty"] = {
    left = "com.mitchellh.ghostty", ratio = 0.60,
  },
}

local function applyLayout(spaceName, display)
  local visible = visibleAppsOn(display)

  -- Pair rule wins over per-app mode if both members are visible together
  if #visible == 2 then
    local rule = pairings[pairKeyFor(visible[1], visible[2])]
    if rule then
      tilePair(visible, rule, display)
      return
    end
  end

  -- Otherwise apply each app's own declared layout mode, if any
  for _, app in ipairs(visible) do
    local mode = appLayouts[app:bundleID()]
    if mode == "fill" then
      fill(app, display)
    -- "stacked" and missing entries: no-op (OS handles z-order / floating)
    end
  end
end
```

**Broken configuration** — Slack double-assigned to LAPTOP's "Chat" *and* CENTRE's
"Meetings":

```
Initial state:
  LAPTOP active = "Chat"       → Slack visible
  CENTRE active = "Meetings"   → Zoom, FaceTime, Slack visible
  Slack process: unhidden (visible on both displays)

User switches CENTRE: "Meetings" → "Coding"
  activateSpace("Coding", CENTRE):
    unhide(Ghostty), unhide(Marq)
    hide(Zoom), hide(FaceTime), hide(Slack)   ← global hide
                                               ✗ Slack vanishes from LAPTOP too
                                                 even though "Chat" is still active

The two displays' workspace activations fight each other via the shared
process-level hide state. There is no way to resolve this with hide/unhide
alone — it would require window-level control (Option B or C below).
```

### Stable APIs for controlling window geometry

All window-geometry work in the PoC can be done through public, stable macOS APIs —
no private frameworks, no SIP changes. The Accessibility API (AXUIElement) exposes
every window's standard controls as addressable elements, and Hammerspoon wraps the
common ones directly.

The useful set:

| Hammerspoon call | Underlying API | Effect |
|---|---|---|
| `win:setFrame(rect)` | `AXPosition` + `AXSize` | Arbitrary geometry. Precise control, used for tiled pairs. |
| `win:toggleZoom()` | `AXPress` on `AXZoomButton` | Green-button click. Delegates to the app's own "maximise me" behaviour. |
| `win:toggleFullScreen()` | `AXPress` on `AXFullScreenButton` | True full-screen — creates a dedicated native Space. Do not use inside the PoC. |
| `win:minimize()` / `unminimize()` | AX minimize button | Classic minimise-to-Dock. |
| `win:maximize()` | `setFrame()` to screen frame | Convenience wrapper for "fill this screen". |

Two practical approaches for each layout mode:

- **Fill** → `win:setFrame(display:frame())`. Fall back to `toggleZoom()` only if a
  specific app resists resize (some Electron apps fight `setFrame`).
- **Tiled pair** → `setFrame()` with computed rects. Most predictable for enforcing
  a specific ratio.
- **Stacked** / **Floating** → no calls needed. macOS handles z-order and stored
  window position natively.

**Sequoia's Window Tiling feature** (separate from full-screen) also exposes stable
triggers: the keyboard shortcuts `Fn+Ctrl+F` (fill), `Fn+Ctrl+←` (left half), etc.
can be synthesised with `hs.eventtap.keyStroke`, and the new "Move & Resize" menu
items under the Window menu are pressable via AX. These are alternatives to
`setFrame` if we want to delegate to macOS's built-in tiling rather than compute
rects ourselves — useful for simple halves and quarters, less so for custom ratios
like 75/25.

**AppleScript** via System Events is another stable path (clicks the green button or
invokes Window menu items), but it is slower and less ergonomic than AX. No reason
to reach for it given Hammerspoon's direct AX bindings.

Takeaway: everything we need for layout lives in public, permissioned APIs. No
private SkyLight calls, no SIP disable, no scripting addition injection. The
constraint on the PoC is the multi-display hide asymmetry discussed above — not
the geometry layer.

### Four options for our PoC

| Option | Mechanism | Cost | Benefit |
|---|---|---|---|
| **A. Pure FlashSpace model** | hide/unhide only; each app assigned to one display | "Slack on both monitors" workflow impossible | Trivially simple; one mechanism |
| **B. Hybrid (allowlist)** | hide/unhide by default; `setFrame` stash for a small allowlist of multi-display apps | Two code paths; allowlist to maintain; flicker on allowlist apps only | Keeps FlashSpace cleanliness for 95% of apps, fixes the specific pain |
| **C. Pure AeroSpace model** | every window tracked and stashed via `setFrame` | Full flicker, Mission Control clutter, large state | Maximum flexibility; window-level membership |
| **D. Off-all-screens variant of C** | move windows outside the bounding rect of all displays (no pixel strip) | Same as C but visually cleaner | Slightly better Mission Control behaviour |

**Recommended path:** start with Option A. Document the "one app, one display"
constraint. Live with it for a week and note which apps actually break the workflow.
If the list is small (2–3 apps), graduate to Option B with an explicit allowlist.
If the allowlist grows toward "all apps", the honest answer is to stop and reconsider
— at that point AeroSpace's model is paying for itself and reinventing it is waste.

---

## 3. Core design dimensions

Before designing anything, fix the three axes:

### Apps
Running processes (`NSRunningApplication`). An app may have multiple windows. macOS
`hide()`/`unhide()` operates at the app level, not the window level. Bundle identifier
is the stable identity — display names are unreliable.

### Spaces (our concept)
Named virtual workspaces. Not macOS Spaces. A space is a label plus a set of assigned
apps plus optional tiling rules. A space has no physical location until it is activated
on a display. Identity is the name string — must survive restarts and reloads.

### Displays
Physical monitors. Each display independently shows one active space at a time.
`NSScreen.localizedName` is the only identifier available without private APIs, which
makes display identity fragile. Two displays can show different spaces simultaneously.

### Display profiles and role resolution

The config must not name specific displays directly — "LEFT" and "CENTRE" should be
*roles* the workspace manager resolves against the current hardware, not hard-coded
references. Three reasons:

1. **Fallback when displays are missing.** Laptop-only mode (no external monitors
   attached) should not silently drop half the workspaces. Roles that can't be
   resolved fall back to the primary display so every space is still reachable.
2. **Plug/unplug reactivity.** `hs.screen.watcher` fires on display changes. On each
   event, we re-resolve the active profile and rebuild the role→screen map.
3. **Stable identity.** Names drift (two "DELL U2723QE" monitors both called the same
   thing). EDID UUIDs are stable for external displays; internal displays lack EDID
   but have a reliable `isBuiltin()` test.

**Profile matching.** A profile is a set of display roles keyed to EDID UUIDs (or
`builtin` for the laptop panel). At load time and on every screen-watcher event:

1. Read `hs.screen.allScreens()`.
2. For each screen, compute a key (`getUUID()` for externals, `builtin` for the
   internal panel).
3. Match the set of keys against named profiles from config. First exact match wins.
4. If no profile matches, fall back to a synthetic "unknown" profile with every
   role pointing at the primary screen.

**Role resolution.** `resolveDisplay(role)` returns an `hs.screen` object or the
primary screen if the role isn't defined in the active profile. Hotkey handlers
and `activateSpace` take a role name, not a screen — the resolver does the mapping.

```
profiles:
  desk-a:
    LEFT   = 10AC7A41-0000-0000-141E-010380351E78   (U2419HC)
    CENTRE = 10AC7942-0000-0000-3021-0104B53C2278   (U2723QE)
    LAPTOP = builtin
  laptop-only:
    LAPTOP = builtin
    LEFT   = builtin    ← fallback so "Reference" still lands somewhere
    CENTRE = builtin
```

With this, `activateSpace("Reference", "LEFT")` works in every profile; on
laptop-only it just lands on the built-in display alongside whatever else is
there.

### Key operations

| Operation | What it means |
|---|---|
| Assign app to space | Record `bundleId → spaceName` mapping in config |
| Activate space on display | Unhide assigned apps, hide all others on that display |
| Move app between spaces | Change `bundleId` mapping, re-activate current space |
| Tile windows in a space | Resize/reposition windows of visible apps on a display |
| Show space overlay | Display a navigable list of spaces with labels |

---

## 4. What we want

Requirements, stated plainly:

- **Named spaces** — "Coding", "Browsing", "Meet", not "Desktop 3". Names are how you
  reason about context. They appear in the menu bar.

- **Per-display menu bar** — each display shows the active space name. Not one shared
  label. Instant update on switch.

- **Space control overlay** — label-based navigation (0–9, a–z). Press the label
  character to jump. No mental translation from position index to hotkey. This is the
  FlashSpace space control PR problem: it shows position ordinal, not the shortcut key.

- **App assignment** — apps are assigned to spaces in config. On activation: assigned
  apps appear, everything else hides. Finder is always floating (never hidden).

- **Multi-monitor independent switching** — activating a space on Display A does not
  disturb Display B. Each display has its own active space.

- **Tiling rules per space (Twins concept)** — declarative rules: "when Coding is active
  on the left monitor, Ghostty takes 60% left, Marq takes 40% right". Rules fire after
  workspace switch completes.

- **Instant, reliable switching** — no animation, no drift, no TOML wipe risk. The
  mechanism must be so simple it's hard to break.

- **Frictionless** — no per-window positioning hotkeys. You switch a space, apps appear
  in the right layout automatically.

---

## 5. Why Hammerspoon as the foundation

**Already proven for tiling.** Use Case 1 (Ghostty + Marq 60/40) works. The event
model, linked resize, and workspace-change hook integration are all validated.

**Same primitives as FlashSpace.** `hs.application:hide()` and `hs.application:unhide()`
are Lua wrappers around the same `NSRunningApplication` calls FlashSpace uses. We are
not giving anything up by not using FlashSpace.

**Window-level control.** `hs.window` exposes `setFrame()`, `frame()`, `screen()`,
`isVisible()` — actual window geometry. FlashSpace has none of this. Tiling is a first-
class capability, not bolted on via a separate app communicating over a socket.

**Config in TOML, not Lua tables.** Workspace, profile, and app-layout config lives in
`~/.hammerspoon/config.toml` — separate from the code. Rationale: the config is also
the future home of custom app-launch and window rules, and TOML keeps non-Lua-literate
editing (and future tooling) viable. We avoid FlashSpace's wipe-on-decode footgun by
loading strictly: a parse error or missing required field fails loud and leaves the
existing file alone; we never rewrite the config from code. Requires vendoring a pure-Lua
TOML parser (`home/hammerspoon/lib/toml.lua`) — no C extension or Hammerspoon build
change.

**No signing issues.** FlashSpace has notarization notes in its README. Hammerspoon
is open, well-maintained, and has no app-signing friction for custom builds.

**Fast iteration.** Edit `init.lua`, press Cmd+R in the console (or watch for file
changes), changes apply immediately. Slower to ship than the FlashSpace GUI but faster
to prototype than a Swift app.

**Lua is sufficient.** The workspace management logic — a table of `spaceName → {bundleIds}`,
per-display active-space tracking, hide/show loops — is twenty lines of Lua. There is
no algorithmic complexity that requires Swift.

---

## 6. What Hammerspoon cannot easily do

**Polished per-display menu bar.** `hs.menubar` creates a single menu bar item. For
a label that appears independently per display (like FlashSpace's `menuBarTitleTemplate`),
you need a native SwiftUI `NSStatusItem` tied to each screen. Hammerspoon can show a
single combined label but cannot anchor separate items to separate displays.

**Native-feeling space control overlay.** `hs.chooser` gives a searchable list. It
works but looks like a developer tool, not a product. A proper overlay — full-screen,
character-indexed, live preview of space contents — requires AppKit/SwiftUI with a
borderless `NSWindow` per display.

**Screen Recording permission (for display screenshots).** FlashSpace's Space Control
shows screenshots of each workspace. Hammerspoon can in principle take screenshots but
the permission prompt and entitlement story is messier than a sandboxed Swift app.

These three gaps are the entire scope for a Swift companion. Everything else stays in
Hammerspoon.

---

## 7. Proposed PoC plan

Build incrementally. Validate each step before moving to the next. Do not design the
Swift companion until steps 1–4 are proven reliable.

### Step 1 — Basic space management in Hammerspoon

Implement the minimal workspace manager. Scope:

1. Load `~/.hammerspoon/config.toml` via a vendored TOML parser.
2. Detect the active display profile and build a `role → hs.screen` map.
3. Watch for screen changes (`hs.screen.watcher`) and rebuild the role map on events.
4. Implement `activateSpace(spaceName, role)` — unhide assigned apps, hide others,
   Finder exempt.
5. Bind 2–3 hotkeys to prove the hide/unhide cycle is reliable.

Config shape (TOML):

```toml
[profiles.desk-a]
LEFT   = "10AC7A41-0000-0000-141E-010380351E78"
CENTRE = "10AC7942-0000-0000-3021-0104B53C2278"
LAPTOP = "builtin"

[profiles.laptop-only]
LAPTOP = "builtin"

[spaces.Reference]
role = "LEFT"
apps = ["notion.id", "app.zen-browser.zen"]

[spaces.Coding]
role = "CENTRE"
apps = ["com.mitchellh.ghostty", "com.jimbarritt.marq"]

[spaces.Chat]
role = "LAPTOP"
apps = ["com.tinyspeck.slackmacgap"]
```

Pseudocode:

```lua
local config      = toml.decode(readFile("~/.hammerspoon/config.toml"))
local activeSpace = {}           -- role → spaceName
local roleToScreen = {}          -- role → hs.screen (rebuilt on screen events)

local function rebuildProfile()
  roleToScreen = resolveProfile(config.profiles, hs.screen.allScreens())
end

local function activateSpace(spaceName, role)
  local space = config.spaces[spaceName]
  if not space then return end

  -- Unhide assigned apps
  for _, bundleId in ipairs(space.apps) do
    local app = hs.application.get(bundleId); if app then app:unhide() end
  end

  -- Hide every app that belongs to *another* space on this role's display
  for name, s in pairs(config.spaces) do
    if name ~= spaceName and s.role == role then
      for _, bundleId in ipairs(s.apps) do
        if bundleId ~= "com.apple.finder" then
          local app = hs.application.get(bundleId); if app then app:hide() end
        end
      end
    end
  end

  activeSpace[role] = spaceName
end

rebuildProfile()
hs.screen.watcher.new(rebuildProfile):start()
```

No tiling yet. Just validate that the hide/show cycle is reliable and the role
fallback works when an external display is unplugged.

### Step 2 — Multi-monitor: independent activation per display

Each display gets its own active space. Activating a space on Display A should not
disturb Display B.

The key constraint: hide/unhide is app-process-global — hiding an app hides it on
all displays simultaneously. This is the same constraint FlashSpace lives with.

For multi-monitor to work correctly, app assignments must be partitioned by display:
an app should appear in at most one display's active space at a time, or the model
breaks. Apps without a display assignment are "floating" (always visible, like Finder).

Test: activate "Coding" on the built-in, "Meet" on the external. Switch "Coding" to
"Browsing" — verify the external display is undisturbed.

### Step 3 — Hotkeys for space switching

Bind `cmd+opt+1..9` and `cmd+opt+a..z` to `activateSpace()`. Use `hs.hotkey.bind`.

Also implement `cmd+opt+space` to open a `hs.chooser`-based space picker as a
temporary stand-in for the eventual Swift overlay.

### Step 4 — Reliability assessment vs FlashSpace

Specifically check:
- **App-not-running case.** If an assigned app isn't running, unhide is a no-op. Is
  that acceptable? Or should the PoC auto-launch it?
- **Race condition on switch.** FlashSpace's `windowVisible` race was solved by using
  `runScriptAfterWorkspaceChange`. In our own manager, we control sequencing — unhide
  all, then tile. No race.
- **Sleep/wake drift.** After sleep, do app positions hold? FlashSpace has this problem
  because windows stay on one native Space. We have the same problem.
- **Display disconnect.** If the external monitor is unplugged while a space is active
  on it, what happens? Apps that were visible disappear; we need a policy (hide them,
  or move them to the built-in).

### Step 5 — Add tiling rules (the Twins logic)

Once space switching is reliable, layer tiling on top. This is already working for
Use Case 1. The extension is to make tiling rules space-aware:

```lua
local tilingRules = {
  Coding = {
    { apps = {"Ghostty", "Marq"}, display = "Built-in Retina Display",
      ratio = 0.6, order = {"Ghostty", "Marq"} },
  },
}
```

After `activateSpace()` completes, look up tiling rules for the new space and apply
`hs.window:setFrame()` to visible windows. This replaces the FlashSpace hook approach
— we own the sequencing directly.

### Step 6 — Decide on Swift companion scope

If steps 1–5 are working well, assess what is actually missing:
- Is the `hs.chooser` space picker tolerable, or do we need the full overlay?
- Is a single combined menu bar label acceptable, or is per-display essential?

If per-display menu bar and a proper overlay are required, design the Swift companion.
Its scope is strictly UI — it calls back into Hammerspoon via `hs.urlevent` URL handler
to trigger space activation. It does not own any workspace state.

```
Swift companion → "hammerspoon://activate-space?name=Coding&display=Built-in"
Hammerspoon   → activateSpace("Coding", "Built-in Retina Display")
```

State lives entirely in Hammerspoon. The Swift app is a dumb UI shell.

---

## Reference: AeroSpace approach summary

AeroSpace moves windows off-screen (bottom-right corner) rather than hiding app
processes. This gives per-window workspace membership — you can assign individual
windows of the same app to different workspaces. The tradeoff: AeroSpace must track
and manage window positions for every window it knows about, and Mission Control looks
ugly because all off-screen windows pile in the corner.

Our approach (hide/unhide, like FlashSpace) gives simpler state management and instant
switching, at the cost of app-granularity only. For our workflow — stable app sets per
workspace — this is the right tradeoff.

---

## What is not being built

- A full tiling window manager (Yabai does this; not the goal)
- Per-keystroke window positioning hotkeys (Rectangle/Sequoia already cover this)
- A FlashSpace fork (composition is cleaner; the two would diverge)
- Anything requiring SIP disabled or private APIs
