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

**No config fragility.** Lua tables are the config. No TOML decoder, no wipe-on-decode,
no format detection heuristic. A syntax error in `init.lua` is caught at load time with
a clear error; it does not silently destroy state.

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

Implement the minimal workspace manager:

```lua
-- Config: which apps belong to which space
local spaces = {
  Coding   = { "com.mitchellh.ghostty", "com.jimbarritt.marq" },
  Browsing = { "app.zen-browser.zen" },
  Meet     = { "us.zoom.xos", "com.apple.facetime" },
}

-- State: which space is active per display
local activeSpace = {}

local function activateSpace(spaceName, displayName)
  local assigned = spaces[spaceName] or {}
  -- Unhide assigned apps
  for _, bundleId in ipairs(assigned) do
    local app = hs.application.get(bundleId)
    if app then app:unhide() end
  end
  -- Hide everything else (that is assigned to *some* space on this display)
  -- Finder is exempt
  for name, apps in pairs(spaces) do
    if name ~= spaceName then
      for _, bundleId in ipairs(apps) do
        local app = hs.application.get(bundleId)
        if app and bundleId ~= "com.apple.finder" then app:hide() end
      end
    end
  end
  activeSpace[displayName] = spaceName
end
```

No tiling yet. Just validate that the hide/show cycle is reliable.

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
