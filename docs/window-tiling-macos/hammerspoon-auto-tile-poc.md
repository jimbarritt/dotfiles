# Hammerspoon Auto-Tile PoC — Agent Brief

## Summary

Hammerspoon is being used as a **behavioural prototype**, not as the long-term home for this functionality. The goal of the PoC is to figure out exactly how auto-tile-beside-Ghostty should feel — the ratio, the linked resize, the pair lifecycle — with the fastest possible iteration loop.

**Long-term plan**: port the validated behaviour into a **new sibling macOS app** (Swift, using `NSAccessibility`) that runs alongside [FlashSpace](https://github.com/wojciech-kulik/FlashSpace). FlashSpace will continue to handle workspace switching; the new app will handle intra-workspace tiling rules. Forking FlashSpace is explicitly not the plan — the two apps should compose.

Once the behaviour is settled in the PoC, the Lua config here becomes the specification for the Swift port: `tile_apps`, `ghostty_ratio`, the pair lifecycle, and the linked-resize guard all translate directly.

## Goal

Create a Hammerspoon config (`~/.hammerspoon/init.lua`) that auto-tiles specific app windows beside Ghostty, with linked resize behaviour. The config doubles as a live spec for the eventual Swift rewrite.

## Context

- Hammerspoon is a macOS automation tool that wraps the accessibility API in Lua — great for fast iteration, not the long-term target
- It runs as a persistent background daemon (menu bar app)
- `hs.window.filter` can subscribe to window events globally (created, visible, moved, destroyed, etc.)
- We are NOT building a full tiling WM — just handling one specific behaviour
- The PoC is **not** checked into the dotfiles repo while it's being iterated; `~/.hammerspoon/init.lua` is a scratchpad
- Eventually paired with FlashSpace for workspace switching, but via composition, not fork

## Design principles

### Declarative, not imperative

Behaviour is defined in a config file; the engine matches rules against the current window set and applies the right layout. Opposite of snap tools (Rectangle, Sequoia Tiling) which are action-based — you press a key or drag, something happens, then the system forgets.

### No keyboard shortcuts for positioning

Explicitly NOT a goal: hotkeys to snap the current window left/right/etc. That model adds cognitive load — you have to remember what's open, which key to press, and in what order. The whole point is: you just open apps, and they arrange themselves correctly.

Keyboard shortcuts for higher-level operations (reload config, disable tiling temporarily, toggle a slot occupant) are fine — the rule is **no per-window positioning hotkeys**.

### Pattern-based rules, not window-specific

Rules describe *classes* of windows ("any Zen Browser window", "Marq when Ghostty is open"), not specific instances. The engine re-evaluates as windows come and go.

## Non-goals

- **Generic snap-to-half / snap-to-quadrant hotkeys** — macOS Sequoia tiling and Rectangle both cover this already
- **Full tiling window manager** — use Yabai if you want that
- **Workspace / space management** — FlashSpace handles that
- **Drag-to-edge snapping** — covered by macOS Sequoia natively
- **Window cycling / focus management** — out of scope
- **Per-keystroke repositioning** — the whole idea is that you don't need to

## Requirements

### 1. Auto-tile on window open

When an app from a configurable list (e.g. `Marq`) opens a new window:
- Find the Ghostty window on the current screen
- Resize Ghostty to the left half of the screen
- Place the new window on the right half of the screen
- Track these two windows as a "tiled pair"

### 2. Linked resize

When either window in a tiled pair is resized:
- The other window should adjust to fill the remaining horizontal space on the screen
- Both windows should remain full height
- Avoid infinite loops — `setFrame` triggers a resize event, so use a guard flag
- The guard flag should be cleared after a short delay (0.5s or so)

### 3. Pair lifecycle

- Only one tiled pair active at a time
- When the right-side window closes (`windowNotVisible`), clear the pair so Ghostty is unmanaged again
- If Ghostty is not running or has no window, skip tiling silently

### 4. Config

- `tile_apps`: a list of app name strings at the top of the file, easy to extend
- Add hot-reload: watch `~/.hammerspoon/` for file changes and call `hs.reload()`
- Use `hs.logger` for debug output

## Example use cases

The scenarios driving the PoC. Each should be expressible as a rule in whatever declarative config format we land on. These are worked through in order — Use Case 1 is what's implemented today.

### Use case 1: Ghostty + Marq 40/60 — implemented

"Always tile Marq and Ghostty with 40/60, Marq on the right."

When Marq becomes visible on the same screen as Ghostty, Ghostty takes the left 60% and Marq takes the right 40%. Linked resize. When Marq closes, Ghostty expands to fill.

Pseudo-rule:

```toml
[[rule]]
name = "ghostty-marq"
match = { apps = ["Ghostty", "Marq"], same_screen = true }
layout = "pair"
order = ["Ghostty", "Marq"]    # left → right
ratio = 0.6                    # Ghostty gets 0.6, Marq gets 0.4
linked_resize = true
on_close = "expand_remaining"
```

### Use case 2: Zen same-app pair

"Always tile Zen Browser windows next to each other."

When a second Zen window becomes visible, place it beside the first (50/50, or configurable). Opens design questions:

- What happens when a *third* Zen window opens? Options: (a) refuse to tile it, leave it floating; (b) rotate the oldest out; (c) stack the new one behind one of the tiled slots.
- When one of the two closes, does the survivor expand to fill?

Pseudo-rule:

```toml
[[rule]]
name = "zen-pair"
match = { app = "Zen Browser", window_count = 2 }
layout = "pair-same-app"
direction = "horizontal"
ratio = 0.5
on_third_window = "leave_floating"
```

### Use case 3: Shared slot with alternation

"If Marq and Zen are on the same space, keep them in the 40% right column and alternate them."

Introduces two new concepts:

1. **Slot** — a named region of the screen (e.g. `right-40` = the rightmost 40%).
2. **Slot policy** — a rule for how multiple members of a slot share it. `alternate` means only one is visible at a time; focusing the hidden one swaps them.

Pseudo-rule:

```toml
[slot.right-40]
frame = { x = 0.6, y = 0, w = 0.4, h = 1.0 }

[[rule]]
name = "marq-zen-alternate"
match = { apps = ["Marq", "Zen Browser"], same_space = true }
layout = "shared-slot"
slot = "right-40"
policy = "alternate"           # one visible at a time
initial_visible = "Marq"
```

This is the most ambitious of the three — it drives a v2 design of the config model. Use Cases 1 and 2 should be solid before tackling it.

### What these use cases collectively require

Looking across them, the config model needs:

- Named apps matched by process/bundle name
- Multi-app rules (Use Case 1) and same-app rules (Use Case 2)
- Named positional slots as a reusable concept (Use Case 3)
- Policies for slot occupancy: `pair`, `pair-same-app`, `shared-slot`
- Scoping predicates: `same_screen`, `same_space`, `window_count`
- Transition hooks: `on_close`, `on_third_window`

None of these are implemented yet — only Use Case 1 is working in the Hammerspoon PoC, and it's hardcoded rather than config-driven. The PoC's job is to validate the *behaviour*; formalising the config schema is a separate step that happens either late in the PoC (if Lua lets us prototype it cleanly) or at the start of the Swift port.

## Technical notes

- Use the `wf.windowVisible` / `wf.windowMoved` / `wf.windowNotVisible` constants — string event names like `"windowResized"` raise "invalid event" errors in current Hammerspoon
- There is **no** `windowResized` event — `windowMoved` fires for both moves and resizes
- `wf.new(tile_apps)` creates a filter for specific apps
- `wf.new(false)` creates an empty filter; use `:setAppFilter('AppName', {})` to add apps
- **`allowRoles = '*'` is required for Firefox-based apps** (Zen Browser, plus Electron/Tauri apps with non-standard AX roles). The default `hs.window.filter` only admits windows whose `AXRole` is `AXStandardWindow`, which silently drops Zen. Build the filter as `wf.new(false)` + `:setAppFilter(app, { allowRoles = '*', visible = true })` per app. This caused an hour of "why doesn't Zen trigger events" debugging
- `win:screen():frame()` gives the usable screen rect
- `win:setFrame({x, y, w, h})` positions and sizes a window
- A small delay (`hs.timer.doAfter(0.3, ...)`) on `windowVisible` helps let the window finish rendering before repositioning
- Check the exact app names Hammerspoon sees using `hs.application.runningApplications()` in the Hammerspoon console

## Composition with FlashSpace

The PoC runs alongside FlashSpace, and the two operate on completely different models. Background context — *how* FlashSpace works under the hood (hide/unhide via `NSRunningApplication`, not native Mission Control spaces), and *why* we picked it over native spaces — lives in [`flashspace.md`](./flashspace.md) ("Why not native macOS Spaces?" and "How a workspace switch actually works → FlashSpace does not use native macOS Spaces"). This section covers only the tiling-specific consequences.

### Consequence for the tiling engine

The `hs.spaces.focusedSpace()` / `hs.spaces.windowSpaces()` APIs talk to the native CoreGraphics spaces layer, which FlashSpace doesn't touch. On a FlashSpace-driven setup those APIs report the same space ID for every window, so any space-membership guard based on them is effectively a no-op: it always passes, even when the user has alt-tabbed to a window in a different FlashSpace workspace. We burned an iteration cycle on this before realising it.

**The correct guard is `win:isVisible()`.** When FlashSpace hides an app via `NSRunningApplication.hide()`, `hs.window:isVisible()` returns false for all of that app's windows, because it reflects the owning app's hidden state. So the rule for "should I tile X next to Ghostty?" reduces to "are both Ghostty and X currently visible on screen?" — no `hs.spaces` involved. `tileNextToGhostty` in `init.lua` implements this.

### Consequence for the Swift port

The sibling Swift app will need the same discipline: before acting on an AX window event, check whether the owning app is hidden (`NSRunningApplication.isHidden`). Reacting to `kAXWindowCreatedNotification` without that check would produce the same bug we hit here — unhiding an app in another FlashSpace workspace would trigger layout changes on a workspace the user isn't looking at.

Better still: if FlashSpace exposes workspace-change events (via its `runScriptAfterWorkspaceChange` hook, the CLI, or in-process notifications), the sibling app should listen to them directly and treat "current workspace" as authoritative, rather than re-deriving visibility from AX state on every event. That keeps the two apps coherent instead of racing each other through the AX layer.

### The runScriptAfterWorkspaceChange hook (implemented)

The `windowVisible` event approach had a race condition: FlashSpace unhides apps sequentially, so the `windowVisible` event for Zen could fire before Ghostty was unhidden — causing `ghostty_win:isVisible()` to return false and the tile to be skipped.

**Fix**: use FlashSpace's `runScriptAfterWorkspaceChange` setting, which fires *after* all apps are shown/hidden for the new workspace:

```toml
# config/flashspace/settings.toml
enableIntegrations = true
runScriptAfterWorkspaceChange = 'open -g "hammerspoon://tile-now"'
```

Hammerspoon binds a URL handler for `hammerspoon://tile-now`:

```lua
hs.urlevent.bind('tile-now', function()
  -- Scan visible apps and tile as appropriate.
  -- No race: all apps are already shown/hidden before this fires.
end)
```

The handler is the primary tiling trigger. The `windowVisible` subscription is kept as a secondary fallback (e.g. for apps opening mid-session, not via workspace switch). FlashSpace calls the script once per display, so the handler may fire more than once per switch — it is idempotent.

## File location

`~/.hammerspoon/init.lua`

## Related work

A survey of what's already out there, to clarify what niche we're filling and what we explicitly don't want to replicate.

### macOS built-in window management

Apple has added window management features incrementally. The Sequoia additions are the most relevant because they now cover most of what Rectangle uniquely offered.

#### Split View (El Capitan, 2015)

Long-press the green maximize button → "Tile Window to Left/Right". Creates a dedicated full-screen space containing exactly two apps with a draggable 50/50 divider. **Extremely rigid**: full-screen only, no third window, no other apps accessible without leaving the space. Useful for "pair these two apps and nothing else," irrelevant to our goal.

#### Stage Manager (Ventura, 2022)

Organises windows into "stages" — the focused app (or set of apps) is centred, others stack on the left. You can manually group windows into a stage ("gather windows"), but this is **focus-based grouping**, not tiling — windows inside a stage aren't arranged in a fixed layout. Orthogonal to what we want.

#### Window Tiling (Sequoia, 2024)

Apple's long-overdue answer to Rectangle. Built into macOS 15, no install required.

- Drag a window to the screen edge or corner to tile (halves, quarters)
- Menu: Window → Move & Resize → Fill / Left / Right / Top / Bottom / quadrants / thirds
- Keyboard shortcuts: `fn-⌃-F` fill, `fn-⌃-C` centre, `fn-⌃-⇧-←/→/↑/↓` for two-window arrangements, `fn-⌃-⌥-⇧-←/→` for three-window arrangements
- Configurable padding between tiles
- Works across displays

**What it still doesn't do**: per-app rules, event-driven triggers, linked resize, bonded window pairs, declarative config, same-app pairing. It's the stateless action-based model, same as Rectangle, just blessed by Apple.

### Third-party snap tools

**Rectangle** / **Magnet** / **BetterSnapTool** / **Moom** / **Loop** — all variations on the stateless action-based snap pattern. Press a hotkey or drag to an edge, the current window goes to a predefined position. Differences between them are mostly UI (radial menu vs menu bar vs hotkeys) and polish, not model.

Rectangle in one line: a keyboard- and drag-driven snap tool that treats every action as independent — there's no concept of two windows belonging together, no reaction to apps opening, no per-app rules. Once you've snapped Ghostty left and Marq right, Rectangle is done — drag Ghostty's edge and Marq stays exactly where it was.

### Comparison table

| | Rectangle | Sequoia Tiling | Ours |
|---|---|---|---|
| **Trigger** | Hotkey / drag-to-edge | Hotkey / drag-to-edge | Event-driven (app opens/moves) |
| **State** | Stateless | Stateless | Stateful — tracks bonded pairs |
| **Scope** | Generic halves/quarters | Generic halves/quarters/thirds | Specific per-app patterns |
| **Linked resize** | No | No | Yes |
| **Per-app rules** | No | No | Yes |
| **Same-app pairing** | No | No | Yes (Use Case 2) |
| **Config** | GUI preferences | System Settings GUI | Declarative file |
| **Cognitive load** | Remember hotkeys / drag zones | Remember hotkeys / drag zones | None during use (set once in config) |

### Full tiling window managers

**Yabai** / **Amethyst** / **chunkwm** — persistent layout invariants managed across all windows. Yabai uses private SkyLight APIs (requires disabling SIP) for genuinely instant repositioning; Amethyst and chunkwm stay within AX and are slower but simpler to install.

These are **much more ambitious** than what we want — they manage *every* window, enforce global layout rules (binary space partitioning, monocle, etc.), and demand that you think in terms of stacks/layers. The cognitive load is the opposite of what we're going for.

### The niche we're filling

Nothing shipping today covers the combination of:

1. **Event-driven** — reacts to app lifecycle events, not user input
2. **Pair-aware** — tracks bonded window relationships with linked resize
3. **Declaratively configured** — rules in a file, not clicks in a GUI
4. **Narrowly scoped** — specific patterns only, not a full WM

The closest conceptual analogue is i3/sway's workspace assignment rules (`assign [class="Firefox"] → workspace 2`), combined with a bonded-window model. There's no macOS equivalent.

### Sources

- [Apple Support: Mac window tiling icons & keyboard shortcuts](https://support.apple.com/guide/mac-help/mac-window-tiling-icons-keyboard-shortcuts-mchl9674d0b0/15.0/15.0)
- [9to5Mac: Automatic window tiling in macOS Sequoia](https://9to5mac.com/2024/07/19/use-automatic-window-tiling-macos-sequoia/)
- [SlashGear: Every macOS Sequoia Window Tiling Shortcut](https://www.slashgear.com/1673225/macos-sequoia-window-tiling-shortcuts/)
- [Rectangle (GitHub)](https://github.com/rxhanson/Rectangle)
- [Yabai (GitHub)](https://github.com/koekeishiya/yabai)

## Performance notes for the Swift port

Observed in the Hammerspoon PoC: linked resize works but feels laggy during a live drag — the sibling window catches up in jumps rather than tracking the mouse smoothly. Root causes and the plan to fix them in the native port:

### Why it's laggy in Hammerspoon

- Every resize event round-trips through LuaSkit's Lua ↔ Objective-C bridge. On a fast drag that's ~60 events/sec × Lua → C → AX → C → Lua marshalling.
- `hs.window.filter` doesn't coalesce rapid `windowMoved` events well — each one is delivered and handled.
- `AXUIElementSetAttributeValue` (what `win:setFrame` ultimately calls) is **synchronous** and blocks until the window server acknowledges. Each setFrame costs ~15–30ms regardless of language, which caps event-driven resize at ~30 updates/sec even under perfect conditions.

### Ceiling set by macOS itself

The AX API is the bottleneck, not Hammerspoon. A naive Swift port that subscribes to `AXObserver` notifications and calls `AXUIElementSetAttributeValue` will be noticeably smoother (~2–3× faster, no Lua overhead, proper runloop coalescing) but still not butter-smooth during a drag.

The apps that feel instant (Yabai) use **private SkyLight APIs** (`SLSSetWindowShape` etc.) that bypass AX entirely. This requires disabling SIP. **Not the path for this app** — FlashSpace stays inside AX and we should too.

### The approach we're taking: drag tracking

Don't react to `windowMoved` events at all during a live drag. Instead:

1. Detect drag start via a global event monitor (`NSEvent.addGlobalMonitorForEvents` on `.leftMouseDown` near a window edge of either tiled window)
2. During the drag, take over positioning in a tight mouse-tracking loop driven by `CADisplayLink` or `NSEvent.mouseLocation` polling at 60fps
3. Position the **sibling** window directly in sync with the cursor coordinates — skip the AX event round-trip for the dragged window, only update the sibling
4. On `.leftMouseUp`, release — let normal AX event handling resume

This is the same pattern Magnet / Rectangle / Loop use for their manual drag-to-resize modes. It effectively makes the two windows feel like one composite window instead of two windows chasing each other.

### Implementation sketch (for the Swift port)

- `ResizeDragController` class, activated when a tiled pair exists
- Global event tap (or `NSEvent.addGlobalMonitorForEvents`) watching for left-mouse events in a ~10px band around the inner edge of the tiled pair
- On drag start: capture initial frames of both windows, start a `CADisplayLink`
- Each tick: read current mouse X, compute new split, call `AXUIElementSetAttributeValue` **only on the sibling** (the dragged window is already being resized by the user)
- On drag end: stop the display link, reconcile via one final AX read to catch any drift

### Event handling outside of drags

Keep the event-driven path for everything else — external resizes (e.g. from a script, keyboard shortcut, or display change). The drag tracker is just the fast path for the common case.

## Testing

1. Open Ghostty
2. Open Marq (or whatever test app is in `tile_apps`)
3. Verify it auto-tiles: Ghostty left half, Marq right half
4. Drag the edge of Ghostty to resize — verify Marq adjusts to fill remaining space
5. Drag the edge of Marq to resize — verify Ghostty adjusts
6. Close Marq — verify Ghostty is no longer constrained
7. Check the Hammerspoon console for log output (menu bar icon → Console)
