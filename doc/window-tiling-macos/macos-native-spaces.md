# macOS Native Spaces

## Overview

Native macOS Spaces are virtual desktops managed by `Dock.app` and `WindowServer` (via the SkyLight framework). There is no public API for scripting them. Private APIs exist but are unstable and partially broken on Sequoia.

## Architecture

- Implemented inside the `Dock.app` process, not a separate service.
- `WindowServer` (`SkyLight.framework`) is the compositor.
- CGS / SkyLight private APIs (`CGSPrivate.h`) are the reverse-engineered access layer.
- State lives in `~/Library/Preferences/com.apple.spaces.plist`. Only updated on Space create/destroy, not on switch. Do not edit directly.

## Per-display vs global (the critical setting)

**System Settings → Desktop & Dock → Mission Control → "Displays have separate Spaces"**:

- **ON**: each display has its own independent set of Spaces. Windows snap to one display.
- **OFF**: one continuous Spaces grid across all displays; swipe switches all monitors together.

System-wide, not per-display. Most multi-monitor workflows want it ON.

## Window-to-Space membership

Three states:

1. Assigned to a specific Space (default).
2. Sticky — "Appears on all Desktops" (right-click Dock icon → Options → Assign To → All Desktops).
3. Full-screen apps get their own dedicated Space.

Sonoma/Sequoia bug: sticky windows always render on top of Space-assigned windows, even when unfocused.

## Public APIs — what doesn't work

- **AppleScript / System Events**: reads preferences only. No switch/move commands.
- **Shortcuts.app**: no Spaces actions. Can only run `osascript` / shell.
- **Accessibility (AXUIElement)**: observes windows; cannot query or set Space membership.
- **NSWorkspace**: unrelated — "workspace" here means running apps, not virtual desktops.

No public API is useful for Space control.

## Private APIs — CGS / SkyLight

Reverse-engineered, undocumented, unstable. Known functions:

- `CGSGetActiveSpace()` — active Space on a display.
- `CGSCopySpaces()` — list all Spaces.
- `CGSManagedDisplayGetCurrentSpace()` — active Space per display.
- `CGSMoveWindowsToManagedSpace()` — move windows (needs SIP disabled for reliability).
- `CGSSpaceGetType()` — user-created vs full-screen.
- `CGSAddSpace()` / `CGSRemoveSpace()`.

Reliable access requires injecting code into `Dock.app`, which needs SIP partially disabled. That is the yabai approach.

## Hammerspoon `hs.spaces`

Wraps private APIs plus AX automation. No SIP required; needs Accessibility permission.

| Function | Purpose |
|---|---|
| `focusedSpace()` | active Space ID |
| `activeSpaceOnScreen(screen)` | active Space on a display |
| `gotoSpace(id)` | switch to Space (visible animation) |
| `moveWindowToSpace(win, id)` | **BROKEN on Sequoia 15** |
| `spacesForScreen(screen)` | list Space IDs on a display |
| `windowsForSpace(id)` | window IDs in a Space |
| `windowSpaces(win)` | Spaces containing a window |
| `spaceDisplay(id)` | display hosting a Space |
| `allSpaces()` | all Space IDs, keyed by screen UUID |
| `addSpaceToScreen(screen)` | create Space (slow — drives Dock via AX) |
| `removeSpace(id)` | destroy Space |
| `screensHaveSeparateSpaces()` | query the setting |
| `spaceType(id)` | user / full-screen |

Gotchas: Sequoia broke `moveWindowToSpace`. Space creation is slow. Space IDs shift when reordered in Mission Control.

## How other tools handle Spaces

| Tool | Strategy | SIP | Reliability |
|---|---|---|---|
| **yabai** | Private SkyLight APIs, injects into Dock | Partial disable | High, brittle across OS updates |
| **AeroSpace** | Reimplements workspaces; moves inactive windows off-screen | None | High, but Mission Control cluttered |
| **FlashSpace** | Hides/unhides apps (`NSRunningApplication.hide()`); all workspaces live in one Space | None | High; app-level only |
| **Hammerspoon `hs.spaces`** | AX + private APIs | None | Partial; broken on Sequoia |

## Capability matrix

### No SIP, no special permissions

- Query active Space (`hs.spaces.focusedSpace`).
- List Spaces and windows in Spaces.
- Hide/unhide apps via `NSRunningApplication` (FlashSpace's approach).
- Observe window events via AX.

### Accessibility permission required

- Switch Spaces (`hs.spaces.gotoSpace`).
- Create/destroy Spaces (slow).
- Trigger keyboard shortcuts via `osascript`.

### SIP partial disable required

- yabai-level Space control.
- Sticky windows programmatically.
- Window layer / transparency control.
- Reliable window-to-Space move.

### Not possible (or unreliable enough to avoid)

- `moveWindowToSpace` on Sequoia (Hammerspoon path).
- Naming Spaces in Mission Control.
- Stable Space IDs across reorder.
- Stage Manager control (no public API).
- Preserving window positions across sleep/wake.

## Keyboard shortcuts (native)

If enabled in System Settings → Keyboard → Keyboard Shortcuts → Mission Control:

- `Ctrl+←` / `Ctrl+→` — switch Space.
- `Ctrl+1..9` — switch to Space N.

Can be triggered via `osascript` keystroke injection or a Hammerspoon hotkey, but direct `hs.spaces.gotoSpace()` is more reliable.

## Stage Manager

Orthogonal to Spaces, not a replacement. Sits on top of Spaces — each Space has its own Stage Manager state. No public API. Requires "Displays have separate Spaces" ON.

## Sequoia notes

- `hs.spaces.moveWindowToSpace` broken.
- New "Window Tiling" feature is just snap-to-edge resizing — unrelated to Spaces.
- Full-screen and Split View still create dedicated Spaces.

## Implications for this project

1. Don't build on native Spaces directly — no reliable API without SIP disable.
2. FlashSpace's hide/unhide model is the pragmatic path for per-workspace app sets.
3. If per-window (not per-app) workspace membership is needed, the AeroSpace off-screen model is the no-SIP option.
4. `hs.spaces` can be used for querying state and switching Spaces if we layer our workspace manager on top of a single native Space (status quo).
5. Document "Displays have separate Spaces = ON" as a setup requirement.

## References

- https://www.hammerspoon.org/docs/hs.spaces.html
- https://github.com/koekeishiya/yabai/wiki/Disabling-System-Integrity-Protection
- https://nikitabobko.github.io/AeroSpace/guide
- https://github.com/MarkVillacampa/undocumented-goodness/blob/master/CoreGraphics/CGSPrivate.h
