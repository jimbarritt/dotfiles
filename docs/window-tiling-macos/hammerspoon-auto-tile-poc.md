# Hammerspoon Auto-Tile PoC — Agent Brief

## Goal

Create a Hammerspoon config (`~/.hammerspoon/init.lua`) that auto-tiles specific app windows beside Ghostty, with linked resize behaviour.

## Context

- Hammerspoon is a macOS automation tool that wraps the accessibility API in Lua
- It runs as a persistent background daemon (menu bar app)
- `hs.window.filter` can subscribe to window events globally (created, visible, resized, destroyed, etc.)
- We are NOT building a full tiling WM — just handling one specific behaviour
- This will eventually be paired with FlashSpace for workspace switching

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

## Technical notes

- Use string event names like `"windowVisible"`, `"windowResized"`, `"windowNotVisible"` — the `wf.windowVisible` constants may resolve to nil depending on Hammerspoon version
- `wf.new(tile_apps)` creates a filter for specific apps
- `wf.new(false)` creates an empty filter; use `:setAppFilter('AppName', {})` to add apps
- `win:screen():frame()` gives the usable screen rect
- `win:setFrame({x, y, w, h})` positions and sizes a window
- A small delay (`hs.timer.doAfter(0.3, ...)`) on `windowVisible` helps let the window finish rendering before repositioning
- Check the exact app names Hammerspoon sees using `hs.application.runningApplications()` in the Hammerspoon console

## File location

`~/.hammerspoon/init.lua`

## Testing

1. Open Ghostty
2. Open Marq (or whatever test app is in `tile_apps`)
3. Verify it auto-tiles: Ghostty left half, Marq right half
4. Drag the edge of Ghostty to resize — verify Marq adjusts to fill remaining space
5. Drag the edge of Marq to resize — verify Ghostty adjusts
6. Close Marq — verify Ghostty is no longer constrained
7. Check the Hammerspoon console for log output (menu bar icon → Console)
