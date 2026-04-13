# macOS Native Display Info

Developer reference for the bespoke Hammerspoon workspace manager. Covers native macOS ways to enumerate and identify connected displays, and how Hammerspoon exposes this.

---

## Hammerspoon `hs.screen` API

### Instance methods

| Method | Returns | Notes |
|---|---|---|
| `screen:name()` | string | Manufacturer name, e.g. `"DELL U2723QE"` |
| `screen:id()` | number | Numeric ID assigned by macOS — can change |
| `screen:getUUID()` | string | EDID-based UUID — stable across reboots |
| `screen:frame()` | table | Usable geometry (excludes menubar/dock) |
| `screen:fullFrame()` | table | Full geometry including reserved areas |
| `screen:position()` | x, y | Position relative to primary screen |
| `screen:currentMode()` | table | Resolution, refresh rate, scale, depth |
| `screen:getInfo()` | table | Full metadata table from macOS |

### Module-level functions

- `hs.screen.allScreens()` — list of all connected screens
- `hs.screen.mainScreen()` — screen with the focused window
- `hs.screen.screenPositions()` — map of screen name to grid position

---

## Native CLI Tools

### `system_profiler SPDisplaysDataType`

Human-readable summary of connected displays. Useful for a quick overview. Does not provide stable IDs.

### `ioreg -l | grep 'EDID UUID'`

Returns the EDID UUID per physical monitor. Survives reconnect and reboot.

Example output:

```
"EDID UUID" = "10AC7942-0000-0000-3021-0104B53C2278"
```

The first 4 hex characters of the UUID encode the manufacturer: `10AC` = Dell.

### `displayplacer`

Third-party CLI for display layout management. Not installed. For reference only.

---

## Stable Monitor Identification

Use `hs.screen:getUUID()` as the canonical identifier for a specific physical monitor.

- More reliable than `name()` — generic names like `"Display"` are common for some monitors
- More reliable than `id()` — numeric IDs can change across reboots or reconnects
- The EDID UUID is tied to the physical panel and persists across all connection events

---

## Dump All Display Info (Lua)

Paste into the Hammerspoon console to enumerate all connected displays with full metadata.

```lua
local function dumpAllScreenInfo()
  local log = hs.logger.new('screens', 'info')
  log.i("=== Connected Displays ===")
  local screens = hs.screen.allScreens()
  log.f("Total screens: %d", #screens)
  for idx, screen in ipairs(screens) do
    log.f("\n--- Display %d ---", idx)
    log.f("Name:          %s", screen:name() or "Unknown")
    log.f("ID:            %d", screen:id() or -1)
    log.f("UUID (EDID):   %s", screen:getUUID() or "Unknown")
    local f = screen:frame()
    local ff = screen:fullFrame()
    log.f("Usable frame:  x=%.0f y=%.0f w=%.0f h=%.0f", f.x, f.y, f.w, f.h)
    log.f("Full frame:    x=%.0f y=%.0f w=%.0f h=%.0f", ff.x, ff.y, ff.w, ff.h)
    local px, py = screen:position()
    log.f("Position:      x=%d y=%d", px, py)
    local mode = screen:currentMode()
    if mode then
      log.f("Mode:          %dx%d @ %.1fHz scale=%.1f depth=%d",
        mode.w, mode.h, mode.freq or 0, mode.scale or 1, mode.depth or 0)
    end
    local info = screen:getInfo()
    if info then
      for k, v in pairs(info) do
        log.f("  info.%s = %s", k, tostring(v))
      end
    end
  end
  log.i("\n=== Screen Positions ===")
  for name, coords in pairs(hs.screen.screenPositions()) do
    log.f("  %s: %s", name, hs.inspect(coords))
  end
end

dumpAllScreenInfo()
```

---

## This Machine (desk-A)

### Display aliases

| Alias | Name |
|---|---|
| `CENTRE` | `DELL U2723QE` |
| `LEFT` | `DELL U2419HC` |
| `LAPTOP` | `Built-in Retina Display` |

### EDID UUIDs

Not yet captured. Run `dumpAllScreenInfo()` from the Hammerspoon console and record the UUID values here.
