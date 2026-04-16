-- ~/.hammerspoon/init.lua
--
-- Bespoke workspace manager. Design: doc/window-tiling-macos/bespoke-poc-with-hammerspoon.md

package.path = package.path .. ";" .. hs.configdir .. "/lib/?.lua"
local toml = require("toml")

local log = hs.logger.new("ws", "info")

-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

local CONFIG_PATH = hs.configdir .. "/config.toml"

local function readFile(path)
  local f, err = io.open(path, "r")
  if not f then error("cannot open " .. path .. ": " .. tostring(err)) end
  local contents = f:read("*a")
  f:close()
  return contents
end

local function loadConfig()
  local raw = readFile(CONFIG_PATH)
  local ok, result = pcall(toml.parse, raw)
  if not ok then error("TOML parse failed: " .. tostring(result)) end
  return result
end

local config = loadConfig()

-- ---------------------------------------------------------------------------
-- Display profile detection
-- ---------------------------------------------------------------------------

-- Priority order used when multiple roles resolve to the same physical screen
-- (e.g. laptop-only profile collapses LEFT/CENTRE/LAPTOP → builtin).
-- First match in this list is the canonical display label for that screen.
local ROLE_PRIORITY = { "LAPTOP", "CENTRE", "LEFT" }

-- Returns a stable key for a screen: EDID UUID for externals, "builtin" for
-- the internal panel (Apple's built-in name always contains "Built-in").
local function screenKey(screen)
  local name = screen:name() or ""
  if name:lower():find("built%-in") then return "builtin" end
  return screen:getUUID() or ("name:" .. name)
end

-- Match current screens against named profiles. Matching uses the SET of
-- unique UUID values in a profile vs the set of present screens — so a
-- laptop-only profile with LEFT=CENTRE=LAPTOP=builtin matches when exactly
-- one screen (builtin) is present.
local function resolveProfile()
  local screens = hs.screen.allScreens()
  local present = {}      -- key → screen
  local presentCount = 0
  for _, s in ipairs(screens) do
    local k = screenKey(s)
    if not present[k] then presentCount = presentCount + 1 end
    present[k] = s
  end

  for name, profile in pairs(config.profiles or {}) do
    -- Collect unique UUID values for this profile
    local uniqueKeys = {}
    local uniqueCount = 0
    for _, key in pairs(profile) do
      if not uniqueKeys[key] then uniqueCount = uniqueCount + 1 end
      uniqueKeys[key] = true
    end

    if uniqueCount == presentCount then
      local match = true
      for key in pairs(uniqueKeys) do
        if not present[key] then match = false; break end
      end
      if match then
        local roleMap = {}
        for role, key in pairs(profile) do
          roleMap[role] = present[key]
        end
        log.i("profile matched: " .. name)
        return name, roleMap
      end
    end
  end

  log.w("no profile matched; falling back to primary for every role")
  local primary = hs.screen.primaryScreen()
  local fallback = {}
  for _, space in pairs(config.spaces or {}) do
    if space.role then fallback[space.role] = primary end
  end
  return "unknown", fallback
end

local activeProfile, roleToScreen = resolveProfile()

local function resolveDisplay(role)
  return roleToScreen[role] or hs.screen.primaryScreen()
end

-- ---------------------------------------------------------------------------
-- Alert style (shared by all popups)
-- ---------------------------------------------------------------------------

local ALERT_STYLE = {
  textFont        = "Menlo",
  textSize        = 30,
  textColor       = { hex = "#00ff88", alpha = 0.95 },
  fillColor       = { hex = "#1a1a2e", alpha = 0.92 },
  strokeColor     = { hex = "#4a4a6a", alpha = 0.8 },
  strokeWidth     = 1.5,
  radius          = 8,
  fadeInDuration  = 0.1,
  fadeOutDuration = 0.15,
  padding         = 24,
  atScreenEdge    = 0,
}

local function alert(msg, duration)
  hs.alert.closeAll(0)
  hs.alert.show(msg, ALERT_STYLE, duration or 1.2)
end

-- ---------------------------------------------------------------------------
-- Given a resolved screen, return the canonical role label for it under the
-- current profile. Uses ROLE_PRIORITY so that when multiple roles collapse to
-- the same screen (laptop-only), LAPTOP wins.
local function displayLabel(screen)
  for _, role in ipairs(ROLE_PRIORITY) do
    if roleToScreen[role] and roleToScreen[role]:id() == screen:id() then
      return role
    end
  end
  return screen:name() or "?"
end

-- ---------------------------------------------------------------------------
-- Space activation
-- ---------------------------------------------------------------------------

local activeSpace   = {}   -- screenId → spaceName  (one entry per physical screen)
local statusAlertId = nil  -- forward-declared so activateSpace can reference it
local refreshStatus        -- forward-declared; defined after buildStatusText
local applyLayout          -- forward-declared; defined after activateSpace (tiling section)
local EXEMPT = {
  ["com.apple.finder"] = true,
}

-- Session-level app overrides: bundleId → spaceName.
-- Overrides which space an app is assigned to for this session only.
-- Config is ground truth and is never modified; cleared on reload.
local sessionAppOverride  = {}

-- Session-level role overrides: spaceName → role.
-- Overrides which display a space lives on (set via cmd+shift+1/2/3).
-- Cleared whenever the display profile changes.
local sessionRoleOverride = {}

-- Returns the effective set of bundle IDs for a space, merging config
-- with session overrides (apps moved in/out at runtime).
local function effectiveApps(spaceName)
  local space = config.spaces and config.spaces[spaceName]
  local apps  = {}
  for _, bundleId in ipairs((space and space.apps) or {}) do
    local ov = sessionAppOverride[bundleId]
    if not ov or ov == spaceName then apps[bundleId] = true end
  end
  for bundleId, targetSpace in pairs(sessionAppOverride) do
    if targetSpace == spaceName then apps[bundleId] = true end
  end
  return apps
end

local function activateSpace(spaceName, role)
  local space = config.spaces and config.spaces[spaceName]
  if not space then log.w("unknown space: " .. spaceName); return end
  role = role or sessionRoleOverride[spaceName] or space.role
  if not role then log.w("no role for space " .. spaceName); return end

  local screen   = resolveDisplay(role)
  local screenId = screen:id()
  local label    = displayLabel(screen)

  -- Update state: one entry per physical screen
  activeSpace[screenId] = spaceName

  -- Build the full set of apps that should be visible across ALL active spaces
  local shouldShow = {}
  for _, sName in pairs(activeSpace) do
    for bundleId in pairs(effectiveApps(sName)) do
      shouldShow[bundleId] = true
    end
  end

  log.i(string.format("activate %s [%s] (screen: %s)", spaceName, label, screen:name()))

  -- Unhide apps for the newly active space
  for bundleId in pairs(effectiveApps(spaceName)) do
    local app = hs.application.get(bundleId)
    if app then app:unhide() end
  end

  -- Hide every running app not in the visible set
  for _, app in ipairs(hs.application.runningApplications()) do
    local bundleId = app:bundleID()
    if bundleId and not shouldShow[bundleId] and not EXEMPT[bundleId] then
      if not app:isHidden() then app:hide() end
    end
  end

  -- Apply layout after a short delay so unhide is complete before tiling
  hs.timer.doAfter(0.3, function() applyLayout(spaceName, screen) end)

  if statusAlertId then
    refreshStatus()
  else
    alert(spaceName .. " [" .. label .. "]")
  end
end

-- ---------------------------------------------------------------------------
-- Tiling
-- ---------------------------------------------------------------------------

local tiledMain          = nil   -- main window currently in a layout, or nil
local ignoringResize     = false
local spaceRatioOverride = {}    -- spaceName → ratio, set when user drags the split

-- Move a window to a target screen then size it. setFrame alone is unreliable
-- across displays — macOS tends to keep the window on its current screen.
-- moveToScreen handles the cross-display hop first, then setFrame sets exact
-- geometry. Logs intended vs actual frame so we can spot silent failures.
local function placeWindow(win, screen, frame, tag)
  if not win then return end
  win:moveToScreen(screen, false, false, 0)
  win:setFrame(frame, 0)
  local got = win:frame()
  local delta = (math.abs(got.x - frame.x) + math.abs(got.y - frame.y)
              +  math.abs(got.w - frame.w) + math.abs(got.h - frame.h))
  if delta > 2 then
    log.w(string.format("place %s [%s]: want %d,%d %dx%d got %d,%d %dx%d",
      tag or "?", win:application():name() or "?",
      frame.x, frame.y, frame.w, frame.h,
      got.x, got.y, got.w, got.h))
  end
end

-- Bundle IDs of all apps in spaces that have a layout.
local pairBundleSet = {}
for _, space in pairs(config.spaces or {}) do
  if space.layout then
    for _, bundleId in ipairs(space.apps or {}) do
      pairBundleSet[bundleId] = true
    end
  end
end

-- When the main window moves/resizes, push the updated sidebar frame to all
-- visible sidebar windows for the active space on that screen.
local function syncLayout(changedWin)
  if not tiledMain or ignoringResize then return end
  if not tiledMain:isVisible() then tiledMain = nil; return end

  local screen    = tiledMain:screen()
  local sf        = screen:frame()
  local spaceName = activeSpace[screen:id()]
  local space     = spaceName and config.spaces and config.spaces[spaceName]
  local layout    = space and space.layout
  if not layout or layout.type ~= "sidebars" then return end

  ignoringResize = true

  if changedWin:id() == tiledMain:id() then
    -- Main resized: push new sidebar frame to all visible sidebar windows
    local mf        = tiledMain:frame()
    local sidebarFr = { x = mf.x + mf.w, y = sf.y, w = sf.x + sf.w - (mf.x + mf.w), h = sf.h }
    spaceRatioOverride[spaceName] = mf.w / sf.w
    for bundleId in pairs(effectiveApps(spaceName)) do
      if bundleId ~= layout.main then
        local app = hs.application.get(bundleId)
        local win = app and app:mainWindow()
        if win and win:isVisible() then win:setFrame(sidebarFr) end
      end
    end
  else
    -- A sidebar was resized: use its left edge as the new boundary,
    -- resize main to fill left of it, push same frame to other sidebars
    local cf        = changedWin:frame()
    local mainFr    = { x = sf.x, y = sf.y, w = cf.x - sf.x,         h = sf.h }
    local sidebarFr = { x = cf.x, y = sf.y, w = sf.x + sf.w - cf.x,  h = sf.h }
    spaceRatioOverride[spaceName] = mainFr.w / sf.w
    tiledMain:setFrame(mainFr)
    for bundleId in pairs(effectiveApps(spaceName)) do
      if bundleId ~= layout.main then
        local app = hs.application.get(bundleId)
        local win = app and app:mainWindow()
        if win and win:isVisible() and win:id() ~= changedWin:id() then
          win:setFrame(sidebarFr)
        end
      end
    end
  end

  hs.timer.doAfter(0.5, function() ignoringResize = false end)
end

-- pairFilter watches layout apps for move/close events.
-- hs.window.filter requires display names (not bundle IDs), so it is built
-- lazily / updated via ensurePairFilter() as apps come online.
_G.pairFilter = nil

local function ensurePairFilter()
  local names = {}
  for bundleId in pairs(pairBundleSet) do
    local app = hs.application.get(bundleId)
    if app then names[app:name()] = true end
  end
  if not next(names) then return end

  if not _G.pairFilter then
    _G.pairFilter = hs.window.filter.new(false)
    _G.pairFilter:subscribe(hs.window.filter.windowMoved, function(win) syncLayout(win) end)
    _G.pairFilter:subscribe(hs.window.filter.windowDestroyed, function(win)
      if not tiledMain then return end
      local screen = tiledMain:screen()
      if not screen then return end
      local spaceName = activeSpace[screen:id()]
      if not spaceName then return end
      tiledMain = nil
      hs.timer.doAfter(0.1, function() applyLayout(spaceName, screen) end)
    end)
  end

  for name in pairs(names) do
    _G.pairFilter:setAppFilter(name, { allowRoles = '*' })
  end
end

-- applyLayout is forward-declared above activateSpace; defined here.
applyLayout = function(spaceName, screen)
  tiledMain = nil

  local space  = config.spaces and config.spaces[spaceName]
  local layout = space and space.layout
  if not layout or layout.type ~= "sidebars" then return end

  local ratio   = spaceRatioOverride[spaceName] or layout.ratio or 0.65
  local sf      = screen:frame()
  local mainApp = layout.main and hs.application.get(layout.main)
  local mainWin = mainApp and mainApp:mainWindow()
  local mainVis = mainWin and mainWin:isVisible()

  -- Any visible non-main app in this space becomes a sidebar
  local sidebarWins = {}
  for bundleId in pairs(effectiveApps(spaceName)) do
    if bundleId ~= layout.main then
      local app = hs.application.get(bundleId)
      local win = app and app:mainWindow()
      if win and win:isVisible() then table.insert(sidebarWins, win) end
    end
  end

  local mainFr    = { x = sf.x,                y = sf.y, w = sf.w * ratio,        h = sf.h }
  local sidebarFr = { x = sf.x + sf.w * ratio, y = sf.y, w = sf.w * (1 - ratio),  h = sf.h }

  if mainVis and #sidebarWins > 0 then
    ignoringResize = true
    placeWindow(mainWin, screen, mainFr, "main")
    for _, win in ipairs(sidebarWins) do placeWindow(win, screen, sidebarFr, "sidebar") end
    tiledMain = mainWin
    log.i(string.format("sidebars: main + %d sidebar(s) @ %.0f%%", #sidebarWins, ratio * 100))
    hs.timer.doAfter(0.5, function() ignoringResize = false end)
  elseif mainVis then
    placeWindow(mainWin, screen, sf, "main-fill")
    log.i("layout: main alone → fill")
  elseif #sidebarWins > 0 then
    for _, win in ipairs(sidebarWins) do placeWindow(win, screen, sf, "sidebar-fill") end
    log.i("layout: sidebars alone → fill")
  end
end

-- Watch for layout apps launching. hs.application.watcher fires on launch
-- regardless of whether the app was running at init.
_G.pairAppWatcher = hs.application.watcher.new(function(appName, eventType, app)
  if eventType ~= hs.application.watcher.launched then return end
  if not app then return end
  local bundleId = app:bundleID()
  if not pairBundleSet[bundleId] then return end
  log.i("layout app launched: " .. appName)
  hs.timer.doAfter(0.5, function()
    ensurePairFilter()
    for spaceName, space in pairs(config.spaces or {}) do
      if space.layout then
        local scr = resolveDisplay(space.role)
        if activeSpace[scr:id()] == spaceName then
          applyLayout(spaceName, scr)
        end
      end
    end
  end)
end)
_G.pairAppWatcher:start()

ensurePairFilter()

-- ---------------------------------------------------------------------------
-- Status overlay (cmd+opt+space) — shows active space per display.
-- Placeholder for the eventual full space-picker overlay.
-- ---------------------------------------------------------------------------

local STATUS_STYLE = ALERT_STYLE

local function buildStatusText()
  local maxNameLen = 0
  for _, hk in ipairs(config.hotkeys or {}) do
    if #hk.space > maxNameLen then maxNameLen = #hk.space end
  end

  local function row(key, name, label, active)
    local pad    = string.rep(" ", maxNameLen - #name)
    local marker = active and "  ◀" or ""
    return "  " .. key .. "   " .. name .. pad .. "   " .. label .. marker
  end

  local seenScreenId = {}
  local activeLines  = {}
  for _, role in ipairs(ROLE_PRIORITY) do
    local screen = roleToScreen[role]
    if screen and not seenScreenId[screen:id()] then
      seenScreenId[screen:id()] = true
      local label = displayLabel(screen)
      local sName = activeSpace[screen:id()] or "(none)"
      local pad   = string.rep(" ", 6 - #label)
      table.insert(activeLines, "  " .. label .. pad .. "  " .. sName)
    end
  end

  local sep   = "  " .. string.rep("─", maxNameLen + 16)
  local lines = { "  " .. activeProfile, sep }
  for _, l in ipairs(activeLines) do table.insert(lines, l) end
  table.insert(lines, sep)

  for _, hk in ipairs(config.hotkeys or {}) do
    local space  = config.spaces and config.spaces[hk.space]
    local role   = space and space.role or "?"
    local label  = displayLabel(resolveDisplay(role))
    local active = activeSpace[resolveDisplay(role):id()] == hk.space
    table.insert(lines, row(hk.key, hk.space, "[" .. label .. "]", active))
  end
  table.insert(lines, sep)
  return table.concat(lines, "\n")
end

local function openStatus()
  hs.alert.closeAll(0)
  statusAlertId = hs.alert.show(buildStatusText(), STATUS_STYLE, math.huge)
end

refreshStatus = function()
  if not statusAlertId then return end
  openStatus()
end

local function showStatus()
  if statusAlertId then
    hs.alert.closeAll(0)
    statusAlertId = nil
    return
  end
  openStatus()
end

-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Diagnostics (cmd+alt+z) — dumps state to the Hammerspoon console.
-- Uses 'z' because z-key space hotkeys are unlikely.
-- ---------------------------------------------------------------------------

local function dumpDiagnostics()
  print("")
  print("=== TILR DIAGNOSTICS ===")
  print("profile: " .. activeProfile)
  print("")
  print("--- screens ---")
  for _, s in ipairs(hs.screen.allScreens()) do
    local f = s:frame()
    print(string.format("  %-28s id=%-12d uuid=%s  x=%d y=%d w=%d h=%d",
      s:name() or "?", s:id(), s:getUUID() or "-", f.x, f.y, f.w, f.h))
  end
  print("")
  print("--- roleToScreen ---")
  for role, s in pairs(roleToScreen) do
    print(string.format("  %-6s → %s (id=%d)", role, s:name() or "?", s:id()))
  end
  print("")
  print("--- activeSpace ---")
  for screenId, sName in pairs(activeSpace) do
    print(string.format("  screen id=%-12d → %s", screenId, sName))
  end
  print("")
  print("--- sessionRoleOverride ---")
  if next(sessionRoleOverride) then
    for space, role in pairs(sessionRoleOverride) do
      print(string.format("  %s → %s", space, role))
    end
  else print("  (empty)") end
  print("")
  print("--- sessionAppOverride ---")
  if next(sessionAppOverride) then
    for bundleId, space in pairs(sessionAppOverride) do
      print(string.format("  %s → %s", bundleId, space))
    end
  else print("  (empty)") end
  print("")
  local win = hs.window.focusedWindow()
  if win then
    local f = win:frame()
    local s = win:screen()
    print(string.format("focused: %s [%s]", win:title() or "?", win:application():name() or "?"))
    print(string.format("  frame:  x=%d y=%d w=%d h=%d", f.x, f.y, f.w, f.h))
    print(string.format("  screen: %s (id=%d)", s and s:name() or "?", s and s:id() or -1))
  else
    print("focused: (none)")
  end
  print("========================")
  alert("Diagnostics → console")
end

hs.hotkey.bind({ "cmd", "alt" }, "z", dumpDiagnostics)

-- ---------------------------------------------------------------------------
-- Hotkeys
-- ---------------------------------------------------------------------------

local function parseMods(str)
  local mods = {}
  for m in string.gmatch(str or "", "%S+") do table.insert(mods, m) end
  return mods
end

local boundHotkeys = {}
local function bindHotkeys()
  for _, hk in ipairs(boundHotkeys) do hk:delete() end
  boundHotkeys = {}
  for _, hk in ipairs(config.hotkeys or {}) do
    local binding = hs.hotkey.bind(parseMods(hk.mods), hk.key, function()
      activateSpace(hk.space)
    end)
    table.insert(boundHotkeys, binding)
  end
end

bindHotkeys()

-- Move focused app to a space (opt+shift+key mirrors the space-switch keys).
-- Sets a session-level override so the app follows to the target space and
-- is removed from its current space. Config is never modified.
local function moveFocusedAppToSpace(targetSpaceName)
  local win = hs.window.focusedWindow()
  if not win then alert("no focused window"); return end
  local app = win:application()
  if not app then return end
  local bundleId = app:bundleID()
  if not bundleId then return end
  local appName     = app:name() or bundleId
  local targetSpace = config.spaces and config.spaces[targetSpaceName]

  sessionAppOverride[bundleId] = targetSpaceName
  log.i(string.format("session move: %s → %s", bundleId, targetSpaceName))

  -- If target space has a layout, register this app with the resize watcher
  if targetSpace and targetSpace.layout then
    pairBundleSet[bundleId] = true
    ensurePairFilter()
  end

  activateSpace(targetSpaceName)

  -- For spaces with no sidebars layout, expand the moved app to fill its screen.
  -- Runs after the unhide delay so the window is visible before setFrame.
  if not (targetSpace and targetSpace.layout and targetSpace.layout.type == "sidebars") then
    hs.timer.doAfter(0.35, function()
      local movedApp = hs.application.get(bundleId)
      local movedWin = movedApp and movedApp:mainWindow()
      if movedWin and movedWin:isVisible() then
        local targetScreen = resolveDisplay(targetSpace and targetSpace.role or "CENTRE")
        placeWindow(movedWin, targetScreen, targetScreen:frame(), "moved-fill")
      end
    end)
  end

  -- Replaces the space-switch alert with a more informative one
  alert("Moved " .. appName .. " → " .. targetSpaceName)
end

for _, hk in ipairs(config.hotkeys or {}) do
  hs.hotkey.bind(parseMods("alt shift"), hk.key, function()
    moveFocusedAppToSpace(hk.space)
  end)
end

-- Move active space to a display: cmd+shift+1=LEFT, cmd+shift+2=CENTRE, cmd+shift+3=LAPTOP.
-- Takes whichever space is active on the focused window's display and reassigns it.
local ROLE_FOR_KEY = { ["1"] = "LEFT", ["2"] = "CENTRE", ["3"] = "LAPTOP" }

local function moveActiveSpaceToRole(targetRole)
  local win       = hs.window.focusedWindow()
  local screen    = (win and win:screen()) or hs.screen.mainScreen()
  local spaceName = activeSpace[screen:id()]
  if not spaceName then alert("no active space here"); return end
  if resolveDisplay(targetRole):id() == screen:id() then
    alert(spaceName .. " already on " .. targetRole); return
  end
  sessionRoleOverride[spaceName] = targetRole
  log.i(string.format("space move: %s → %s", spaceName, targetRole))
  activateSpace(spaceName, targetRole)
  alert(spaceName .. " → " .. targetRole)
end

for key, role in pairs(ROLE_FOR_KEY) do
  hs.hotkey.bind({ "cmd", "shift" }, key, function() moveActiveSpaceToRole(role) end)
end

hs.hotkey.bind({ "cmd", "alt" }, "space", showStatus)

-- ---------------------------------------------------------------------------
-- Reactivity
-- ---------------------------------------------------------------------------

-- Apply per-profile defaults: activate each space on its role, deduped by
-- screen ID so laptop-only (all roles → same screen) only activates once.
-- Clears sessionRoleOverride so display assignments start fresh on each profile.
local function applyProfileDefaults(profileName, silent)
  local profileDefaults = config.defaults and config.defaults[profileName]
  if not profileDefaults then
    log.w("no defaults for profile: " .. profileName); return
  end
  sessionRoleOverride = {}
  local seenScreenId  = {}
  for _, role in ipairs(ROLE_PRIORITY) do
    local spaceName = profileDefaults[role]
    if spaceName and config.spaces and config.spaces[spaceName] then
      local screenId = resolveDisplay(role):id()
      if not seenScreenId[screenId] then
        seenScreenId[screenId] = true
        if silent then
          activeSpace[screenId] = spaceName
          log.i("default (silent): " .. role .. " → " .. spaceName)
        else
          activateSpace(spaceName, role)
        end
      end
    end
  end
end

_G.screenWatcher = hs.screen.watcher.new(function()
  local prevProfile   = activeProfile
  activeProfile, roleToScreen = resolveProfile()
  if activeProfile ~= prevProfile then
    log.i("profile changed: " .. prevProfile .. " → " .. activeProfile)
    applyProfileDefaults(activeProfile, false)
  end
  alert("profile: " .. activeProfile)
end)
_G.screenWatcher:start()

-- Config reload
hs.hotkey.bind({ "cmd", "alt" }, "r", function() hs.reload() end)

-- Apply startup defaults — activates each space so apps are shown/hidden correctly.
applyProfileDefaults(activeProfile, false)

alert("Tilr — " .. activeProfile)
log.i("init.lua loaded; profile=" .. activeProfile)
