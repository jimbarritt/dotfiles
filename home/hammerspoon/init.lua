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
-- Screen registry
--
-- screensByName: logical name → hs.screen object, rebuilt on every screen change.
--   "laptop"            always the builtin (detected by name containing "Built-in")
--   "external-main"     UUID-matched from config.screens
--   "external-secondary" UUID-matched from config.screens
--
-- SCREEN_PRECEDENCE: name → integer rank (1 = highest priority).
--   Used only to determine migration direction on unplug:
--     going DOWN (source rank < dest rank) → active space travels and takes over
--     going UP   (source rank > dest rank) → active space hides, dest keeps its own
-- ---------------------------------------------------------------------------

local SCREEN_PRECEDENCE = {}
do
  local chain = config.screens and config.screens.precedence or
                {"external-main", "external-secondary", "laptop"}
  for i, name in ipairs(chain) do SCREEN_PRECEDENCE[name] = i end
end

local function precedenceOf(name) return SCREEN_PRECEDENCE[name] or 99 end

local function buildScreensByName()
  local byName = {}
  for _, s in ipairs(hs.screen.allScreens()) do
    local sname = s:name() or ""
    if sname:lower():find("built%-in") then
      byName["laptop"] = s
    else
      local uuid      = s:getUUID()
      local logical   = uuid and config.screens and config.screens[uuid]
      if logical then
        byName[logical] = s
      else
        log.w("unrecognised screen: " .. sname .. " uuid=" .. tostring(uuid))
      end
    end
  end
  return byName
end

local screensByName = buildScreensByName()

-- Returns a human-readable summary of connected screens (used in alerts).
local function screenSetLabel()
  if screensByName["external-main"] and screensByName["external-secondary"] then
    return "full-desk"
  elseif screensByName["external-main"] then
    return "main-only"
  elseif screensByName["external-secondary"] then
    return "secondary-only"
  else
    return "laptop-only"
  end
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
-- Space activation
-- ---------------------------------------------------------------------------

local activeSpace      = {}   -- screenId (int) → spaceName
local statusAlertId    = nil
local refreshStatus           -- forward-declared
local applyLayout             -- forward-declared

local EXEMPT = { ["com.apple.finder"] = true }

-- Session-level overrides (cleared on reload).
local sessionAppOverride    = {}   -- bundleId  → spaceName
local sessionScreenOverride = {}   -- spaceName → screenName

-- Returns the effective bundle IDs for a space, merging config with session moves.
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

-- Resolve which physical screen a space should live on right now.
-- Walks the space's prefer list; returns (screen, screenName).
local function resolveSpaceScreen(spaceName)
  local ov = sessionScreenOverride[spaceName]
  if ov and screensByName[ov] then return screensByName[ov], ov end
  local space  = config.spaces and config.spaces[spaceName]
  local prefer = space and space.prefer or {}
  for _, name in ipairs(prefer) do
    if screensByName[name] then return screensByName[name], name end
  end
  local fallback = screensByName["laptop"] or hs.screen.primaryScreen()
  return fallback, "laptop"
end

local function activateSpace(spaceName, forceScreenName)
  local space = config.spaces and config.spaces[spaceName]
  if not space then log.w("unknown space: " .. spaceName); return end

  local screen, screenName
  if forceScreenName and screensByName[forceScreenName] then
    screen, screenName = screensByName[forceScreenName], forceScreenName
  else
    screen, screenName = resolveSpaceScreen(spaceName)
  end
  local screenId = screen:id()

  activeSpace[screenId] = spaceName

  -- Build full set of apps that should be visible across ALL active spaces
  local shouldShow = {}
  for _, sName in pairs(activeSpace) do
    for bundleId in pairs(effectiveApps(sName)) do shouldShow[bundleId] = true end
  end

  log.i(string.format("activate %s [%s] (screen: %s)", spaceName, screenName, screen:name()))

  local alertMsg = spaceName .. " [" .. screenName .. "]"
  if statusAlertId then refreshStatus()
  else alert(alertMsg) end

  for bundleId in pairs(effectiveApps(spaceName)) do
    local app = hs.application.get(bundleId)
    if app then app:unhide() end
  end

  for _, app in ipairs(hs.application.runningApplications()) do
    local bundleId = app:bundleID()
    if bundleId and not shouldShow[bundleId] and not EXEMPT[bundleId] then
      if not app:isHidden() then app:hide() end
    end
  end

  if shouldShow["org.hammerspoon.Hammerspoon"] then hs.openConsole(false)
  else local w = hs.console.hswindow(); if w then w:close() end end

  hs.timer.doAfter(0.2, function()
    applyLayout(spaceName, screen)
    if statusAlertId then refreshStatus()
    else alert(alertMsg) end
  end)
end

-- ---------------------------------------------------------------------------
-- Tiling
-- ---------------------------------------------------------------------------

local tiledMain          = nil
local ignoringResize     = false
local spaceRatioOverride = {}

-- Move a window to a target screen then apply exact frame.
-- moveToScreen handles the cross-display hop; setFrame sets exact geometry.
local function placeWindow(win, screen, frame, tag)
  if not win then return end
  win:moveToScreen(screen, false, false, 0)
  win:setFrame(frame, 0)
  local got   = win:frame()
  local delta = math.abs(got.x - frame.x) + math.abs(got.y - frame.y)
              + math.abs(got.w - frame.w) + math.abs(got.h - frame.h)
  if delta > 2 then
    log.w(string.format("place %s [%s]: want %d,%d %dx%d got %d,%d %dx%d",
      tag or "?", (win:application() and win:application():name()) or "?",
      frame.x, frame.y, frame.w, frame.h, got.x, got.y, got.w, got.h))
  end
end

local pairBundleSet = {}
for _, space in pairs(config.spaces or {}) do
  if space.layout then
    for _, bundleId in ipairs(space.apps or {}) do
      pairBundleSet[bundleId] = true
    end
  end
end

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
    local cf        = changedWin:frame()
    local mainFr    = { x = sf.x, y = sf.y, w = cf.x - sf.x,        h = sf.h }
    local sidebarFr = { x = cf.x, y = sf.y, w = sf.x + sf.w - cf.x, h = sf.h }
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
    _G.pairFilter:subscribe(hs.window.filter.windowMoved, syncLayout)
    _G.pairFilter:subscribe(hs.window.filter.windowDestroyed, function()
      if not tiledMain then return end
      local screen    = tiledMain:screen()
      if not screen   then return end
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

applyLayout = function(spaceName, screen)
  tiledMain = nil
  local space  = config.spaces and config.spaces[spaceName]
  local layout = space and space.layout
  if not layout or layout.type ~= "sidebars" then
    for bundleId in pairs(effectiveApps(spaceName)) do
      local app = hs.application.get(bundleId)
      local win = app and app:mainWindow()
      if win and win:isVisible() then win:moveToScreen(screen, false, false, 0) end
    end
    return
  end

  local ratio   = spaceRatioOverride[spaceName] or layout.ratio or 0.65
  local sf      = screen:frame()
  local mainApp = layout.main and hs.application.get(layout.main)
  local mainWin = mainApp and mainApp:mainWindow()
  local mainVis = mainWin and mainWin:isVisible()

  local sidebarWins = {}
  for bundleId in pairs(effectiveApps(spaceName)) do
    if bundleId ~= layout.main then
      local app = hs.application.get(bundleId)
      local win = app and app:mainWindow()
      if win and win:isVisible() then table.insert(sidebarWins, win) end
    end
  end

  local mainFr    = { x = sf.x,                y = sf.y, w = sf.w * ratio,       h = sf.h }
  local sidebarFr = { x = sf.x + sf.w * ratio, y = sf.y, w = sf.w * (1 - ratio), h = sf.h }

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
        local scr, _ = resolveSpaceScreen(spaceName)
        if activeSpace[scr:id()] == spaceName then applyLayout(spaceName, scr) end
      end
    end
  end)
end)
_G.pairAppWatcher:start()

ensurePairFilter()

-- ---------------------------------------------------------------------------
-- Status overlay (cmd+alt+space)
-- ---------------------------------------------------------------------------

local STATUS_STYLE = ALERT_STYLE

local function buildStatusText()
  local maxNameLen = 0
  for _, hk in ipairs(config.hotkeys or {}) do
    if #hk.space > maxNameLen then maxNameLen = #hk.space end
  end

  local function row(key, name, screenName, active)
    local pad    = string.rep(" ", maxNameLen - #name)
    local marker = active and "  ◀" or ""
    return "  " .. key .. "   " .. name .. pad .. "   [" .. screenName .. "]" .. marker
  end

  -- One line per unique physical screen currently present
  local chain   = config.screens and config.screens.precedence or {}
  local seenId  = {}
  local activeLines = {}
  for _, screenName in ipairs(chain) do
    local s = screensByName[screenName]
    if s and not seenId[s:id()] then
      seenId[s:id()] = true
      local sName = activeSpace[s:id()] or "(none)"
      local pad   = string.rep(" ", 18 - #screenName)
      table.insert(activeLines, "  " .. screenName .. pad .. sName)
    end
  end

  local sep   = "  " .. string.rep("─", maxNameLen + 20)
  local lines = { "  " .. screenSetLabel(), sep }
  for _, l in ipairs(activeLines) do table.insert(lines, l) end
  table.insert(lines, sep)

  for _, hk in ipairs(config.hotkeys or {}) do
    local _, screenName = resolveSpaceScreen(hk.space)
    local screen        = screensByName[screenName]
    local active        = screen and activeSpace[screen:id()] == hk.space
    table.insert(lines, row(hk.key, hk.space, screenName, active))
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
  if statusAlertId then hs.alert.closeAll(0); statusAlertId = nil; return end
  openStatus()
end

-- ---------------------------------------------------------------------------
-- Diagnostics (cmd+alt+z)
-- ---------------------------------------------------------------------------

local function dumpDiagnostics()
  print("")
  print("=== TILR DIAGNOSTICS ===")
  print("setup: " .. screenSetLabel())
  print("")
  print("--- screens (all) ---")
  for _, s in ipairs(hs.screen.allScreens()) do
    local f = s:frame()
    print(string.format("  %-28s id=%-12d uuid=%s  x=%d y=%d w=%d h=%d",
      s:name() or "?", s:id(), s:getUUID() or "-", f.x, f.y, f.w, f.h))
  end
  print("")
  print("--- screensByName ---")
  for name, s in pairs(screensByName) do
    print(string.format("  %-20s → %s (id=%d)", name, s:name() or "?", s:id()))
  end
  print("")
  print("--- activeSpace ---")
  for screenId, sName in pairs(activeSpace) do
    print(string.format("  screen id=%-12d → %s", screenId, sName))
  end
  print("")
  print("--- spaces (resolved screen) ---")
  for spaceName, space in pairs(config.spaces or {}) do
    local scr, scrName = resolveSpaceScreen(spaceName)
    local isActive = activeSpace[scr:id()] == spaceName
    print(string.format("  %-12s → %-20s (%s)%s",
      spaceName, scrName, scr:name() or "?", isActive and "  [ACTIVE]" or ""))
  end
  print("")
  print("--- sessionScreenOverride ---")
  if next(sessionScreenOverride) then
    for space, name in pairs(sessionScreenOverride) do
      print(string.format("  %s → %s", space, name))
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
    print(string.format("focused: %s [%s]", win:title() or "?",
      (win:application() and win:application():name()) or "?"))
    print(string.format("  frame:  x=%d y=%d w=%d h=%d", f.x, f.y, f.w, f.h))
    print(string.format("  screen: %s (id=%d)", s and s:name() or "?", s and s:id() or -1))
  else
    print("focused: (none)")
  end
  print("========================")
  alert("Diagnostics → console")
end

-- ---------------------------------------------------------------------------
-- Hotkeys
-- ---------------------------------------------------------------------------

local function parseMods(str)
  local mods = {}
  for m in string.gmatch(str or "", "%S+") do table.insert(mods, m) end
  return mods
end

if _G.boundHotkeys then
  for _, hk in ipairs(_G.boundHotkeys) do hk:delete() end
end
_G.boundHotkeys = {}

local function bindKey(mods, key, fn)
  table.insert(_G.boundHotkeys, hs.hotkey.bind(mods, key, fn))
end

bindKey({ "cmd", "alt" }, "z", dumpDiagnostics)

for _, hk in ipairs(config.hotkeys or {}) do
  bindKey(parseMods(hk.mods), hk.key, function() activateSpace(hk.space) end)
end

-- Move focused app to a different space (opt+shift+key).
local function moveFocusedAppToSpace(targetSpaceName)
  local win = hs.window.focusedWindow()
  if not win then alert("no focused window"); return end
  local app      = win:application()
  if not app then return end
  local bundleId = app:bundleID()
  if not bundleId then return end
  local appName     = app:name() or bundleId
  local targetSpace = config.spaces and config.spaces[targetSpaceName]

  sessionAppOverride[bundleId] = targetSpaceName
  log.i(string.format("session move: %s → %s", bundleId, targetSpaceName))

  if targetSpace and targetSpace.layout then
    pairBundleSet[bundleId] = true
    ensurePairFilter()
  end

  activateSpace(targetSpaceName)

  if not (targetSpace and targetSpace.layout and targetSpace.layout.type == "sidebars") then
    hs.timer.doAfter(0.35, function()
      local movedApp = hs.application.get(bundleId)
      local movedWin = movedApp and movedApp:mainWindow()
      if movedWin and movedWin:isVisible() then
        local targetScreen = resolveSpaceScreen(targetSpaceName)
        placeWindow(movedWin, targetScreen, targetScreen:frame(), "moved-fill")
      end
    end)
  end

  alert("Moved " .. appName .. " → " .. targetSpaceName)
end

for _, hk in ipairs(config.hotkeys or {}) do
  bindKey(parseMods("alt shift"), hk.key, function() moveFocusedAppToSpace(hk.space) end)
end

-- Move active space to a named screen (cmd+shift+1/2/3).
local SCREEN_FOR_KEY = {
  ["1"] = "external-main",
  ["2"] = "external-secondary",
  ["3"] = "laptop",
}

local function moveActiveSpaceToScreen(targetScreenName)
  local win       = hs.window.focusedWindow()
  local screen    = (win and win:screen()) or hs.screen.mainScreen()
  local spaceName = activeSpace[screen:id()]
  if not spaceName then alert("no active space here"); return end
  local targetScreen = screensByName[targetScreenName]
  if not targetScreen then alert(targetScreenName .. " not connected"); return end
  if targetScreen:id() == screen:id() then
    alert(spaceName .. " already on " .. targetScreenName); return
  end
  sessionScreenOverride[spaceName] = targetScreenName
  log.i(string.format("space move: %s → %s", spaceName, targetScreenName))
  activateSpace(spaceName)
  alert(spaceName .. " → " .. targetScreenName)
end

for key, screenName in pairs(SCREEN_FOR_KEY) do
  bindKey({ "cmd", "shift" }, key, function() moveActiveSpaceToScreen(screenName) end)
end

bindKey({ "cmd", "alt" }, "space", showStatus)

-- ---------------------------------------------------------------------------
-- Reactivity — screen connect / disconnect
-- ---------------------------------------------------------------------------

-- Activate defaults for a single screen name (called on connect).
local function applyDefaultForScreen(screenName)
  local isLaptopOnly = not screensByName["external-main"]
                    and not screensByName["external-secondary"]
  local defaults = (isLaptopOnly and config.defaults and config.defaults["laptop-only"])
                or (config.defaults)
  if not defaults then return end
  local spaceName = defaults[screenName]
  if spaceName and spaceName ~= "" and config.spaces and config.spaces[spaceName] then
    activateSpace(spaceName, screenName)
  end
end

-- On unplug of removedName: migrate spaces, apply direction-aware takeover rule.
local function handleScreenRemoved(removedName, prevByName)
  local removedScreen = prevByName[removedName]
  if not removedScreen then return end

  local removedId      = removedScreen:id()
  local activeOnRemoved = activeSpace[removedId]
  activeSpace[removedId] = nil

  -- Clear session overrides that pointed to the removed screen
  for spaceName, name in pairs(sessionScreenOverride) do
    if name == removedName then sessionScreenOverride[spaceName] = nil end
  end

  if not activeOnRemoved then return end

  -- Where does the formerly-active space land now?
  local _, destName = resolveSpaceScreen(activeOnRemoved)
  local goingDown   = precedenceOf(removedName) < precedenceOf(destName)

  if goingDown then
    -- Active space travels: take over the destination
    log.i(string.format("screen removed: %s (DOWN) → %s takes over %s",
      removedName, activeOnRemoved, destName))
    activateSpace(activeOnRemoved)
  else
    -- Active space migrates up: hide its apps, leave destination alone
    log.i(string.format("screen removed: %s (UP) → %s hides on %s",
      removedName, activeOnRemoved, destName))
    for bundleId in pairs(effectiveApps(activeOnRemoved)) do
      local app = hs.application.get(bundleId)
      if app and not app:isHidden() then app:hide() end
    end
  end
end

-- When an app is activated (e.g. via alt-tab), check if it belongs to a
-- different space and activate that space automatically.
local activatingSpace = false

_G.focusWatcher = hs.application.watcher.new(function(appName, eventType, app)
  if eventType ~= hs.application.watcher.activated then return end
  if activatingSpace then return end
  if not app then return end
  local bundleId = app:bundleID()
  if not bundleId or EXEMPT[bundleId] then return end
  if bundleId == "org.hammerspoon.Hammerspoon" then return end

  -- Find which space owns this app
  local targetSpace = sessionAppOverride[bundleId]
  if not targetSpace then
    for spaceName in pairs(config.spaces or {}) do
      if effectiveApps(spaceName)[bundleId] then targetSpace = spaceName; break end
    end
  end
  if not targetSpace then return end

  -- Already active on its screen — nothing to do
  local screen = resolveSpaceScreen(targetSpace)
  if activeSpace[screen:id()] == targetSpace then return end

  log.i("focus: " .. appName .. " → activating " .. targetSpace)
  activatingSpace = true
  activateSpace(targetSpace)
  hs.timer.doAfter(0.5, function() activatingSpace = false end)
end)
_G.focusWatcher:start()

_G.screenWatcher = hs.screen.watcher.new(function()
  local prevByName = screensByName
  screensByName    = buildScreensByName()

  -- Handle removed screens
  for name in pairs(prevByName) do
    if not screensByName[name] then
      handleScreenRemoved(name, prevByName)
    end
  end

  -- Handle added screens
  for name in pairs(screensByName) do
    if not prevByName[name] then
      log.i("screen added: " .. name)
      applyDefaultForScreen(name)
    end
  end

  alert(screenSetLabel())
end)
_G.screenWatcher:start()

-- Config reload
bindKey({ "cmd", "alt" }, "r", function() hs.reload() end)

-- Apply startup defaults
local function applyStartupDefaults()
  local isLaptopOnly = not screensByName["external-main"]
                    and not screensByName["external-secondary"]
  local defaults = (isLaptopOnly and config.defaults and config.defaults["laptop-only"])
                or config.defaults
  if not defaults then return end
  local seenScreenId = {}
  local chain = config.screens and config.screens.precedence or {}
  for _, screenName in ipairs(chain) do
    local spaceName = defaults[screenName]
    if spaceName and spaceName ~= "" and config.spaces and config.spaces[spaceName] then
      local s = screensByName[screenName]
      if s and not seenScreenId[s:id()] then
        seenScreenId[s:id()] = true
        activateSpace(spaceName, screenName)
      end
    end
  end
end

applyStartupDefaults()

alert("Tilr — " .. screenSetLabel())
log.i("init.lua loaded; setup=" .. screenSetLabel())
