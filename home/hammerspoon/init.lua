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
  ["com.apple.finder"]            = true,
  ["org.hammerspoon.Hammerspoon"] = true,
}

-- Session-level app overrides: bundleId → spaceName.
-- Overrides which space an app is assigned to for this session only.
-- Config is ground truth and is never modified; cleared on reload.
local sessionAppOverride = {}

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
  role = role or space.role
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
    mainWin:setFrame(mainFr)
    for _, win in ipairs(sidebarWins) do win:setFrame(sidebarFr) end
    tiledMain = mainWin
    log.i(string.format("sidebars: main + %d sidebar(s) @ %.0f%%", #sidebarWins, ratio * 100))
    hs.timer.doAfter(0.5, function() ignoringResize = false end)
  elseif mainVis then
    mainWin:setFrame(sf)
    log.i("layout: main alone → fill")
  elseif #sidebarWins > 0 then
    for _, win in ipairs(sidebarWins) do win:setFrame(sf) end
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
-- Console table: spaces × profiles
-- ---------------------------------------------------------------------------

local function printSpacesTable()
  -- Build hotkey lookup: spaceName → display string e.g. "cmd+alt+3"
  local hotkeyFor = {}
  for _, hk in ipairs(config.hotkeys or {}) do
    local modsStr = hk.mods:gsub("%s+", "+")
    hotkeyFor[hk.space] = modsStr .. "+" .. hk.key
  end

  -- Collect and sort profile names: home-desk first, then rest alphabetically
  local profileNames = {}
  for name in pairs(config.profiles or {}) do
    table.insert(profileNames, name)
  end
  table.sort(profileNames, function(a, b)
    if a == "home-desk" then return true end
    if b == "home-desk" then return false end
    return a < b
  end)

  -- For a given profile and role, return the canonical display label
  local function labelInProfile(role, profileConf)
    if not profileConf then return "—" end
    local key = profileConf[role]
    if not key then return "—" end
    for _, pRole in ipairs(ROLE_PRIORITY) do
      if profileConf[pRole] == key then return pRole end
    end
    return role
  end

  -- Collect and sort space names
  local spaceNames = {}
  for name in pairs(config.spaces or {}) do table.insert(spaceNames, name) end
  table.sort(spaceNames)

  -- Column widths
  local COL_SPACE   = 14
  local COL_HOTKEY  = 14
  local COL_PROFILE = 12

  local function pad(s, w) return s .. string.rep(" ", math.max(0, w - #s)) end

  -- Header
  local header = pad("Space", COL_SPACE) .. pad("Hotkey", COL_HOTKEY)
  for _, pName in ipairs(profileNames) do
    header = header .. pad(pName, COL_PROFILE)
  end
  local sep = string.rep("-", #header)

  print(sep)
  print(header)
  print(sep)

  for _, sName in ipairs(spaceNames) do
    local space = config.spaces[sName]
    local role  = space.role or "?"
    local hk    = hotkeyFor[sName] or "—"
    local row   = pad(sName, COL_SPACE) .. pad(hk, COL_HOTKEY)
    for _, pName in ipairs(profileNames) do
      row = row .. pad(labelInProfile(role, config.profiles[pName]), COL_PROFILE)
    end
    print(row)
  end

  print(sep)
end

printSpacesTable()

print("--- roleToScreen ---")
for role, screen in pairs(roleToScreen) do
  print("  " .. role .. " → " .. screen:name() .. " (id=" .. tostring(screen:id()) .. ")")
end
print("--- activeSpace (empty until a space is switched to) ---")
for role, sName in pairs(activeSpace) do
  print("  " .. role .. " → " .. sName)
end

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
  sessionAppOverride[bundleId] = targetSpaceName
  log.i(string.format("session move: %s → %s", bundleId, targetSpaceName))
  -- If target space has a layout, register this app with the resize watcher
  local targetSpace = config.spaces and config.spaces[targetSpaceName]
  if targetSpace and targetSpace.layout then
    pairBundleSet[bundleId] = true
    ensurePairFilter()
  end
  activateSpace(targetSpaceName)
end

for _, hk in ipairs(config.hotkeys or {}) do
  hs.hotkey.bind(parseMods("alt shift"), hk.key, function()
    moveFocusedAppToSpace(hk.space)
  end)
end

hs.hotkey.bind({ "cmd", "alt" }, "space", showStatus)

-- ---------------------------------------------------------------------------
-- Reactivity
-- ---------------------------------------------------------------------------

local screenWatcher = hs.screen.watcher.new(function()
  activeProfile, roleToScreen = resolveProfile()
  alert("profile: " .. activeProfile)
end)
screenWatcher:start()

-- Config reload
hs.hotkey.bind({ "cmd", "alt" }, "r", function() hs.reload() end)

-- Populate defaults silently. Keyed by screen ID so laptop-only (all roles
-- → same screen) only sets one entry. ROLE_PRIORITY order means LAPTOP wins
-- when multiple roles share a screen.
for _, role in ipairs(ROLE_PRIORITY) do
  local spaceName = config.defaults and config.defaults[role]
  if spaceName and config.spaces and config.spaces[spaceName] then
    local screenId = resolveDisplay(role):id()
    if not activeSpace[screenId] then
      activeSpace[screenId] = spaceName
      log.i("default: " .. role .. " → " .. spaceName)
    end
  end
end

alert("Hammerspoon loaded — " .. activeProfile)
log.i("init.lua loaded; profile=" .. activeProfile)
