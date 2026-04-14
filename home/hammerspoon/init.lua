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
    local s = config.spaces[sName]
    if s then
      for _, bundleId in ipairs(s.apps or {}) do
        shouldShow[bundleId] = true
      end
    end
  end

  log.i(string.format("activate %s [%s] (screen: %s)", spaceName, label, screen:name()))

  -- Unhide apps for the newly active space
  for _, bundleId in ipairs(space.apps or {}) do
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

local tiledPair      = nil   -- { left = win, right = win } or nil
local ignoringResize = false

-- Bundle IDs of all apps that appear in any pair layout across all spaces.
local pairBundleSet = {}
for _, space in pairs(config.spaces or {}) do
  local layout = space.layout
  if layout and layout.type == "pair" then
    if layout.left  then pairBundleSet[layout.left]  = true end
    if layout.right then pairBundleSet[layout.right] = true end
  end
end

local function syncPair(changedWin)
  if not tiledPair or ignoringResize then return end
  local left  = tiledPair.left
  local right = tiledPair.right
  if not left:isVisible() or not right:isVisible() then
    tiledPair = nil; return
  end

  local sf = left:screen():frame()
  ignoringResize = true

  if changedWin:id() == left:id() then
    local lf = left:frame()
    right:setFrame({ x = lf.x + lf.w, y = sf.y, w = sf.x + sf.w - (lf.x + lf.w), h = sf.h })
  elseif changedWin:id() == right:id() then
    local rf = right:frame()
    left:setFrame({ x = sf.x, y = sf.y, w = rf.x - sf.x, h = sf.h })
  end

  hs.timer.doAfter(0.5, function() ignoringResize = false end)
end

-- pairFilter watches running pair apps for move/close events.
-- hs.window.filter requires app display names (not bundle IDs), so it can only
-- track apps that are already running. ensurePairFilter() is called whenever a
-- pair app launches to add it. Stored in _G so the GC can't collect it.
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
    _G.pairFilter:subscribe(hs.window.filter.windowMoved, function(win) syncPair(win) end)
    _G.pairFilter:subscribe(hs.window.filter.windowDestroyed, function(win)
      if not tiledPair then return end
      local other = nil
      if     win:id() == tiledPair.right:id() then other = tiledPair.left
      elseif win:id() == tiledPair.left:id()  then other = tiledPair.right
      end
      tiledPair = nil
      if other and other:isVisible() then
        local sf = other:screen():frame()
        ignoringResize = true
        other:setFrame(sf)
        hs.timer.doAfter(0.5, function() ignoringResize = false end)
        log.i("pair member closed — expanding other to fill")
      end
    end)
  end

  -- setAppFilter is idempotent — safe to call again for already-tracked apps
  for name in pairs(names) do
    _G.pairFilter:setAppFilter(name, { allowRoles = '*' })
  end
end

-- applyLayout is forward-declared above activateSpace; defined here.
applyLayout = function(spaceName, screen)
  tiledPair = nil

  local space  = config.spaces and config.spaces[spaceName]
  local layout = space and space.layout
  if not layout or layout.type ~= "pair" then return end

  local ratio    = layout.ratio or 0.65
  local sf       = screen:frame()
  local leftApp  = layout.left  and hs.application.get(layout.left)
  local rightApp = layout.right and hs.application.get(layout.right)
  local leftWin  = leftApp  and leftApp:mainWindow()
  local rightWin = rightApp and rightApp:mainWindow()
  local leftVis  = leftWin  and leftWin:isVisible()
  local rightVis = rightWin and rightWin:isVisible()

  if leftVis and rightVis then
    ignoringResize = true
    leftWin:setFrame({  x = sf.x,                y = sf.y, w = sf.w * ratio,        h = sf.h })
    rightWin:setFrame({ x = sf.x + sf.w * ratio, y = sf.y, w = sf.w * (1 - ratio),  h = sf.h })
    tiledPair = { left = leftWin, right = rightWin }
    log.i(string.format("tiled: %s | %s @ %.0f%%", layout.left, layout.right, ratio * 100))
    hs.timer.doAfter(0.5, function() ignoringResize = false end)
  elseif leftVis then
    leftWin:setFrame(sf)
    log.i("layout: left alone → fill")
  elseif rightVis then
    rightWin:setFrame(sf)
    log.i("layout: right alone → fill")
  end
end

-- Watch for pair apps launching. hs.application.watcher fires on launch
-- regardless of whether the app was running at init — fixing the gap that
-- hs.window.filter can't cover (it can't watch for apps not yet running).
_G.pairAppWatcher = hs.application.watcher.new(function(appName, eventType, app)
  if eventType ~= hs.application.watcher.launched then return end
  if not app then return end
  local bundleId = app:bundleID()
  if not pairBundleSet[bundleId] then return end
  log.i("pair app launched: " .. appName)
  -- Give the app time to create its main window, then tile if the space is active
  hs.timer.doAfter(0.5, function()
    ensurePairFilter()
    for spaceName, space in pairs(config.spaces or {}) do
      local layout = space.layout
      if layout and layout.type == "pair" then
        if layout.left == bundleId or layout.right == bundleId then
          local scr = resolveDisplay(space.role)
          if activeSpace[scr:id()] == spaceName then
            applyLayout(spaceName, scr)
          end
        end
      end
    end
  end)
end)
_G.pairAppWatcher:start()

-- Seed the filter for any pair apps already running at load time
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
