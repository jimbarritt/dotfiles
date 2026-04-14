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

local activeSpace = {}  -- screenId → spaceName  (one entry per physical screen)
local EXEMPT = { ["com.apple.finder"] = true }

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

  -- Hide every config-managed app not in the visible set
  for _, other in pairs(config.spaces or {}) do
    for _, bundleId in ipairs(other.apps or {}) do
      if not shouldShow[bundleId] and not EXEMPT[bundleId] then
        local app = hs.application.get(bundleId)
        if app then
          log.i("hiding: " .. bundleId .. " (" .. app:name() .. ")")
          app:hide()
        else
          log.w("not found: " .. bundleId)
        end
      end
    end
  end

  alert(spaceName .. " [" .. label .. "]")
end

-- ---------------------------------------------------------------------------
-- Status overlay (cmd+opt+space) — shows active space per display.
-- Placeholder for the eventual full space-picker overlay.
-- ---------------------------------------------------------------------------

local statusAlertId = nil
local STATUS_STYLE  = ALERT_STYLE

local function showStatus()
  if statusAlertId then
    hs.alert.closeSpecific(statusAlertId)
    statusAlertId = nil
    return
  end

  -- Measure longest space name for column alignment
  local maxNameLen = 0
  for _, hk in ipairs(config.hotkeys or {}) do
    if #hk.space > maxNameLen then maxNameLen = #hk.space end
  end

  local function row(key, name, label, active)
    local pad    = string.rep(" ", maxNameLen - #name)
    local marker = active and "  ◀" or ""
    return "  " .. key .. "   " .. name .. pad .. "   " .. label .. marker
  end

  -- Active-spaces summary: one line per unique physical screen.
  -- On laptop-only multiple roles collapse to the same screen, so search
  -- all roles that point to it to find whichever one has an active space.
  local seenScreenId = {}
  local activeLines  = {}
  for _, role in ipairs(ROLE_PRIORITY) do
    local screen = roleToScreen[role]
    if screen and not seenScreenId[screen:id()] then
      seenScreenId[screen:id()] = true
      local label = displayLabel(screen)
      local sName = activeSpace[screen:id()] or "(none)"
      local pad = string.rep(" ", 6 - #label)
      table.insert(activeLines, "  " .. label .. pad .. "  " .. sName)
    end
  end

  local sep = "  " .. string.rep("─", maxNameLen + 16)
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

  statusAlertId = hs.alert.show(
    table.concat(lines, "\n"),
    STATUS_STYLE,
    hs.mouse.getCurrentScreen(),
    math.huge
  )
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
