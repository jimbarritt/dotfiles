-- ~/.hammerspoon/init.lua
-- Auto-tile PoC: tile apps beside Ghostty with linked resize

local wf = hs.window.filter
local log = hs.logger.new('autotile', 'info')

-- Make hs.application.find('Ghostty') and friends search via Spotlight's
-- index instead of only walking hs.application.runningApplications(). More
-- reliable for apps that aren't currently frontmost and slightly faster.
hs.application.enableSpotlightForNameSearches(true)

-- ---------------------------------------------------------------------------
-- Rolling backups of this config
--
-- Runs on every reload. If init.lua has changed since the last backup,
-- snapshot it to init.lua.backup.1 and rotate previous backups down to .5.
-- Semantics: .1 = current state, .2 = state one reload ago, ..., .5 = four
-- reloads ago. If the current reload breaks something, recover from .2.
-- Identical reloads are a no-op so we don't fill the ring with duplicates.
-- ---------------------------------------------------------------------------
local function rotateConfigBackups()
  local config = hs.configdir .. '/init.lua'
  local max_backups = 5

  local function readFile(path)
    local f = io.open(path, 'r')
    if not f then return nil end
    local content = f:read('*a')
    f:close()
    return content
  end

  local current = readFile(config)
  if not current then return end

  if readFile(config .. '.backup.1') == current then return end

  for i = max_backups - 1, 1, -1 do
    local src = config .. '.backup.' .. i
    if hs.fs.attributes(src) then
      os.rename(src, config .. '.backup.' .. (i + 1))
    end
  end

  local f = io.open(config .. '.backup.1', 'w')
  if f then
    f:write(current)
    f:close()
    log.i('Snapshotted init.lua to backup.1 (previous backups rotated)')
  else
    log.w('Failed to write init.lua.backup.1')
  end
end

rotateConfigBackups()

-- Apps that should auto-tile beside Ghostty when opened.
-- App names must match exactly what hs.window.filter sees — check with
-- `hs.application.runningApplications()` in the Hammerspoon console if
-- a new entry doesn't trigger.
local tile_apps = {
  'Marq',
  'Zen',
  'Ghostty',
}

-- Fraction of screen width given to Ghostty on initial tile (0..1)
local ghostty_ratio = 0.60

-- Track which windows are currently tiled together
local tiled_pair = nil -- { left = hs.window, right = hs.window }
local ignoring_resize = false

local function syncPair(changed_win)
  if not tiled_pair or ignoring_resize then return end

  local left = tiled_pair.left
  local right = tiled_pair.right
  if not left:isVisible() or not right:isVisible() then return end

  local screen = left:screen():frame()

  ignoring_resize = true

  if changed_win:id() == left:id() then
    -- Left was resized — right fills remaining space
    local lf = left:frame()
    right:setFrame({
      x = lf.x + lf.w,
      y = screen.y,
      w = screen.x + screen.w - (lf.x + lf.w),
      h = screen.h,
    })
  elseif changed_win:id() == right:id() then
    -- Right was resized — left fills remaining space
    local rf = right:frame()
    left:setFrame({
      x = screen.x,
      y = screen.y,
      w = rf.x - screen.x,
      h = screen.h,
    })
  end

  -- Delay clearing the flag so the setFrame-triggered
  -- resize event doesn't cause a loop
  hs.timer.doAfter(0.5, function()
    ignoring_resize = false
  end)
end

local function tileNextToGhostty(win)
  local ghostty = hs.application.find('Ghostty')
  if not ghostty then
    log.i('Ghostty not running, skipping tile')
    return
  end

  local ghostty_win = ghostty:mainWindow()

  -- If Ghostty itself triggered the event, look for a visible tile partner instead.
  if ghostty_win and win:id() == ghostty_win:id() then
    for _, app_name in ipairs(tile_apps) do
      if app_name ~= 'Ghostty' then
        local app = hs.application.find(app_name)
        local partner = app and app:mainWindow()
        if partner and partner:isVisible() then
          log.i('Ghostty fired windowVisible — tiling with ' .. app_name)
          win = partner
          break
        end
      end
    end
    if win:id() == ghostty_win:id() then
      log.i('Ghostty fired windowVisible but no visible tile partner found')
      return
    end
  end

  if not ghostty_win then
    log.i('No Ghostty window found, skipping tile')
    return
  end

  -- FlashSpace hides apps (via NSRunningApplication.hide) when switching
  -- workspaces — it does NOT use native macOS Spaces. So the correct
  -- "should we tile?" test is "is Ghostty currently visible", not anything
  -- based on hs.spaces.focusedSpace() / windowSpaces(). Those APIs only
  -- observe native Mission Control spaces and would always report the
  -- same space regardless of which FlashSpace workspace is active.
  if not ghostty_win:isVisible() then
    log.i('Skipping tile: Ghostty is hidden'
      .. ' (likely another FlashSpace workspace is active)')
    return
  end

  if not win:isVisible() then
    local _n = win:application() and win:application():name() or '(app unavailable)'
    log.i('Skipping tile: ' .. _n .. ' is not visible')
    return
  end

  local app_name = (win:application() and win:application():name()) or '(app unavailable)'

  local screen = ghostty_win:screen():frame()
  local left_w = screen.w * ghostty_ratio

  local left_frame = {
    x = screen.x,
    y = screen.y,
    w = left_w,
    h = screen.h,
  }
  local right_frame = {
    x = screen.x + left_w,
    y = screen.y,
    w = screen.w - left_w,
    h = screen.h,
  }

  ignoring_resize = true

  ghostty_win:setFrame(left_frame)
  win:setFrame(right_frame)

  tiled_pair = { left = ghostty_win, right = win }
  log.i('Tiled ' .. app_name .. ' to the right of Ghostty')

  hs.timer.doAfter(0.5, function()
    ignoring_resize = false
  end)
end

-- ---------------------------------------------------------------------------
-- FlashSpace integration: tile-now URL handler
--
-- FlashSpace calls runScriptAfterWorkspaceChange = 'open -g "hammerspoon://tile-now"'
-- after every workspace switch, once all apps are shown/hidden. This is more
-- reliable than windowVisible events, which race against FlashSpace's hide/unhide
-- sequence.
-- ---------------------------------------------------------------------------
hs.urlevent.bind('tile-now', function()
  log.i('tile-now: workspace switch complete, scanning for tile partners')
  local ghostty = hs.application.find('Ghostty')
  local ghostty_win = ghostty and ghostty:mainWindow()
  if not ghostty_win or not ghostty_win:isVisible() then
    -- Ghostty not on this workspace — expand any visible tile app to full screen.
    for _, app_name in ipairs(tile_apps) do
      if app_name ~= 'Ghostty' then
        local app = hs.application.find(app_name)
        local win = app and app:mainWindow()
        if win and win:isVisible() then
          local screen = win:screen():frame()
          log.i('tile-now: ' .. app_name .. ' is alone, expanding to full screen')
          tiled_pair = nil
          win:setFrame(screen)
        end
      end
    end
    return
  end
  for _, app_name in ipairs(tile_apps) do
    if app_name ~= 'Ghostty' then
      local app = hs.application.find(app_name)
      local win = app and app:mainWindow()
      if win and win:isVisible() then
        log.i('tile-now: tiling ' .. app_name .. ' next to Ghostty')
        tileNextToGhostty(win)
        return
      end
    end
  end
  -- Ghostty is visible but alone — expand to full screen.
  log.i('tile-now: Ghostty is alone, expanding to full screen')
  tiled_pair = nil
  local screen = ghostty_win:screen():frame()
  ghostty_win:setFrame(screen)
end)

-- Watch tile_apps for new windows (triggers auto-tile) and close (clears pair).
-- Built from an empty filter with explicit per-app rules, using
-- `allowRoles = '*'` so apps like Zen (Firefox-based, reports non-standard
-- AXRoles) aren't silently excluded by the default AXStandardWindow filter.
local tile_filter = wf.new(false)
for _, app in ipairs(tile_apps) do
  tile_filter:setAppFilter(app, {
    allowRoles = '*',
    visible = true,
  })
end

tile_filter:subscribe(wf.windowVisible, function(win)
  if not win then return end
  hs.timer.doAfter(0.3, function()
    tileNextToGhostty(win)
  end)
end)

tile_filter:subscribe(wf.windowNotVisible, function(win)
  if tiled_pair and tiled_pair.right:id() == win:id() then
    local left = tiled_pair.left
    tiled_pair = nil

    -- Expand the left window (Ghostty) to fill the whole screen.
    -- Guard syncPair from reacting to the setFrame event.
    if left and left:isVisible() then
      local screen = left:screen():frame()
      ignoring_resize = true
      left:setFrame(screen)
      hs.timer.doAfter(0.5, function()
        ignoring_resize = false
      end)
      log.i('Tiled pair cleared, left window expanded to full screen')
    else
      log.i('Tiled pair cleared (left window no longer visible)')
    end
  end
end)

-- Watch for either window being moved or resized (linked resize).
-- hs.window.filter has no separate resize event — windowMoved fires for both.
-- Same role-permissiveness as tile_filter so Zen-style apps are tracked.
local resize_filter = wf.new(false)
resize_filter:setAppFilter('Ghostty', { allowRoles = '*' })
for _, app in ipairs(tile_apps) do
  resize_filter:setAppFilter(app, { allowRoles = '*' })
end

resize_filter:subscribe(wf.windowMoved, function(win)
  syncPair(win)
end)

-- Hot-reload on config changes. Only reacts to `.lua` files, so the
-- rolling `.backup.N` snapshots written by rotateConfigBackups() are
-- ignored and don't cause a reload loop.
--
-- Stored in a global (not local) so Lua's GC can't collect the watcher
-- between reloads — a known Hammerspoon gotcha with pathwatcher/timer.
_G.config_watcher = hs.pathwatcher.new(hs.configdir, function(paths)
  for _, path in ipairs(paths) do
    if path:match('%.lua$') then
      log.i('Config changed (' .. path .. '), reloading')
      hs.reload()
      return
    end
  end
end):start()

log.i('Auto-tile PoC loaded')

-- ---------------------------------------------------------------------------
-- Toast notification (FlashSpace-style)
--
-- Dark translucent pill at the bottom-centre of the main screen, positioned
-- with a gap of 7% of screen height above the bottom. Fades in and out.
-- ---------------------------------------------------------------------------
local function showToast(message)
  local screen = hs.screen.mainScreen():frame()

  local font = 'Courier New Bold'
  local fontSize = 22
  local hPad = 40
  local vPad = 20

  local textStyle = {
    font = { name = font, size = fontSize },
    color = { white = 1, alpha = 1 },
    paragraphStyle = { alignment = 'center' },
  }

  local measured = hs.drawing.getTextDrawingSize(message, textStyle)
  local w = math.ceil(measured.w) + hPad * 2
  local h = math.ceil(measured.h) + vPad * 2

  local bottomGap = screen.h * 0.07
  local x = screen.x + (screen.w - w) / 2
  local y = screen.y + screen.h - h - bottomGap

  local canvas = hs.canvas.new({ x = x, y = y, w = w, h = h })
  canvas:level('overlay')
  canvas:appendElements({
    {
      type = 'rectangle',
      action = 'strokeAndFill',
      fillColor = { red = 0.10, green = 0.10, blue = 0.12, alpha = 0.40 },
      strokeColor = { red = 0.05, green = 0.25, blue = 0.10, alpha = 1.0 },
      strokeWidth = 4,
      roundedRectRadii = { xRadius = 10, yRadius = 10 },
    },
    {
      type = 'text',
      text = message,
      textFont = font,
      textSize = fontSize,
      textColor = { red = 0.10, green = 0.38, blue = 0.18, alpha = 1.0 },
      textAlignment = 'center',
      frame = { x = hPad, y = vPad, w = measured.w, h = measured.h },
    },
  })

  canvas:show(0.2)
  hs.timer.doAfter(3.0, function()
    canvas:hide(0.3)
    hs.timer.doAfter(0.5, function()
      canvas:delete()
    end)
  end)
end

-- On-screen confirmation that the reload completed successfully.
-- Shown on every load, so it acts as both a startup and a reload notifier.
showToast('Hammerspoon reloaded')
