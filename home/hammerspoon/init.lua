-- ~/.hammerspoon/init.lua

local log = hs.logger.new('hs', 'info')

-- ---------------------------------------------------------------------------
-- Workspace spaces
-- (define per-space behaviour here)
-- ---------------------------------------------------------------------------


-- ---------------------------------------------------------------------------
-- Config reload
-- ---------------------------------------------------------------------------
hs.hotkey.bind({ 'cmd', 'ctrl' }, 'r', function()
  hs.reload()
end)

hs.alert.show('Hammerspoon loaded')
log.i('init.lua loaded')
