local log = hs.logger.new('test', 'debug')

-- Write to a file since we can't easily capture stdout
local logfile = '/tmp/hs_screen_dump.txt'
local f = io.open(logfile, 'w')
if not f then error("Cannot open log file") end

local function write(msg)
  f:write(msg .. '\n')
  f:flush()
end

write("=== Hammerspoon Screen Information Dump ===\n")

local screens = hs.screen.allScreens()
write(string.format("Total screens: %d\n", #screens))

for idx, screen in ipairs(screens) do
  write(string.format("\n--- Screen %d ---", idx))
  
  local name = screen:name()
  write(string.format("\nname: %s", name or "nil"))
  
  local id = screen:id()
  write(string.format("\nid: %s", id or "nil"))
  
  local uuid = screen:getUUID()
  write(string.format("\nUUID: %s", uuid or "nil"))
  
  local frame = screen:frame()
  write(string.format("\nframe (usable): x=%.0f, y=%.0f, w=%.0f, h=%.0f", 
    frame.x, frame.y, frame.w, frame.h))
  
  local fullframe = screen:fullFrame()
  write(string.format("\nfullFrame (with dock/menu): x=%.0f, y=%.0f, w=%.0f, h=%.0f",
    fullframe.x, fullframe.y, fullframe.w, fullframe.h))
  
  local x, y = screen:position()
  write(string.format("\nposition (relative to primary): x=%d, y=%d", x, y))
  
  local info = screen:getInfo()
  if info then
    write(string.format("\ngetInfo() keys: %s", table.concat(require('inspect').getkeys(info), ', ')))
    for k, v in pairs(info) do
      write(string.format("  %s: %s", k, tostring(v)))
    end
  else
    write("\ngetInfo(): nil")
  end
  
  local currentMode = screen:currentMode()
  if currentMode then
    write(string.format("\ncurrentMode keys: %s", table.concat(require('inspect').getkeys(currentMode), ', ')))
    for k, v in pairs(currentMode) do
      write(string.format("  %s: %s", k, tostring(v)))
    end
  end
end

write("\n\n=== Screen Positions ===\n")
local positions = hs.screen.screenPositions()
for name, pos in pairs(positions) do
  write(string.format("%s: %s\n", name, hs.inspect(pos)))
end

f:close()
write("Dump complete - check " .. logfile)
