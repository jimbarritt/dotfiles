-- Green Tinted (Dark) — cognitive-load hierarchy with green monochromatic theme
vim.g.colors_name = "green-tinted"
package.loaded["colorscheme.palettes.green-dark"] = nil
package.loaded["colorscheme.mapping"] = nil
package.loaded["colorscheme.apply"] = nil
local palette = require("colorscheme.palettes.green-dark")
require("colorscheme.apply").apply(palette, { suppress_style = false, transparent = true })
