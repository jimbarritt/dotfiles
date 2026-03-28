-- Green Tinted Light — companion to green-tinted (dark), inverted for light bg
vim.g.colors_name = "green-tinted-light"
package.loaded["colorscheme.palettes.green-light"] = nil
package.loaded["colorscheme.mapping"] = nil
package.loaded["colorscheme.apply"] = nil
local palette = require("colorscheme.palettes.green-light")
require("colorscheme.apply").apply(palette, { suppress_style = true })
