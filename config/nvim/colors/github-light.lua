-- GitHub Light — faithful port of GitHub's PrettyLights syntax theme
-- Uses Primer design system colours with style suppression (no bold/italic on code)
vim.g.colors_name = "github-light"
package.loaded["colorscheme.palettes.github-light"] = nil
package.loaded["colorscheme.mapping"] = nil
package.loaded["colorscheme.apply"] = nil
local palette = require("colorscheme.palettes.github-light")
require("colorscheme.apply").apply(palette, { suppress_style = true })
