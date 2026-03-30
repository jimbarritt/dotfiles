# Vimium C Setup (Zen Browser)

## Installing Vimium C

1. Open Zen Browser
2. Go to `about:addons` (or Menu > Add-ons and Themes)
3. Search for "Vimium C" and install it
4. Click the Vimium C icon in the toolbar, then "Options" (or go to `about:addons` > Vimium C > Preferences)

## Custom Key Mappings

In Vimium C Options, scroll to **Custom key mappings** and add:

```
unmap T
map gt Vomnibar.activateTabSelection
```

This replaces the default `T` with a vim-style `gt` to open the tab switcher.

## Light Theme for Vomnibar

The Vomnibar popup doesn't follow the Zen browser theme — it defaults to
dark regardless of your browser theme. To force it to light mode:

1. In Vimium C Options, scroll to **Custom CSS for Vomnibar**
2. Paste the following CSS:

```css
body {
  background: #ffffff;
  color: #1f2328;
}
#list .item {
  color: #1f2328;
}
#list .item.selected {
  background: #eaeef2;
}
input {
  background: #f6f8fa;
  color: #1f2328;
}
```

3. Click **Save** at the bottom of the page

These colours are from the GitHub Light / Primer palette to match the
nvim `github-light` colorscheme. The key field is specifically labelled
"Custom CSS for Vomnibar" — not the general "Custom CSS" field above it.
