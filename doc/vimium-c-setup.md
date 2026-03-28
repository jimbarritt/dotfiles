# Vimium C Setup (Zen Browser)

## Custom Key Mappings

Replace the default `T` tab browser with a vim-style `gt` combo:

```
unmap T
map gt Vomnibar.activateTabSelection
```

Add this in Vimium C settings under "Custom key mappings".

## Light Theme for Vomnibar

The Vomnibar popup doesn't automatically follow the Zen browser theme.
To match a light colour scheme, add this under "Custom CSS for Vomnibar"
in Vimium C settings:

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

These colours are taken from the GitHub Light / Primer palette to match
the nvim `github-light` colorscheme.
