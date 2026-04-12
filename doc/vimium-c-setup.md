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

The vomnibar popup (tab switcher, URL suggestions, bookmarks) does not follow the Zen browser theme. It needs **two** settings to be light:

### 1. Turn off Auto Dark Mode (this is the critical one)

In Vimium C Options, find **Auto dark mode** under Advanced Options. Set it to:

- **Off** / value `0`

If this is left on (`1` = follow system, `2` = always on), Vimium C applies its own built-in dark theme to the vomnibar and your custom CSS will be fought/overridden. This is the setting most commonly missed — if the vomnibar keeps going dark despite pasting CSS, this is almost always why.

### 2. Paste custom CSS into the single "Custom CSS" field

Vimium C has exactly **one** custom CSS textarea, under Advanced Options. Depending on Vimium C version it's labelled something like:

> Custom CSS for Vomnibar, HUD, and Find mode

There is no separate "general" vs "vomnibar" CSS field — they're the same field, and the CSS is scoped by the selectors you use. Paste:

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

These selectors (`#list .item`, `input`) target the vomnibar's structure specifically — they don't affect link hints or other Vimium UI.

Click **Save** at the bottom of the page.

Colours are from the GitHub Light / Primer palette to match the nvim `github-light` colorscheme.

## Syncing settings across machines

Vimium C stores all of its options under `browser.storage.sync`, which piggybacks on Firefox Sync. If you sign into the same Mozilla account on multiple machines and enable Add-ons sync (`about:preferences#sync`), Vimium C options propagate automatically — no need to re-paste CSS, re-enter key mappings, or re-toggle auto dark mode on each machine.

The complete set of Vimium C settings that actually matter is small (six keys as of v2.12.3):

```
vimSync          true
keyLayout        2
exclusionRules   []
keyMappings      (the unmap T / map gt block above)
autoDarkMode     0
userDefinedCss   (the CSS block above)
```

If Firefox Sync isn't an option, treat this list as the canonical source and set it by hand on each machine.

## Where the settings live on disk

For debugging / inspection only — do not edit these files directly.

```
# Per-extension IndexedDB (storage.local)
~/Library/Application Support/zen/Profiles/<profile>/storage/default/\
  moz-extension+++<extension-uuid>^userContextId=4294967295/idb/*.sqlite

# Sync storage (storage.sync) — this one is plain JSON and readable
~/Library/Application Support/zen/Profiles/<profile>/storage-sync-v2.sqlite
```

The extension's internal UUID (different from its id `vimium-c@gdh1995.cn`) lives in the pref `extensions.webextensions.uuids` in `prefs.js`.

To dump the current sync storage as readable JSON:

```bash
sqlite3 "$HOME/Library/Application Support/zen/Profiles/<profile>/storage-sync-v2.sqlite" \
  "SELECT data FROM storage_sync_data WHERE ext_id='vimium-c@gdh1995.cn';"
```

## Troubleshooting

**Vomnibar is still dark after pasting CSS.** Check `autoDarkMode` first — it's almost always this. The CSS is fine, but Vimium C's own dark theme is layered on top.

**CSS field seems to have two sections.** It doesn't — there's one textarea. Earlier versions of this doc claimed there was a separate "Custom CSS for Vomnibar" field, which was wrong.

**Key mapping didn't take effect.** Make sure `#!no-check` is the first line of the key mappings block — it suppresses Vimium C's warnings about remapping defaults.
