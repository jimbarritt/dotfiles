# Zen Browser Config in Dotfiles

How to capture Zen Browser configuration into dotfiles on macOS.

## Where Zen stores things

On macOS, Zen lives under:

```
~/Library/Application Support/zen/
├── installs.ini
├── profiles.ini                    # which profile is default
├── Profile Groups/
└── Profiles/
    ├── 0fv3kpnx.Ubiqtek/
    ├── 4mqlrsrd.Default (release)/
    └── r8igpii2.Default Profile/
```

To find the profile folder from inside Zen, open `about:support` and click "Show in Finder" next to the profile directory entry, or open `about:profiles` for the full list.

## What's inside a profile

| File | What it is | Dotfile it? |
|---|---|---|
| `prefs.js` | All `about:config` settings (incl. `zen.*`) | Partial — see below |
| `user.js` | User-owned overrides, applied on top of `prefs.js` at startup | **Yes — this is the one** |
| `chrome/userChrome.css` | Custom browser UI CSS | Yes |
| `chrome/userContent.css` | Custom webpage CSS | Yes |
| `containers.json` | Container tab definitions | Maybe |
| `handlers.json` | Default app/protocol handlers | Maybe |
| `search.json.mozlz4` | Search engines (binary, compressed) | No |
| `extensions.json`, `extension-preferences.json`, `extensions/` | Installed addons | No — reinstall instead |
| `places.sqlite`, `cookies.sqlite`, `sessionstore.jsonlz4` | History / cookies / open tabs | No (runtime state) |

## The right approach: `user.js`

Don't track `prefs.js` — Zen rewrites it constantly and it contains runtime junk (window positions, last-used timestamps, telemetry IDs, crash state). Instead:

1. Create a `user.js` file in your profile folder (it doesn't exist by default).
2. Put only the prefs you care about in it, e.g.:

   ```js
   // Thinner content border (default is 8)
   user_pref("zen.theme.content-element-separation", 2);

   // Hide the tab bar in compact mode
   user_pref("zen.view.compact.hide-tabbar", true);
   ```

3. Zen reads `user.js` on every startup and applies those values, overriding whatever's in `prefs.js`.

This is the same pattern as [Betterfox](https://github.com/yokoffing/Betterfox) and [better-zen](https://github.com/Codextor/better-zen), both of which ship a curated `user.js` you drop into the profile folder.

## Suggested dotfiles layout

```
dotfiles/zen/
├── user.js
└── chrome/
    ├── userChrome.css
    └── userContent.css
```

Then symlink into the active profile (substitute the profile directory name for your install):

```bash
PROFILE="$HOME/Library/Application Support/zen/Profiles/4mqlrsrd.Default (release)"

ln -s ~/projects/dotfiles/zen/user.js    "$PROFILE/user.js"
ln -s ~/projects/dotfiles/zen/chrome     "$PROFILE/chrome"
```

## Extracting current prefs into `user.js`

Open `about:config` and filter by `zen.` — anything shown bold / modified is a user-set value. Or from the terminal:

```bash
rg '^user_pref\("zen\.' \
  "$HOME/Library/Application Support/zen/Profiles/4mqlrsrd.Default (release)/prefs.js"
```

That gives you a starting list of Zen-specific customizations. Copy the ones you want to keep into `user.js`.

## Gotcha: don't move the profile directory

The Zen devs note that **relocating the profile directory itself breaks the browser** (see [zen-browser/desktop#11432](https://github.com/zen-browser/desktop/issues/11432)). Symlink individual files (`user.js`, `chrome/`) into the profile — don't move or symlink `Profiles/` itself.

## Example: removing the thick window border

The thick padding around the content area is controlled by:

```
zen.theme.content-element-separation
```

- Default: `8` (pixels)
- Set to `0` to remove entirely
- `2` is a nice subtle gap

Drop it into `user.js`:

```js
user_pref("zen.theme.content-element-separation", 2);
```

Restart Zen (or toggle the value in `about:config` to apply immediately).

## References

- [Managing Firefox Profiles — Zen Docs](https://docs.zen-browser.app/guides/manage-profiles)
- [Hidden/Advanced Preferences — Zen Docs](https://docs.zen-browser.app/guides/about-config-flags)
- [Live Editing Zen Theme — Zen Docs](https://docs.zen-browser.app/guides/live-editing)
- [better-zen — example curated user.js](https://github.com/Codextor/better-zen)
- [Betterfox — upstream Firefox user.js project](https://github.com/yokoffing/Betterfox)
- [Profile relocation issue #11432](https://github.com/zen-browser/desktop/issues/11432)
