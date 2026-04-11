# FlashSpace — research & config notes

Replacing AeroSpace with FlashSpace (+ Hammerspoon for the bits FlashSpace doesn't do). This file is the working notebook: how FlashSpace actually works, how it maps onto the AeroSpace setup we're coming from, and the config we want to land on.

## TL;DR

- FlashSpace is **app-based**, not window-based. A workspace is a set of apps that get unhidden when you switch to it; everything else on that display gets hidden. There is no per-window membership.
- All hotkeys live in `~/.config/flashspace/profiles.toml` as `shortcut = 'cmd+opt+1'` strings on each workspace. macOS System Settings is not involved.
- Each workspace pins to one display via `display = 'Built-in Retina Display'`. FlashSpace tracks the active workspace **per display**, so two displays can have independent active workspaces simultaneously — matching AeroSpace's `workspace-to-monitor-force-assignment` behaviour.
- Display matching is by `NSScreen.localizedName` only — no EDID/serial fallback. If the monitor is missing, the workspace silently fails to activate.
- There's a full CLI at `/usr/local/bin/flashspace` we can drive from Hammerspoon.
- **Big gap vs AeroSpace**: no `move-window-to-workspace`. The closest thing is reassigning the *whole app* to a different workspace via `assignAppShortcut` or `flashspace assign-app`. We need a workaround if we want AeroSpace's `alt-shift-<num>` flow.

## Config file

- Tracked in dotfiles at `config/flashspace/`, symlinked into `~/.config/flashspace/` by `do.sh`'s `link_config flashspace`. FlashSpace reads from `~/.config/flashspace/profiles.toml` (also `settings.toml`, currently empty).
- Auto-generated `profiles-backup-*.json` snapshots are gitignored at the repo root — they're machine-local state, not config.
- Format auto-detected by probing `profiles.{json,yaml,toml}`. Hand-edits work but **GUI changes rewrite the file**, so any comments / key ordering will be lost. Pick one source of truth.
- Requires a FlashSpace restart after editing — no hot reload.
- No published schema. Ground truth is the Swift sources: [`AppSettings.swift`](https://github.com/wojciech-kulik/FlashSpace/blob/main/FlashSpace/Features/Settings/_Models/AppSettings.swift) and [`Workspace.swift`](https://github.com/wojciech-kulik/FlashSpace/blob/main/FlashSpace/Features/Workspaces/Models/Workspace.swift).
- UI state (active profile id, etc.) lives in `~/Library/Preferences/pl.wojciechkulik.FlashSpace.plist` — don't edit by hand.

## How a workspace switch actually works

1. Activating workspace `Coding` on display D:
   - All assigned apps for `Coding` are unhidden (`NSRunningApplication.unhide`).
   - All other apps currently on display D are hidden.
   - Focus goes to `appToFocus` (configurable per workspace) or the most-recently-focused assigned app.
2. Apps **not** assigned to any workspace are never managed — opening one just lays it on top of whatever's there. This is deliberate (per the README "How it works" section).
3. There is no auto-launch unless `openAppsOnActivation = true` is set on the workspace.

Implication: a workspace is best thought of as "the set of apps I want visible together", not a virtual desktop with arbitrary windows in it.

## Hotkeys

Hotkeys are stored on each workspace as a string. Recognised modifiers are **`cmd`, `ctrl`, `opt`, `shift`** — `alt` is silently dropped (it's not in `KeyModifiersMap.toModifiers`), so `cmd+alt+1` parses as `cmd+1`. Use `opt`. Example:

```toml
[[profiles.workspaces]]
name = 'Coding'
display = 'Built-in Retina Display'
shortcut = 'cmd+opt+1'           # activate this workspace
assignAppShortcut = 'cmd+opt+shift+1'  # reassign the focused app to this workspace
```

`assignAppShortcut` is the closest thing to AeroSpace's `move-node-to-workspace`, but it's app-level, not window-level: pressing it moves *every window of the focused app* into this workspace permanently.

Source: [`Workspace.swift`](https://github.com/wojciech-kulik/FlashSpace/blob/main/FlashSpace/Features/Workspaces/Models/Workspace.swift) (look for `activateShortcut` / `assignAppShortcut` — they're aliased to `shortcut` / `assignAppShortcut` via `CodingKeys`).

## Display pinning and multi-monitor

- `display = '<NSScreen localizedName>'` pins a workspace to that physical screen.
- FlashSpace stores `activeWorkspace[display]` as a per-display map. Switching to workspace `A` (pinned to the external monitor) **only** changes the external monitor — your laptop screen keeps whatever workspace it had. This is the AeroSpace behaviour we want.
- Setting `switchWorkspaceOnCursorScreen = true` overrides this so the active workspace follows the mouse instead. Default is off; we want it off.
- **Display name lookup is `localizedName` only**. There's no serial-number or EDID fallback ([issue #274](https://github.com/wojciech-kulik/FlashSpace/issues/274)). If the monitor is unplugged, activation silently no-ops. If you have two identical monitors, the recommendation is BetterDisplay to rename them ([issue #204](https://github.com/wojciech-kulik/FlashSpace/issues/204)).
- Get the names FlashSpace sees with: `flashspace list-displays`.

## HUDs / switchers

FlashSpace ships **two** built-in overlays:

1. **Workspace Switcher** — cmd-tab-style horizontal strip. Default trigger `Option+Tab`. Settings: `enableWorkspaceSwitcher`, `showWorkspaceSwitcher`, `workspaceSwitcherShowScreenshots`, `workspaceSwitcherVisibleWorkspaces`.
2. **Space Control** — Mission-Control-style grid preview. Navigate with 0–9 + arrows. Settings: `enableSpaceControl`, `showSpaceControl`, `spaceControlNumberOfColumns`. Trigger from CLI with `flashspace open-space-control`.

Both go in `settings.toml`.

## Space Control

Space Control is a Mission-Control-style grid overlay showing all workspaces. Enable it in `settings.toml`:

```toml
enableSpaceControl = true
showSpaceControl = 'cmd+opt+space'
```

### Freeing up `cmd+opt+space`

macOS binds `cmd+opt+space` to "Show Finder search window" by default. Disable it via:

```sh
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 65 \
  '<dict><key>enabled</key><false/><key>value</key><dict><key>parameters</key><array><integer>32</integer><integer>49</integer><integer>1572864</integer></array><key>type</key><string>standard</string></dict></dict>'
```

Activate without logging out:

```sh
/System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
```

The shortcut is immediately free — no restart needed.

## CLI

Full CLI at `/usr/local/bin/flashspace`. Useful subcommands:

| Command | Purpose |
|---|---|
| `flashspace workspace --number N` | activate workspace N (also `--name X`, `--next`, `--prev`, `--recent`) |
| `flashspace assign-app --workspace X --activate true` | reassign focused app to workspace X |
| `flashspace profile "Docked"` | switch profile |
| `flashspace open-space-control` | open the grid HUD |
| `flashspace get-workspace --display X` | print active workspace for a display |
| `flashspace focus --direction left\|right\|up\|down` | move focus between windows |
| `flashspace list-workspaces` / `list-displays` | enumeration |

Plus passive hooks in `settings.toml`:

```toml
runScriptOnWorkspaceChange = '...'
runScriptAfterWorkspaceChange = '...'
runScriptOnProfileChange = '...'
# env vars available: $WORKSPACE, $DISPLAY
```

Useful for Hammerspoon integration later.

## Profiles

A *profile* is a named collection of workspaces and their display assignments. Exactly one profile is active. Use cases: Laptop-only vs Docked, Work vs Personal — different physical setups need different display names and hotkeys.

**Gotcha**: switching profile doesn't auto-activate a workspace ([issue #388](https://github.com/wojciech-kulik/FlashSpace/issues/388)). You land in "no active workspace" until you trigger one.

For now we have one profile (`Default`). If we end up with a docking station / different external monitor setups, we'll want a second profile.

## Mapping AeroSpace → FlashSpace

| AeroSpace | FlashSpace | Notes |
|---|---|---|
| `cmd-alt-1` = `workspace 1` | `shortcut = 'cmd+opt+1'` per workspace | direct |
| `[workspace-to-monitor-force-assignment] 1='main', A='secondary'` | `display = '...'` per workspace | direct, but display is by name not role |
| `alt-shift-1` = `move-node-to-workspace 1` | `assignAppShortcut = 'cmd+opt+shift+1'` | **not equivalent** — moves the whole app, not the window |
| `cmd-alt-h/j/k/l` = focus left/down/up/right | `flashspace focus --direction left` | bind via Hammerspoon if wanted |
| `alt-shift-h/j/k/l` = move left/down/up/right | not supported | not a tiling WM |
| `alt-tab` = workspace-back-and-forth | `flashspace workspace --recent` (bind via Hammerspoon) | |
| tiling layouts (`tiles`, `accordion`) | not supported | this is the Hammerspoon PoC's job |
| gaps | not applicable | |

## Gaps and what to do about them

1. **No per-window move-to-workspace.** AeroSpace's `alt-shift-N` is core muscle memory. FlashSpace's app-level reassignment is *destructive* (it changes the app's home workspace, not just where this window currently lives). Options:
   - Live with `assignAppShortcut` and accept that "moving" an app permanently rebinds it.
   - Build window-level moves in Hammerspoon by directly using the macOS Spaces / window APIs, independent of FlashSpace.
   - Just don't move windows between workspaces — design workspaces around stable app sets.
2. **No tiling.** That's the Hammerspoon PoC.
3. **Display name fragility.** External monitors change `localizedName` on macOS reinstalls, OS updates, and adapter swaps. Mitigation: a second profile per physical setup, or run `flashspace list-displays` and update `display = ...` when it breaks.
4. **GUI overwrites comments in `profiles.toml`.** Decision needed: hand-edit only (and avoid the GUI), or treat the GUI as source of truth and don't try to keep comments.

## Open questions for the next session

- What's the `localizedName` of the external monitor? (`flashspace list-displays`)
- Do we want the workspace switcher HUD enabled? Which one — strip, grid, or both?
- Hand-edit `profiles.toml` or use GUI as source of truth?
- For "send window to second monitor" — is the AeroSpace flow (move *this window* to a workspace pinned to monitor 2) actually what we want, or is "switch the app's home workspace" acceptable, or do we want pure Hammerspoon `win:moveToScreen()`?
- Do we need Hammerspoon to bridge AeroSpace-style focus/move keys (`hjkl`) at all, or is mouse-based focus on FlashSpace enough?

## Proposed first-pass `profiles.toml` rewrite

Once we've answered the open questions, we can land something like this. **Don't apply yet** — the display name for the second monitor is a placeholder.

```toml
[[profiles]]
id = '119514A4-A1F6-47A9-B4AE-F7803EE36DBB'
name = 'Default'

# --- Built-in display: numbered workspaces 1-4, mirroring aerospace 1..n ---

[[profiles.workspaces]]
display = 'Built-in Retina Display'
name = 'Coding'
shortcut = 'cmd+opt+1'
assignAppShortcut = 'cmd+opt+shift+1'

  [[profiles.workspaces.apps]]
  bundleIdentifier = 'com.mitchellh.ghostty'
  name = 'Ghostty'

  [[profiles.workspaces.apps]]
  bundleIdentifier = 'com.jimbarritt.marq'
  name = 'Marq'

[[profiles.workspaces]]
display = 'Built-in Retina Display'
name = 'Browsing'
shortcut = 'cmd+opt+2'
assignAppShortcut = 'cmd+opt+shift+2'

  [[profiles.workspaces.apps]]
  bundleIdentifier = 'app.zen-browser.zen'
  name = 'Zen'

[[profiles.workspaces]]
display = 'Built-in Retina Display'
name = 'Chrome'
shortcut = 'cmd+opt+3'
assignAppShortcut = 'cmd+opt+shift+3'

[[profiles.workspaces]]
display = 'Built-in Retina Display'
name = 'Claude'
shortcut = 'cmd+opt+4'
assignAppShortcut = 'cmd+opt+shift+4'

  [[profiles.workspaces.apps]]
  bundleIdentifier = 'com.anthropic.claudefordesktop'
  name = 'Claude'

# --- Secondary display: lettered workspaces A-C, mirroring aerospace A,B,C ---
# REPLACE the display name with whatever `flashspace list-displays` reports.

[[profiles.workspaces]]
display = 'TODO-external-monitor-name'
name = 'Meet'
shortcut = 'cmd+opt+a'
assignAppShortcut = 'cmd+opt+shift+a'

[[profiles.workspaces]]
display = 'TODO-external-monitor-name'
name = 'Reference'
shortcut = 'cmd+opt+b'
assignAppShortcut = 'cmd+opt+shift+b'

[[profiles.workspaces]]
display = 'TODO-external-monitor-name'
name = 'Scratch'
shortcut = 'cmd+opt+c'
assignAppShortcut = 'cmd+opt+shift+c'
```
