# FlashSpace — research & config notes

Replacing AeroSpace with FlashSpace (+ Hammerspoon for the bits FlashSpace doesn't do). This file is the working notebook: how FlashSpace actually works, how it maps onto the AeroSpace setup we're coming from, and the config we want to land on.

## TL;DR

- FlashSpace is **app-based**, not window-based. A workspace is a set of apps that get unhidden when you switch to it; everything else on that display gets hidden. There is no per-window membership.
- All hotkeys live in `~/.config/flashspace/profiles.toml` as `shortcut = 'cmd+opt+1'` strings on each workspace. macOS System Settings is not involved.
- Each workspace pins to one display via `display = 'Built-in Retina Display'`. FlashSpace tracks the active workspace **per display**, so two displays can have independent active workspaces simultaneously — matching AeroSpace's `workspace-to-monitor-force-assignment` behaviour.
- Display matching is by `NSScreen.localizedName` only — no EDID/serial fallback. If the monitor is missing, the workspace silently fails to activate.
- There's a full CLI at `/usr/local/bin/flashspace` we can drive from Hammerspoon.
- **Big gap vs AeroSpace**: no `move-window-to-workspace`. The closest thing is reassigning the *whole app* to a different workspace via `assignAppShortcut` or `flashspace assign-app`. We need a workaround if we want AeroSpace's `alt-shift-<num>` flow.

## Why not native macOS Spaces?

The natural question: macOS already has per-display Mission Control spaces, Hammerspoon has a working `hs.spaces.*` API, and `ctrl+1..9` will switch spaces if you enable it in System Settings. Why run a third-party app for this?

Pinning this down so we don't keep re-asking.

**What native spaces give you for free** that FlashSpace also provides:

- Per-display workspace state (each display has its own set of Desktops)
- Hotkey switching (`ctrl+1..9` if enabled in System Settings → Keyboard)
- Real multi-desktop metaphor — windows preserve position per space, per-space wallpapers work
- `hs.spaces.*` Hammerspoon bindings actually observe reality — a bonus if you also need to tile inside workspaces (see [`hammerspoon-auto-tile-poc.md`](./hammerspoon-auto-tile-poc.md))

**What they don't give you, that FlashSpace does:**

- **Named, semantically meaningful workspaces.** Native spaces are just "Desktop 1..N". FlashSpace has "Coding", "Browsing", "Meet" — and the name is how you reason about them, in docs and out loud. This is the biggest single reason.
- **Declarative config.** FlashSpace reads `~/.config/flashspace/profiles.toml` with `bundleIdentifier → workspace` mappings, version-controlled in this dotfiles repo. Native spaces have no config file; you drag windows in Mission Control or click Dock → Options → Assign To per app, one at a time, and the state lives in an opaque plist.
- **Stable identity.** Native space IDs shift when you reorder spaces in Mission Control. Empty spaces get auto-destroyed. Full-screen apps create their own ad-hoc spaces that eat space slots. None of this matters for FlashSpace because workspace identity is just a string in a TOML file.
- **Per-display active-workspace tracking that actually works.** FlashSpace explicitly stores `activeWorkspace[display]` and switches each display independently. Native spaces nominally do this when "Displays have separate Spaces" is enabled, but the UX of switching one display without disturbing the other is clumsy.
- **CLI and hooks.** `flashspace workspace --number 1`, `flashspace assign-app`, `runScriptOnWorkspaceChange`. Native spaces have no official CLI — third-party tools that drive them (Yabai etc.) use private SkyLight APIs and require disabling SIP.
- **Instant switching.** FlashSpace is hide/unhide — essentially free, jump-cut fast. Native-spaces switching animates through Mission Control's slide transition and can't be disabled.
- **Unambiguous hotkeys from anywhere.** `cmd+opt+1` always activates workspace 1 regardless of what app has focus. Native spaces' `ctrl+1..9` has inconsistencies with modal dialogs, full-screen apps, and some games.

**Could Hammerspoon close the gap?** Partially. `hs.spaces.gotoSpace()` and `hs.spaces.moveWindowToSpace()` exist and work, so you could bind hotkeys and maintain an `app → space_id` table in Lua. But you'd be re-implementing FlashSpace's config model on top of a less stable foundation (unstable IDs, phantom full-screen spaces, Mission Control animation overhead) and giving up the FlashSpace CLI in exchange. Bad trade.

**Verdict**: FlashSpace stays as the workspace layer. The Hammerspoon auto-tile PoC composes with it via app-visibility state, not via native spaces — see that doc's "Composition with FlashSpace" section.

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

### FlashSpace does not use native macOS Spaces

Concrete mechanism, from `FlashSpace/Features/Workspaces/WorkspaceManager.swift`: activation calls `NSRunningApplication.hide()` / `unhide()` on the relevant `NSRunningApplication` objects. Every FlashSpace workspace lives on the **same** underlying native (Mission Control) space — "switching workspace" is a hide/unhide pass over app processes, not a space swap.

Consequences worth knowing if you're writing tooling that composes with FlashSpace:

- **`hs.spaces.focusedSpace()` / `hs.spaces.windowSpaces()` don't observe FlashSpace.** Those APIs talk to the native CoreGraphics spaces layer, which FlashSpace doesn't touch. They'll report the same space ID regardless of which FlashSpace workspace is active, so any membership check built on them is effectively a no-op in this setup. Use `hs.window:isVisible()` (which reflects the owning app's hidden state) instead.
- **Anything driven by AX window events needs to filter out hidden apps.** When FlashSpace hides an app, its windows still exist — they just aren't visible. `kAXWindowCreatedNotification` / `wf.windowVisible` etc. can still fire for apps on other workspaces when they're un-hidden, and naive event handlers will act on them as if the user is looking at them. Guard with `NSRunningApplication.isHidden` (Swift) or `win:isVisible()` (Hammerspoon).
- **Tools that want to know "did the user just switch workspace?"** should listen to FlashSpace directly via `runScriptOnWorkspaceChange` in `settings.toml`, or watch for `NSRunningApplication` hidden-state transitions. Don't try to infer it from space membership.

This is the single most important piece of context for anything integrating with FlashSpace, and it's easy to miss because the word "workspace" implies a virtual-desktop model that isn't what's happening under the hood.

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

## Sending a workspace to a different monitor

The use case: you're in a Notion workspace on CENTRE, a Meet starts, you want to send Notion to LEFT so you can share that whole screen.

`opt+shift+A/B/C` sends the active workspace to a monitor (AeroSpace muscle memory preserved):
- `opt+shift+a` → LEFT external monitor
- `opt+shift+b` → Built-in Retina Display (laptop)
- `opt+shift+c` → CENTRE external monitor

Implemented as a Karabiner-Elements complex modification (Karabiner is already installed for low-level key remapping). Add this rule to `karabiner.json` under `complex_modifications.rules`:

```json
{
  "description": "FlashSpace: send active workspace to monitor",
  "manipulators": [
    {
      "type": "basic",
      "from": { "key_code": "a", "modifiers": { "mandatory": ["option", "shift"] } },
      "to": [{ "shell_command": "flashspace update-workspace --active-workspace --display 'TODO-LEFT'" }]
    },
    {
      "type": "basic",
      "from": { "key_code": "b", "modifiers": { "mandatory": ["option", "shift"] } },
      "to": [{ "shell_command": "flashspace update-workspace --active-workspace --display 'Built-in Retina Display'" }]
    },
    {
      "type": "basic",
      "from": { "key_code": "c", "modifiers": { "mandatory": ["option", "shift"] } },
      "to": [{ "shell_command": "flashspace update-workspace --active-workspace --display 'TODO-CENTRE'" }]
    }
  ]
}
```

**TODO when docked**: run `flashspace list-displays` and replace `TODO-LEFT` and `TODO-CENTRE` with the exact strings it returns.

Notes:
- `update-workspace` writes to `profiles.toml` — the move is persistent. Move it back the same way.
- The workspace switcher and Space Control only show workspaces for the current display — after moving, the workspace disappears from CENTRE's view and appears in LEFT's.

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
