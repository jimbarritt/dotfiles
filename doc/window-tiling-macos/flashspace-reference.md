# FlashSpace — complete technical reference

Derived from reading the FlashSpace source at `<flashspace-repo>`.

---

## settings.toml keys

All fields are optional. Defaults noted where found in source.

### General

| Key | Type | Default | Notes |
|---|---|---|---|
| `checkForUpdatesAutomatically` | Bool | false | |
| `showFlashSpace` | HotKey | — | Show FlashSpace window |
| `toggleFlashSpace` | HotKey | — | Toggle visibility |
| `showFloatingNotifications` | Bool | true | Toast notifications (see below) |
| `pauseResumeFlashSpace` | HotKey | — | |

### Menu bar

| Key | Type | Default | Notes |
|---|---|---|---|
| `showMenuBarTitle` | Bool | true | Show workspace name in menu bar |
| `showMenuBarIcon` | Bool | true | Show lightning bolt icon |
| `menuBarTitleTemplate` | String | `"$WORKSPACE"` | See placeholders below |
| `menuBarDisplayAliases` | String | `""` | `"Built-in Retina Display=Internal;LG=Ext"` |

**Menu bar title placeholders:**

| Placeholder | Resolves to |
|---|---|
| `$WORKSPACE` | Active workspace name |
| `$WORKSPACE_NUMBER` | Active workspace ordinal (1-based) |
| `$DISPLAY` | Active display name (after alias resolution) |
| `$PROFILE` | Active profile name |

### Focus manager

| Key | Type | Default | Notes |
|---|---|---|---|
| `enableFocusManagement` | Bool | false | Directional window focus |
| `centerCursorOnFocusChange` | Bool | false | |
| `focusLeft/Right/Up/Down` | HotKey | — | |
| `focusNextWorkspaceApp` | HotKey | — | |
| `focusPreviousWorkspaceApp` | HotKey | — | |
| `focusNextWorkspaceWindow` | HotKey | — | |
| `focusPreviousWorkspaceWindow` | HotKey | — | |
| `focusFrontmostWindow` | Bool | false | Always focus frontmost |

### Gestures

| Key | Type | Default | Notes |
|---|---|---|---|
| `enableSwipeGestures` | Bool | false | |
| `swipeThreshold` | Double | 0.2 | |
| `restartAppOnWakeUp` | Bool | false | |
| `swipeLeft3FingerAction` | GestureAction | `previousWorkspace` | |
| `swipeRight3FingerAction` | GestureAction | `nextWorkspace` | |
| `swipeUp3FingerAction` | GestureAction | `none` | |
| `swipeDown3FingerAction` | GestureAction | `none` | |
| `swipeLeft4FingerAction` | GestureAction | `none` | |
| `swipeRight4FingerAction` | GestureAction | `none` | |
| `swipeUp4FingerAction` | GestureAction | `none` | |
| `swipeDown4FingerAction` | GestureAction | `none` | |

**Gesture action values:** `none`, `toggleSpaceControl`, `showSpaceControl`, `hideSpaceControl`, `nextWorkspace`, `previousWorkspace`, `mostRecentWorkspace`, `hideAllApps`, `hideUnassignedApps`, `showUnassignedApps`, `nextProfile`, `previousProfile`, `focusLeft`, `focusRight`, `focusUp`, `focusDown`, `focusNextApp`, `focusPreviousApp`, `focusNextWindow`, `focusPreviousWindow`, `activateWorkspace:Name`, `activateProfile:Name`

### Workspaces

| Key | Type | Default | Notes |
|---|---|---|---|
| `displayMode` | String | `static` | `static` or `dynamic` |
| `centerCursorOnWorkspaceChange` | Bool | false | |
| `changeWorkspaceOnAppAssign` | Bool | true | Auto-switch on assign |
| `activeWorkspaceOnFocusChange` | Bool | true | Track active workspace on focus |
| `autoAssignAppsToWorkspaces` | Bool | false | |
| `autoAssignAlreadyAssignedApps` | Bool | false | |
| `skipEmptyWorkspacesOnSwitch` | Bool | false | |
| `keepUnassignedAppsOnSwitch` | Bool | false | Don't hide unassigned apps |
| `restoreHiddenAppsOnSwitch` | Bool | true | Show previously hidden apps on return |
| `enableWorkspaceTransitions` | Bool | false | Animated transitions |
| `showRecentWorkspaceWhenActivatedTwice` | Bool | false | Double-tap hotkey = recent |
| `workspaceTransitionDuration` | Double | 0.3 | Max 0.5 |
| `workspaceTransitionDimming` | Double | 0.2 | 0–0.5, screen dim during transition |
| `loopWorkspaces` | Bool | true | Wrap last→first |
| `loopWorkspacesOnAllDisplays` | Bool | false | Dynamic mode only |
| `switchWorkspaceOnCursorScreen` | Bool | false | Switch display under cursor, not active window |
| `switchToPreviousWorkspace` | HotKey | — | |
| `switchToNextWorkspace` | HotKey | — | |
| `switchToRecentWorkspace` | HotKey | — | |
| `assignFocusedApp` | HotKey | — | |
| `unassignFocusedApp` | HotKey | — | |
| `showUnassignedApps` | HotKey | — | |
| `toggleFocusedAppAssignment` | HotKey | — | |
| `assignVisibleApps` | HotKey | — | Assign all on-screen apps |
| `hideUnassignedApps` | HotKey | — | |
| `hideAllApps` | HotKey | — | |
| `alternativeDisplays` | String | `""` | `"Primary=Fallback;Ext=Built-in"` — fallback when disconnected |
| `enablePictureInPictureSupport` | Bool | true | |
| `switchWorkspaceWhenPipCloses` | Bool | true | |

### Floating apps

| Key | Type | Default | Notes |
|---|---|---|---|
| `floatingApps` | [MacApp] | — | Always float above all workspaces |
| `floatTheFocusedApp` | HotKey | — | |
| `unfloatTheFocusedApp` | HotKey | — | |
| `toggleTheFocusedAppFloating` | HotKey | — | |

### Space Control

| Key | Type | Default | Notes |
|---|---|---|---|
| `enableSpaceControl` | Bool | false | |
| `showSpaceControl` | HotKey | — | |
| `enableSpaceControlAnimations` | Bool | true | |
| `enableSpaceControlTilesAnimations` | Bool | true | |
| `spaceControlCurrentDisplayWorkspaces` | Bool | false | Show only current display |
| `spaceControlHideEmptyWorkspaces` | Bool | false | |
| `spaceControlUpdateScreenshotsOnOpen` | Bool | false | Refresh screenshots on open |
| `spaceControlNumberOfColumns` | Int | 0 (auto) | Fixed column count |

### Workspace Switcher

| Key | Type | Default | Notes |
|---|---|---|---|
| `enableWorkspaceSwitcher` | Bool | true | |
| `showWorkspaceSwitcher` | HotKey | `opt+tab` | Shift variant registered automatically for reverse |
| `workspaceSwitcherShowScreenshots` | Bool | true | |
| `workspaceSwitcherVisibleWorkspaces` | Int | 5 | Min 1 |
| `workspaceSwitcherSortByLastActivation` | Bool | false | |
| `workspaceSwitcherCurrentDisplayWorkspaces` | Bool | true | Current display only |

### Integrations

| Key | Type | Default | Notes |
|---|---|---|---|
| `enableIntegrations` | Bool | false | Must be true for scripts to run |
| `runScriptOnLaunch` | String | `""` | |
| `runScriptOnWorkspaceChange` | String | sketchybar trigger | Runs before switch |
| `runScriptAfterWorkspaceChange` | String | `""` | Runs after switch completes |
| `runScriptOnProfileChange` | String | `sketchybar --reload` | |

Script env vars: `$WORKSPACE`, `$DISPLAY`, `$PROFILE`

### Profiles

| Key | Type | Default | Notes |
|---|---|---|---|
| `switchToNextProfile` | HotKey | — | |
| `switchToPreviousProfile` | HotKey | — | |

---

## Toast notifications

When actions trigger a notification (app assigned, workspace switched, etc.) FlashSpace shows a custom **Toast** — a borderless `NSWindow` it creates itself, not a system notification.

- Positioned **bottom-centre** of the main screen (~7% up from the bottom edge)
- Blurred/vibrancy background (`.sidebar` material)
- Stays visible for **2 seconds**, then fades out over 0.3s
- SF Symbols icon + bold text; text colour varies by action
- Controlled by `showFloatingNotifications` in settings.toml (default: `true`)
- Source: `FlashSpace/UI/Toast.swift`

---

## profiles.toml structure

```toml
[[profiles]]
id = 'UUID'          # required, stable identity — don't change
name = 'Default'     # required

[[profiles.workspaces]]
id = 'UUID'          # required
name = 'Coding'      # required
display = 'Built-in Retina Display'   # required (static mode)
shortcut = 'cmd+opt+1'               # activateShortcut alias
assignAppShortcut = 'cmd+opt+shift+1'
openAppsOnActivation = false
symbolIconName = 'laptop'            # SF Symbols name

  [[profiles.workspaces.apps]]
  name = 'Ghostty'
  bundleIdentifier = 'com.mitchellh.ghostty'
  iconPath = '/Applications/Ghostty.app/Contents/Resources/AppIcon.icns'
  autoOpen = false                   # launch on workspace activation

  [[profiles.workspaces.appToFocus]]  # optional, single app
  name = 'Ghostty'
  bundleIdentifier = 'com.mitchellh.ghostty'
```

**TOML gotchas:**
- `[[profiles.workspaces.apps]]` blocks must come **after** all scalar fields of the parent `[[profiles.workspaces]]` block. Scalars after an array-of-tables block are interpreted as belonging to the last table, not the parent.
- `shortcut` is the TOML alias for `activateShortcut` (via CodingKeys in Swift).
- Don't add `apps = []` inline if you're going to add `[[profiles.workspaces.apps]]` blocks — TOML disallows both forms for the same key.
- Profile UUIDs are identity — change them and FlashSpace loses track of the active profile.

---

## Hotkey format

**Format:** `modifier+modifier+key` (case-insensitive, order of modifiers doesn't matter)

**Valid modifiers:** `cmd`, `ctrl`, `opt`, `shift`  
**`alt` is NOT valid** — it's silently dropped by the parser.

**Key names:** `a–z`, `0–9`, `space`, `up`, `down`, `left`, `right`, `enter`, `escape`, `tab`, `delete`, `forward-delete`, `capslock`, `f1–f12`, `num0–num9`, `num-plus`, `num-minus`, `num-multiply`, `num-divide`, `num-clear`, `num-enter`, `num-equals`, `num-decimal`, `plus`

---

## CLI reference

Full CLI at `/usr/local/bin/flashspace`. Communicates with the running app via socket.

### Profiles
```sh
flashspace create-profile <NAME> [--copy] [--activate]
flashspace delete-profile <NAME>
flashspace profile [<NAME>] [--next] [--prev]
flashspace get-profile
flashspace list-profiles
```

### Workspaces
```sh
flashspace create-workspace <NAME> [--display <NAME>] [--icon <NAME>] \
  [--activate-key <HOTKEY>] [--assign-key <HOTKEY>] [--open-apps] [--activate]
flashspace delete-workspace <NAME>
flashspace update-workspace (--workspace <NAME> | --active-workspace) \
  (--display <NAME> | --active-display) [--open-apps true|false]
flashspace workspace [<NAME>] [--number <INT>] [--next] [--prev] [--recent] \
  [--skip-empty] [--loop] [--clean]
flashspace get-workspace [--display <NAME>]
flashspace list-workspaces [--with-display] [--profile <NAME>]
```

### Apps
```sh
flashspace assign-app [--name <NAME|BUNDLE>] [--workspace <NAME>] \
  [--activate true|false] [--show-notification]
flashspace assign-visible-apps [--workspace <NAME>] [--show-notification]
flashspace unassign-app [--name <NAME|BUNDLE>] [--show-notification]
flashspace hide-unassigned-apps
flashspace list-apps <WORKSPACE> [--profile <NAME>] [--with-bundle-id] \
  [--with-icon] [--only-running]
flashspace list-running-apps [--with-bundle-id]
```

### Display / focus
```sh
flashspace list-displays
flashspace get-display
flashspace focus (--direction up|down|left|right | --next-app | --prev-app | \
  --next-window | --prev-window)
```

### Floating apps
```sh
flashspace floating-apps <float|unfloat|toggle> [--name <NAME|BUNDLE>] \
  [--show-notification]
flashspace list-floating-apps [--with-bundle-id]
```

### Other
```sh
flashspace open                  # launch app
flashspace open-space-control   # open Space Control overlay
flashspace get-app [--with-windows-count]
```

---

## Non-obvious behaviours

**Settings are global, not per-profile.** Only workspaces and their app assignments are per-profile.

**`activeWorkspaceOnFocusChange`** — when true, focusing an app updates which workspace is tracked as active, but does NOT hide/show any apps. It's purely a tracking change.

**`showRecentWorkspaceWhenActivatedTwice`** — pressing a workspace hotkey twice jumps to the previous workspace on that display (like a toggle). Useful for quick back-and-forth.

**Workspace switcher reverse direction** — the `showWorkspaceSwitcher` hotkey's Shift variant is registered automatically. E.g. `opt+tab` forward, `shift+opt+tab` backward. You don't configure this separately.

**Display matching** — matched by `NSScreen.localizedName` only. No serial/EDID fallback. If disconnected, `alternativeDisplays` config can map it to a fallback. Run `flashspace list-displays` to see what names FlashSpace sees.

**Profile switch doesn't activate a workspace** — you land in "no active workspace" until you trigger one manually. Known issue: [wojciech-kulik/FlashSpace#388](https://github.com/wojciech-kulik/FlashSpace/issues/388).

**App identity is by bundle ID** — if bundle ID is empty it falls back to name. Bundle ID takes priority; two apps match if they share a bundle ID regardless of display name.

**Workspace numbers are computed** — the `$WORKSPACE_NUMBER` placeholder and `--number N` CLI flag use the 1-based array index within the profile. They're not stored in config.

**GUI rewrites `profiles.toml`** — any comments, blank lines, or custom key ordering will be lost. Pick one source of truth: hand-edit or GUI.
