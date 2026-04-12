# FlashSpace PR Plan: `label` field for Workspace

## Overview

Add an optional `label: String?` field to the `Workspace` model. Space control renders it
in the corner badge instead of the 1-based index when present. The label is independent
of the activate shortcut — it is a display hint only, opt-in and backward-compatible in
exactly the same way as `symbolIconName`. Existing configs decode unchanged; only workspaces
that explicitly set `label` are affected.

---

## Files to Change

All paths relative to the FlashSpace repo root.

| File | Change |
|------|--------|
| `FlashSpace/Features/Workspaces/Models/Workspace.swift` | Add `label: String?` field and `CodingKeys` case |
| `FlashSpace/Features/SpaceControl/Views/SpaceControlViewModel.swift` | Carry `label` through `SpaceControlWorkspace` |
| `FlashSpace/Features/SpaceControl/Views/SpaceControlView.swift` | Render `label ?? "\(index + 1)"` in the corner badge |
| `FlashSpace/Features/CLI/Models/CreateWorkspaceRequest.swift` | Add `label: String?` field; pass to `Workspace` init |
| `FlashSpace/Features/CLI/Models/UpdateWorkspaceRequest.swift` | Add `label: String?` field; apply in `WorkspaceCommands` |
| `FlashSpaceCLI/Sources/Subcommands/Workspace/CreateWorkspaceCommand.swift` | Add `@Option var label: String?`; include in request |
| `FlashSpaceCLI/Sources/Subcommands/Workspace/UpdateWorkspaceCommand.swift` | Add `@Option var label: String?`; include in request |

Note: `CreateWorkspaceRequest` and `UpdateWorkspaceRequest` exist in **both** the app target
(`FlashSpace/Features/CLI/Models/`) and the CLI SPM package
(`FlashSpaceCLI/Sources/Subcommands/Workspace/`). Both copies must change — they are
duplicated structs, not shared.

---

## Step-by-Step Implementation

### 1. `Workspace` model — add field

**File:** `FlashSpace/Features/Workspaces/Models/Workspace.swift`

In the `CodingKeys` enum (lines 14–24), add:
```swift
case label
```

In the struct body (after `openAppsOnActivation`, line 34), add:
```swift
var label: String?
```

### 2. `SpaceControlWorkspace` — carry label

**File:** `FlashSpace/Features/SpaceControl/Views/SpaceControlViewModel.swift`

Add `label: String?` to the `SpaceControlWorkspace` struct (lines 12–19):
```swift
struct SpaceControlWorkspace {
    let index: Int
    let name: String
    let label: String?       // <-- add
    let symbol: String
    ...
}
```

In `refresh()`, where `SpaceControlWorkspace` is constructed (lines 82–89), add:
```swift
label: workspace.label,
```

### 3. `SpaceControlView` — render label

**File:** `FlashSpace/Features/SpaceControl/Views/SpaceControlView.swift`

The `workspaceNumber` function (lines 121–130) currently receives `workspace.index + 1` as
an `Int` and formats it with `"\(number)"`. Change the call site (line 31) and the
function signature to accept a `String` and use `label ?? "\(index + 1)"`:

Call site (currently line 31):
```swift
// before
.overlay(alignment: .topTrailing) { workspaceNumber(workspace.index + 1) }

// after
.overlay(alignment: .topTrailing) {
    workspaceNumber(workspace.label ?? "\(workspace.index + 1)")
}
```

Function signature (currently line 121):
```swift
// before
private func workspaceNumber(_ number: Int) -> some View {
    Text("\(number)")

// after
private func workspaceNumber(_ label: String) -> some View {
    Text(label)
```

Everything else in the function body stays identical.

### 4. App-target `CreateWorkspaceRequest` — add field and wire to init

**File:** `FlashSpace/Features/CLI/Models/CreateWorkspaceRequest.swift`

In the struct (lines 11–19), add:
```swift
let label: String?
```

In `toWorkspace` (lines 22–34), add to the `Workspace` init:
```swift
label: label,
```

### 5. App-target `UpdateWorkspaceRequest` — add field

**File:** `FlashSpace/Features/CLI/Models/UpdateWorkspaceRequest.swift`

Add to the struct (lines 10–19):
```swift
let label: String?
```

In `WorkspaceCommands.updateWorkspace()` (`FlashSpace/Features/CLI/Commands/WorkspaceCommands.swift`,
lines 88–123), add after the `openApps` block (line 116):
```swift
if let label = request.label {
    workspace.label = label
}
```

### 6. CLI package `CreateWorkspaceCommand` — add option

**File:** `FlashSpaceCLI/Sources/Subcommands/Workspace/CreateWorkspaceCommand.swift`

The `CreateWorkspaceRequest` struct at the top of this file (lines 11–19) also needs
`label: String?` added (same as step 4).

Add `@Option` to the command struct (after `--icon`, line 33):
```swift
@Option(help: "A short display label shown in space control (e.g. '0', 'A')")
var label: String?
```

Include it in the `CreateWorkspaceRequest` init in `run()` (lines 49–57):
```swift
label: label,
```

### 7. CLI package `UpdateWorkspaceCommand` — add option

**File:** `FlashSpaceCLI/Sources/Subcommands/Workspace/UpdateWorkspaceCommand.swift`

The `UpdateWorkspaceRequest` struct at the top of this file (lines 12–21) also needs
`label: String?` added.

Add `@Option` to the command struct (alongside `--open-apps`, line 38):
```swift
@Option(help: "A short display label shown in space control (e.g. '0', 'A')")
var label: String?
```

The `run()` function currently throws `CommandError.operationFailed("Invalid command")` when
neither `--display` nor `--active-display` is provided (line 50). The `label`-only update
path is valid, so add a branch:

```swift
func run() throws {
    if let display {
        sendCommand(.updateWorkspace(.init(name: workspace, display: .name(display), openApps: openApps, label: label)))
    } else if activeDisplay {
        sendCommand(.updateWorkspace(.init(name: workspace, display: .active, openApps: openApps, label: label)))
    } else if label != nil || openApps != nil {
        sendCommand(.updateWorkspace(.init(name: workspace, display: nil, openApps: openApps, label: label)))
    } else {
        throw CommandError.operationFailed("Invalid command")
    }
    runWithTimeout()
}
```

Also update `validate()` to not require `--display`/`--active-display` when `--label` or
`--open-apps` is supplied (adjust the final guard at line 69):
```swift
if display == nil, !activeDisplay, label == nil, openApps == nil {
    throw CommandError.operationFailed("You must provide either a display name or use the --active-display flag")
}
```

---

## Build & Test Plan

### Build from source

```sh
cd /Users/jmdb/Code/github/wojciech-kulik/FlashSpace
brew bundle          # installs xcodegen + other deps
xcodegen generate    # generates FlashSpace.xcodeproj
open FlashSpace.xcodeproj
```

Then in Xcode: select the FlashSpace target → Signing & Capabilities → set your team. Build & run.

Note: Run `xcodegen generate` again after every branch switch or pull.

### Manual verification

- Set `label = "0"` on one workspace in `~/.config/flashspace/profiles.toml` by running:
  ```sh
  flashspace update-workspace --workspace "Scratch" --label "0"
  ```
  (Use the newly built CLI, or re-install via FlashSpace Settings → CLI tab.)

- Open space control (`showFlashSpace` shortcut). Verify the corner badge shows `"0"` for
  that workspace and the numeric index for all others.

- Remove `label` from the workspace; verify fallback to `"1"` (1-based index).

- Verify `flashspace create-workspace "Test" --label "T"` creates a workspace whose badge
  shows `T`.

- Verify a config without any `label` fields loads cleanly (no decode error, badges show
  numeric indices).

- Verify `flashspace list-workspaces` output is unaffected (label is not printed there
  unless the maintainer wants it; leave as-is for first PR).

---

## PR Notes

Suggested PR title: **feat: add optional `label` field to Workspace for space control display**

Body:

> **What**
> Adds `label: String?` to the `Workspace` model. When set, space control renders it in
> the corner badge instead of the 1-based position index. All other paths are unchanged.
>
> **Why**
> The numeric index in space control doesn't correspond to the activate-shortcut key (e.g.
> a workspace bound to `cmd+opt+0` shows index `1`). A user-defined label makes the
> overlay directly navigable by the character shown.
>
> **Backward compatibility**
> - Fully additive. Existing configs have no `label` key; decoders treat absent optional
>   fields as `nil`, so no migration needed and no wipe-on-decode risk.
> - `label` is independent of `activateShortcut` — it is a display hint only, like
>   `symbolIconName`.
> - No UI settings panel added; the field is CLI-only for now (consistent with `symbolIconName`
>   which is also CLI-only in the current codebase).
>
> **Changes**
> - `Workspace` model: +1 optional field
> - `SpaceControlViewModel/View`: label forwarded and rendered
> - `CreateWorkspaceRequest` / `UpdateWorkspaceRequest` (both app and CLI copies): +1 optional field
> - `create-workspace` CLI subcommand: `--label <string>` option
> - `update-workspace` CLI subcommand: `--label <string>` option; `validate()` relaxed so
>   a label-only update is accepted without requiring `--display`/`--active-display`

### Context from README

Space control already supports 0-9 key navigation per the README (line 98: "Use 0-9 and
arrow keys to switch between workspaces"). The `label` field slots cleanly into this — it
controls which character is *displayed* next to each workspace, which is what the user
presses to jump there. Use this framing in the PR description to show the feature is
consistent with existing design intent.

---

## Our Config (post-merge)

Once merged and released:

- Set `label = "0"` on the Scratch workspace.
- Set `label = "1"` through `"9"` on numbered workspaces (matching the digit in their
  `cmd+opt+<n>` shortcut).
- Set `label = "A"`, `"B"`, ... for any further workspaces beyond 9.
- Update the `flash-create-workspace` wrapper script
  (`home/bin/flash-create-workspace`) to accept an optional `--label` argument and
  pass it through to `flashspace create-workspace`.
