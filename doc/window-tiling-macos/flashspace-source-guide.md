# FlashSpace macOS App — Developer Navigation Guide

## Overview

FlashSpace is a blazingly-fast virtual workspace manager for macOS that enhances and replaces native macOS Spaces. It allows you to define virtual workspaces, assign apps to them, and switch between workspaces instantly without macOS animations. The app is built in Swift and uses SwiftUI for the UI layer.

## Key Source Directories

| Directory | Purpose |
|-----------|---------|
| `/FlashSpace/App/` | App entry point, delegates, constants, migrations |
| `/FlashSpace/Features/Config/` | TOML/JSON/YAML serialization, config format detection |
| `/FlashSpace/Features/Profiles/` | Profile model and repository for managing multiple profiles |
| `/FlashSpace/Features/Workspaces/` | Workspace model, repository, and manager (core logic) |
| `/FlashSpace/Features/Settings/` | Settings UI and repository (split into feature-specific settings classes) |
| `/FlashSpace/Features/CLI/` | Unix socket server for IPC, command execution |
| `/FlashSpace/Features/Integrations/` | Script execution hooks for workspace/profile changes |
| `/FlashSpace/Features/HotKeys/` | Hotkey recording and management |
| `/FlashSpace/Features/FocusManager/` | Window focus tracking and app focus detection |
| `/FlashSpace/Features/FloatingApps/` | Floating apps that persist across all workspaces |
| `/FlashSpace/Features/Displays/` | Display detection and management |
| `/FlashSpace/Features/MenuBar/` | Menu bar icon/text rendering |
| `/FlashSpace/Features/SpaceControl/` | Grid preview of all workspaces |
| `/FlashSpace/Features/WorkspaceSwitcher/` | Fast workspace switcher (Option+Tab-like) |
| `/FlashSpace/Features/PictureInPicture/` | PiP window tracking and restoration |

---

## 1. Config/TOML Serialization & Persistence

**Files:**
- `/Features/Config/ConfigSerializer.swift` — Main serialization entry point
- `/Features/Config/Models/ConfigFormat.swift` — Enum for JSON/TOML/YAML formats
- `/Features/Config/Models/ConfigCoder.swift` — Protocol for encoder/decoder
- `/Features/Profiles/ProfilesRepository.swift` — Loads/saves profiles (uses ConfigSerializer)
- `/Features/Settings/SettingsRepository.swift` — Loads/saves settings (uses ConfigSerializer)

**Key Functions:**

```swift
// ConfigSerializer.swift (lines 26-39)
static func deserialize<T: Decodable>(_ type: T.Type, filename: String) throws -> T?
// Returns nil if file doesn't exist, throws on decode error

static func serialize(filename: String, _ value: some Encodable) throws
// Writes to ~/.config/flashspace/{filename}.{json|toml|yaml}
```

**Config Directory:**
- Location: `~/.config/flashspace/`
- Files: `profiles.{json,toml,yaml}` and `settings.{json,toml,yaml}`
- Format detection (line 104-114): Checks for existing files in order `json → toml → yaml`, defaults to JSON

**Known Gotchas:**

### The Wipe-on-Decode-Failure Bug

In `ProfilesRepository.loadFromDisk()` (lines 43-59):

```swift
guard let config = try? ConfigSerializer.deserialize(ProfilesConfig.self, filename: "profiles"),
      !config.profiles.isEmpty else {
    return createDefaultProfile()  // <-- GOTCHA: Silently wipes on decode error!
}
```

**The Problem:** If the TOML file contains invalid syntax or a field that fails to decode, the `try?` silently catches the error and returns `nil`. This triggers `createDefaultProfile()`, which **wipes all user workspaces and replaces them with an empty default profile**, even if the file is 99% valid.

**Why This Happens:**
- `try?` suppresses the error completely (line 47)
- There's no logging or recovery mechanism
- A malformed TOML file is treated identically to a missing file

**Example Scenario:**
If `~/.config/flashspace/profiles.toml` has a syntax error (missing quote, bad array format), all workspaces are lost on next app launch.

**The specific trigger we hit:** Adding a `[[profiles.workspaces]]` block with no `[[profiles.workspaces.apps]]` entries. Because `apps: [MacApp]` is non-optional, the TOML decoder throws "key not found" → `try?` catches it → wipe.

**How to avoid:** Never add or modify workspaces by editing the TOML directly. Use the CLI instead (see "Working with Workspaces via CLI" below).

---

## 2. Workspace Model

**File:** `/Features/Workspaces/Models/Workspace.swift`

**Struct Definition:**

```swift
struct Workspace: Identifiable, Codable, Hashable {
    var id: WorkspaceID  // UUID
    var name: String
    var display: DisplayName  // String (display name)
    var activateShortcut: AppHotKey?  // Optional hotkey to activate workspace
    var assignAppShortcut: AppHotKey?  // Optional hotkey for app assignment
    var apps: [MacApp]  // **REQUIRED** (non-optional) — list of assigned apps
    var appToFocus: MacApp?  // Which app should be focused when workspace activates
    var symbolIconName: String?  // SF Symbol name for icon
    var openAppsOnActivation: Bool?  // Auto-launch apps marked with autoOpen flag
}
```

**Key Properties:**
- `apps` is **NOT OPTIONAL** — a workspace always has an `apps` array, even if empty
- `display` is always a `DisplayName` (String), never nil
- Custom `CodingKeys` mapping: `"shortcut"` → `activateShortcut` (line 18)

**Computed Properties:**

```swift
var displays: Set<DisplayName>  // Set of all displays where workspace apps are running
var isDynamic: Bool  // Dynamic display mode (apps follow their display)
var isOnTheCurrentScreen: Bool  // Is the workspace on the currently active screen?
```

**Display Assignment Modes:**
- Static: Workspace tied to a specific display
- Dynamic: Workspace follows where its assigned apps are running

---

## 3. Profile Model

**File:** `/Features/Profiles/Models/Profile.swift`

**Struct Definition:**

```swift
struct Profile: Identifiable, Codable, Hashable {
    let id: ProfileId  // UUID
    var name: String
    var workspaces: [Workspace]  // Profile contains zero or more workspaces
    var shortcut: AppHotKey?  // Optional hotkey to activate profile
}

struct ProfilesConfig: Codable {
    let profiles: [Profile]  // Root config structure
}
```

**Key Points:**
- Profiles are containers for workspace configurations
- A profile can be switched via hotkey (stored in `shortcut`)
- Each profile can have its own set of workspaces
- Default profile has UUID matching `Profile.default` constant

**Profile Management:**
- Selected profile stored in `UserDefaults` via key `selectedProfileId`
- Profile changes trigger `Integrations.runOnProfileChangeIfNeeded()`
- Profile switching resets active workspace state across all displays

---

## 4. The `runScriptAfterWorkspaceChange` Hook

**Configuration Location:**
- Stored in `AppSettings.runScriptAfterWorkspaceChange` (String field)
- Managed by `IntegrationsSettings` class
- Default value: empty string (no script by default)

**Files:**
- `/Features/Settings/_Models/AppSettings.swift` — Line 112
- `/Features/Settings/Integrations/IntegrationsSettings.swift` — Lines 15-19
- `/Features/Integrations/Integrations.swift` — Main execution logic

**Trigger Points:**

```swift
// WorkspaceManager.swift, line 407-419
private func runIntegrationAfterActivation(for workspace: Workspace) {
    let newWorkspace = ActiveWorkspace(
        id: workspace.id,
        name: workspace.name,
        number: workspaceRepository.workspaces.firstIndex { $0.id == workspace.id }.map { "\($0 + 1)" },
        symbolIconName: workspace.symbolIconName,
        display: workspace.displayForPrint
    )
    Integrations.runAfterActivationIfNeeded(workspace: newWorkspace)  // <-- Called here
}
```

Called in `activateWorkspace()` at line 400, **after** all apps are shown/hidden and workspace transition animation completes.

**Execution:** `/Features/Integrations/Integrations.swift` (lines 14-21)

```swift
static func runAfterActivationIfNeeded(workspace: ActiveWorkspace) {
    let script = settings.runScriptAfterWorkspaceChange
        .replacingOccurrences(of: "$WORKSPACE_NUMBER", with: workspace.number ?? "")
        .replacingOccurrences(of: "$WORKSPACE", with: workspace.name)
        .replacingOccurrences(of: "$DISPLAY", with: workspace.display)
        .replacingOccurrences(of: "$PROFILE", with: profilesRepository.selectedProfile.name)
    runScript(script, synchronous: true)  // <-- Runs synchronously
}
```

**Script Execution Details (lines 44-56):**
- Shell: User's default shell (from `getpwuid()`)
- Execution: `shell -c "script"`
- Synchronous: Waits for script completion before returning
- Only runs if `enableIntegrations` is true AND script is non-empty

**Variable Substitutions:**
- `$WORKSPACE_NUMBER` — 1-indexed position in workspace list (or empty if unknown)
- `$WORKSPACE` — Workspace name
- `$DISPLAY` — Display name
- `$PROFILE` — Active profile name

**Other Integration Hooks:**
- `runScriptOnWorkspaceChange` — Runs **before** hiding apps (earlier in activation)
- `runScriptOnLaunch` — Runs on app startup
- `runScriptOnProfileChange` — Runs when profile is switched
- Default for workspace change: `"sketchybar --trigger flashspace_workspace_change WORKSPACE=\"$WORKSPACE\" DISPLAY=\"$DISPLAY\""`

---

## 5. App Assignment to Workspaces

**Files:**
- `/Features/Workspaces/Models/MacApp.swift` — App representation
- `/Features/Workspaces/WorkspaceRepository.swift` — App CRUD operations
- `/Features/Workspaces/WorkspaceManager.swift` — Visibility management

**MacApp Struct:**

```swift
struct MacApp: Codable, Hashable, Equatable {
    var name: String  // Display name
    var bundleIdentifier: BundleId  // Bundle ID (used for launching/matching)
    var iconPath: String?  // Path to app icon cache
    var autoOpen: Bool?  // If true, app auto-launches when workspace activates
}
```

**Equality:** Uses `bundleIdentifier` if available; falls back to name comparison

**App Assignment Operations:**

```swift
// WorkspaceRepository.swift
func addApp(to workspaceId: WorkspaceID, app: MacApp)  // Line 77-82
func deleteApp(from workspaceId: WorkspaceID, app: MacApp, notify: Bool = true)  // Line 85-94
func deleteAppFromAllWorkspaces(app: MacApp)  // Line 96-106
func moveApps(_ apps: [MacApp], from sourceWorkspaceId: WorkspaceID, 
              to targetWorkspaceId: WorkspaceID)  // Line 131-147
func setAutoOpen(_ enabled: Bool, for app: MacApp, in workspaceId: WorkspaceID)  // Line 117-123
```

**App Visibility Management (WorkspaceManager):**

```swift
private func showApps(in workspace: Workspace, setFocus: Bool, on displays: Set<DisplayName>)
private func hideApps(in workspace: Workspace)  // Line 400
```

**App Launching:**
- Triggered by `openAppsIfNeeded()` in `activateWorkspace()` (line 287-310)
- Uses `NSWorkspace.shared.openApplication(at:)`
- Only launches apps marked with `autoOpen == true`
- Only launches if app is not already running

**Special Apps:**
- `AppConstants.lastFocusedOption` — Pseudo-app representing "last focused app in workspace"
- Finder (`com.apple.finder`) — Always visible, never hidden

---

## 6. URL Scheme / External Hooks (IPC)

**There is NO URL scheme handler in FlashSpace.** Instead, communication happens via a **Unix socket server**.

**Files:**
- `/Features/CLI/CLIServer.swift` — Socket server implementation
- `/Features/CLI/Models/CommandRequest.swift` — Command enum
- `/Features/CLI/Models/CommandExecutor.swift` — Command routing

**IPC Mechanism:**

```swift
// CLIServer.swift, line 38-53
private func startServer() {
    let socketPath = "/tmp/flashspace.socket"  // <-- Unix domain socket
    let params = NWParameters(tls: nil, tcp: .init())
    params.requiredLocalEndpoint = .unix(path: socketPath)
    listener = try NWListener(using: params)
}
```

**Socket Location:** `/tmp/flashspace.socket`

**Client Usage (Example):**

Hammerspoon could call FlashSpace via socket like:
```lua
-- Pseudo-code; actual implementation depends on Lua socket library
local socket = require("socket")
local sock = socket.unix()
sock:connect("/tmp/flashspace.socket")
sock:send(encodedCommand)
local response = sock:receive()
```

**Available Commands (CommandRequest enum, lines 10-57):**

```swift
case activateWorkspace(name: String?, number: Int?, clean: Bool)
case nextWorkspace(skipEmpty: Bool, clean: Bool, loop: Bool)
case previousWorkspace(skipEmpty: Bool, clean: Bool, loop: Bool)
case assignApp(app: String?, workspaceName: String?, activate: Bool?, showNotification: Bool)
case activateProfile(name: String)
case listWorkspaces(withDisplay: Bool, profile: String?)
case getWorkspace(display: String?)
// ... many more
```

**Command Processing:**
1. Socket server receives data (line 64-75)
2. Data is decoded into `CommandRequest` enum (line 83)
3. Command is routed through executors (`ProfileCommands`, `WorkspaceCommands`, etc.)
4. Response is encoded and sent back

**Executors:**
- `/Features/CLI/Commands/WorkspaceCommands.swift`
- `/Features/CLI/Commands/ProfileCommands.swift`
- `/Features/CLI/Commands/AppCommands.swift`
- `/Features/CLI/Commands/FocusCommands.swift`
- `/Features/CLI/Commands/GetCommands.swift`
- `/Features/CLI/Commands/ListCommands.swift`

---

## 7. Settings Model

**Files:**
- `/Features/Settings/_Models/AppSettings.swift` — Full settings struct
- `/Features/Settings/SettingsRepository.swift` — Load/save orchestration
- `/Features/Settings/Integrations/IntegrationsSettings.swift` — Example of feature-specific settings class

**AppSettings Structure:**

Top-level config struct with optional fields for all settings:

```swift
struct AppSettings: Codable {
    // General
    var checkForUpdatesAutomatically: Bool?
    var showFlashSpace: AppHotKey?
    var pauseResumeFlashSpace: AppHotKey?
    
    // Workspaces
    var displayMode: DisplayMode?  // Static or Dynamic
    var centerCursorOnWorkspaceChange: Bool?
    var changeWorkspaceOnAppAssign: Bool?
    var autoAssignAppsToWorkspaces: Bool?
    var skipEmptyWorkspacesOnSwitch: Bool?
    var switchToNextWorkspace: AppHotKey?
    var switchToPreviousWorkspace: AppHotKey?
    
    // Integrations
    var enableIntegrations: Bool?
    var runScriptOnLaunch: String?
    var runScriptOnWorkspaceChange: String?
    var runScriptAfterWorkspaceChange: String?  // <-- The main hook
    var runScriptOnProfileChange: String?
    
    // Profiles, Focus Manager, Gestures, Space Control, etc.
    // ... 100+ fields total
}
```

**Settings Organization (lines 12-36 in SettingsRepository):**

FlashSpace splits settings into **feature-specific classes**:
- `GeneralSettings`
- `MenuBarSettings`
- `GesturesSettings`
- `FocusManagerSettings`
- `WorkspaceSettings`
- `PictureInPictureSettings`
- `FloatingAppsSettings`
- `SpaceControlSettings`
- `WorkspaceSwitcherSettings`
- `IntegrationsSettings`
- `ProfileSettings`

Each implements `SettingsProtocol` with:
```swift
protocol SettingsProtocol {
    var updatePublisher: AnyPublisher<(), Never> { get }
    func load(from appSettings: AppSettings)
    func update(_ appSettings: inout AppSettings)
}
```

**Load/Save Cycle:**

```swift
// SettingsRepository.loadFromDisk(), line 91-104
guard let settings = try? ConfigSerializer.deserialize(AppSettings.self, filename: "settings") else { return }
currentSettings = settings
allSettings.forEach { $0.load(from: settings) }  // Each feature loads its slice
```

```swift
// SettingsRepository.updateSettings(), line 79-89
var settings = AppSettings()
allSettings.forEach { $0.update(&settings) }  // Each feature writes its slice
currentSettings = settings
saveToDisk()
```

**Watched Settings:**
- Use Combine `@Published` properties
- Changes are debounced (500ms default)
- Updates trigger `updateSubject.send()` → `saveToDisk()`

---

## Gotchas & Known Issues

### 1. Wipe-on-Decode-Failure in ProfilesRepository

**Location:** `/Features/Profiles/ProfilesRepository.swift`, lines 47-49

**Issue:**
```swift
guard let config = try? ConfigSerializer.deserialize(ProfilesConfig.self, filename: "profiles"),
      !config.profiles.isEmpty else {
    return createDefaultProfile()  // Silently wipes all workspaces!
}
```

If the TOML/JSON/YAML file is corrupt or has invalid syntax, `deserialize()` throws an error that is caught by `try?`, returning `nil`. This is indistinguishable from a missing file, so the app **creates a blank default profile and overwrites the saved file**, destroying all user data.

**Fix Options:**
1. Log the error and offer recovery
2. Rename corrupted file to `.bak` instead of overwriting
3. Show error dialog to user

### 2. Workspace.apps is Non-Optional but Empty is Valid

**Location:** `/Features/Workspaces/Models/Workspace.swift`, line 31

The `apps` field is declared as `[MacApp]` (not `[MacApp]?`), meaning it must always be present but can be an empty array. This is correct but slightly differs from the optional nature of many other workspace fields. Be aware that:
- A workspace with zero apps is valid
- Code must check `workspace.apps.isEmpty` rather than `workspace.apps == nil`

### 3. Display Assignment Modes (Static vs Dynamic)

**Files:**
- `/Features/Workspaces/Models/Workspace.swift`, lines 37-80
- `/Features/Settings/_Models/AppSettings.swift`, line 49

In **static mode**, a workspace is tied to a specific display. In **dynamic mode**, the workspace "follows" its apps across displays. The `displays` computed property (line 37-54) returns different results based on `isDynamic` setting:

```swift
var isDynamic: Bool {
    AppDependencies.shared.workspaceSettings.displayMode == .dynamic
}
```

Be careful when caching workspace display info — it may change if the display mode setting changes or if apps move.

### 4. Profile Change Resets All Active Workspaces

**Location:** `/Features/Workspaces/WorkspaceManager.swift`, lines 73-80

When a profile is switched:
```swift
NotificationCenter.default
    .publisher(for: .profileChanged)
    .sink { [weak self] _ in
        self?.activeWorkspace = [:]
        self?.mostRecentWorkspace = [:]
        self?.activeWorkspaceDetails = nil
    }
```

All active workspace state is cleared. This means if you switch profiles and back, the previous workspace isn't automatically re-activated.

### 5. Script Hooks Run Synchronously

**Location:** `/Features/Integrations/Integrations.swift`, line 20

```swift
runScript(script, synchronous: true)  // Blocks until script completes
```

The `runScriptAfterWorkspaceChange` hook **blocks the main thread** until the script finishes. A long-running script will freeze the UI. There's also `runScriptOnWorkspaceChange` which runs synchronously earlier in the activation flow.

### 6. Config Format Detection Has No Version Field

**Location:** `/Features/Config/ConfigSerializer.swift`, lines 104-114

Format detection works by checking which file exists first (JSON, then TOML, then YAML). There's no version field in the config files, so version migrations are handled separately in code. Switching config formats via `convert(to:)` creates backups but requires rewriting both `settings` and `profiles` files.

### 7. MacApp Equality Uses BundleID but Falls Back to Name

**Location:** `/Features/Workspaces/Models/MacApp.swift`, lines 77-83

```swift
static func == (lhs: MacApp, rhs: MacApp) -> Bool {
    if lhs.bundleIdentifier.isEmpty || rhs.bundleIdentifier.isEmpty {
        return lhs.name == rhs.name  // Fallback to name if bundle ID missing
    } else {
        return lhs.bundleIdentifier == rhs.bundleIdentifier
    }
}
```

This can cause confusion if two apps have the same name but different bundle IDs (or one has a missing bundle ID). Be careful when comparing apps loaded from old config versions that may have empty bundle IDs.

### 8. No URL Scheme Handler

FlashSpace does not implement `onOpenURL()` or URL scheme handling. All external communication goes through the Unix socket at `/tmp/flashspace.socket`. If you need to call FlashSpace from another app, you must use the CLI (which talks to the socket) or implement a socket client.

### 9. Stale socket prevents launch

If FlashSpace won't start (silently fails or crashes on launch), a stale `/tmp/flashspace.socket` file from a previous run (e.g. a dev build that didn't clean up) may be blocking the socket server from binding. Fix: `rm /tmp/flashspace.socket`, then relaunch.

---

## Working with Workspaces via CLI

**Never edit `profiles.toml` directly.** Use the `flashspace` CLI at `/usr/local/bin/flashspace` — it communicates via the Unix socket, goes through `WorkspaceRepository`, and always writes a valid config.

### Hotkey format

Matches the TOML `shortcut` / `assignAppShortcut` field format exactly:

```
cmd+opt+1     # activate shortcut
opt+shift+1   # assign-app shortcut
```

### Common commands

```sh
# List current workspaces
flashspace list-workspaces

# Create a new workspace (no apps needed — CLI handles the empty array correctly)
flashspace create-workspace "Scratch" --activate-key "cmd+opt+0" --assign-key "opt+shift+0"

# Create with an SF Symbol icon
flashspace create-workspace "Scratch" --icon "doc.on.clipboard" --activate-key "cmd+opt+0"

# Delete a workspace
flashspace delete-workspace "Scratch"

# Activate a workspace by name
flashspace workspace "Coding"

# Assign the currently focused app to a workspace
flashspace assign-app --workspace-name "Scratch"
```

### Why the CLI is safe but direct TOML editing isn't

The CLI calls `WorkspaceRepository.addWorkspace()` which constructs a `Workspace` with `apps: []` in memory and then serialises the whole config. The serialiser writes `apps = []` explicitly, so the TOML is always valid. Manual TOML editing risks omitting required non-optional fields (see wipe-on-decode-failure gotcha above).

---

## Architecture Patterns

**Dependency Injection:**
- `AppDependencies.shared` singleton holds all major services
- Initialized in `AppDependencies` class (likely in `/App/AppDependencies.swift`)

**Repository Pattern:**
- `ProfilesRepository` — Manages profiles and disk I/O
- `WorkspaceRepository` — Manages workspaces (delegates save to ProfilesRepository)
- `SettingsRepository` — Manages all settings

**Settings Split Pattern:**
- `AppSettings` — Single struct that gets serialized to disk
- Feature-specific `*Settings` classes — Split the struct into manageable pieces
- `SettingsProtocol` — Standardizes load/save logic

**Notification-Based Events:**
- Profile change → `.profileChanged`
- App list change → `.appsListChanged`
- Open main window → `.openMainWindow`
- Workspace activation → Integration hooks

**Combine Reactive:**
- Most @Published properties use Combine for binding
- Changes debounced to avoid excessive saves
- PassthroughSubject used for one-way events

