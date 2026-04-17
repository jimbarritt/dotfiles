# Tilr Swift — Starter App Plan

A foundation-only Swift app that proves out the primitives we'll need for a full
native port of the Hammerspoon Tilr (`home/hammerspoon/init.lua`). No window
management yet — just global hotkeys, a menu-bar label, a popup, TOML config,
and a two-file state model.

Once this skeleton is alive, we decide whether to grow it into a full native
replacement for the Lua implementation or keep both in parallel.

---

## 1. Goals

Deliver a native `.app` that:

1. Shows the currently-active Tilr space in the menu bar.
2. Binds global hotkeys (from config) that switch the active space.
3. Displays a popup like today's `hs.alert.show` when a space activates.
4. Loads space definitions from a user-owned TOML config.
5. Persists runtime state to a separate private file (not the user config).

**Deliberately out of scope for the starter:**

- Window moving, app hide/unhide, Accessibility API
- Multi-monitor behaviour, screen precedence, prefer-lists
- Sidebar layouts, ratios, session overrides beyond `active_space`
- Settings UI (edit config.toml directly)
- Per-display menu bars

These all live in the Lua version today. The starter's job is to ensure the
Swift scaffolding is solid before we start porting the interesting logic.

---

## 2. Two-file state model

One of the key lessons from the Lua version: baking runtime state into the
user's config file gets messy fast (see prior session's "per-space ratio
persistence" problem). The Swift app splits it cleanly.

### `~/.config/tilr/config.toml` — user-owned, declarative, version-controllable

```toml
[[spaces]]
name = "Coding"
hotkey = "cmd+alt+1"

[[spaces]]
name = "Reference"
hotkey = "cmd+alt+2"

[[spaces]]
name = "Scratch"
hotkey = "cmd+alt+3"
```

### `~/Library/Application Support/tilr/state.toml` — app-owned, runtime

```toml
[state]
active_space = "Coding"
last_changed = "2026-04-17T20:45:00+0100"

# future: sessionScreenOverride, sessionAppOverride, per-space ratios
```

**Why this split is load-bearing:**

- User edits `config.toml`; the app never writes to it.
- Runtime events (space activations, overrides, ratios) write only to
  `state.toml`.
- Resetting session state = delete `state.toml`. Config is untouched.
- `state.toml` can be in a `.gitignored` location; `config.toml` can live in
  dotfiles.

---

## 3. Tech stack

| Concern | Choice | Reason |
|---|---|---|
| Project type | Xcode App project, `LSUIElement=true` | Needs real `.app` bundle for eventual AX permissions |
| Swift | 5.9+, Xcode 15+ | Already on this machine |
| UI framework | AppKit primary, SwiftUI for popup | `NSStatusItem` is AppKit-only; SwiftUI is nicer for the popup view |
| Hotkeys | [soffes/HotKey](https://github.com/soffes/HotKey) via SPM | Carbon-based, stable, ~2k stars |
| TOML | [LebJe/TOMLKit](https://github.com/LebJe/TOMLKit) via SPM | Modern, `Codable`-friendly |
| Config dir | `~/.config/tilr/` | Matches dotfiles convention |
| State dir | `~/Library/Application Support/tilr/` | Standard macOS app-data location |

---

## 4. Architecture sketch

```
ConfigLoader        reads config.toml → [SpaceConfig]
StateStore          reads/writes state.toml; publishes activeSpace
SpaceRegistry       in-memory list of Spaces, built from ConfigLoader
HotKeyManager       registers HotKey per space; dispatches to SpaceActivator
SpaceActivator      activate(name): StateStore.setActive + PopupWindow.show
MenuBarController   owns NSStatusItem; observes StateStore.activeSpace
PopupWindow         borderless floating NSPanel with fade in/out
```

### Event flow

```
hotkey fires
  → SpaceActivator.activate("Coding")
    → StateStore.setActive("Coding")       (writes state.toml, publishes change)
    → PopupWindow.show("Coding")           (borderless panel on focused screen)
  → MenuBarController observer
    → NSStatusItem.button.title = "Coding"
```

The `StateStore` is the single source of truth for runtime. Every UI surface
(menu bar, popup, any future overlay) subscribes to it.

---

## 5. Repo layout

**Decision needed:** Swift project lives where?

- **Option A** — new top-level dir in dotfiles (`swift/Tilr/`)
- **Option B** — separate repo (`~/Code/github/jimbarritt/tilr/`)

**Recommendation: Option B.** Dotfiles is for configuration, not app source.
A separate repo gives a clean place for Releases, Issues, a proper README, and
keeps dotfiles small. Link from here to the repo once it exists.

---

## 6. Delta plan

Each delta is an end-to-end working deliverable. The order is deliberately
front-loaded on *visible* wins (popup, menu bar) so there's something to
demo after every delta.

### Delta 0 — Skeleton (~0.5d)
- Xcode project, `LSUIElement=true`, app icon placeholder
- SPM deps: HotKey, TOMLKit
- `NSStatusItem` with static title "Tilr", Quit menu item
- **Deliverable:** app runs, shows in menu bar, quits cleanly

### Delta 1 — Popup alert (~0.5d)
- Borderless `NSPanel` (`.nonactivating`, `.utilityWindow` style mask minus
  title bar; transparent background; vibrant centre label)
- SwiftUI view for the label; fade in, hold 200ms, fade out (matches current
  `hs.alert.show` feel)
- Menu item "Show popup" → renders "Hello, Tilr"
- **Deliverable:** popup triggerable from menu, positioned on focused screen

### Delta 2 — Hotkey → popup (~0.5d)
- Register hardcoded `cmd+alt+t`
- Press → show popup "Tilr fired"
- **Deliverable:** global hotkey working, popup responds

### Delta 3 — Config loading (~0.5d)
- `SpaceConfig: Codable` struct, `ConfigLoader.load()` via TOMLKit
- Load `~/.config/tilr/config.toml` on launch; log parsed spaces
- Handle missing file gracefully (write a default config)
- **Deliverable:** config parsed, validated, sensible error if malformed

### Delta 4 — Hotkeys from config (~0.5d)
- For each space in config, register its hotkey via HotKeyManager
- Press → popup shows that space's name
- **Deliverable:** config-driven hotkeys, popup shows space name

### Delta 5 — State file (~0.5d)
- `StateStore` with Combine publisher for `activeSpace`
- Load/save `~/Library/Application Support/tilr/state.toml`
- On hotkey fire: `StateStore.setActive(name)` persists and publishes
- **Deliverable:** state survives restart; active space restored on launch

### Delta 6 — Menu bar title (~0.5d)
- `MenuBarController` subscribes to `StateStore.$activeSpace`
- `NSStatusItem.button.title` updates live; format: `[Coding]`, `[Reference]`
- **Deliverable:** menu bar always shows current space name in brackets

### Delta 7 — Polish (~0.5d)
- Config file watch → hot reload on save
- Launch at login via `SMAppService`
- App icon + About dialog polish
- **Deliverable:** shippable starter

**Total: ~4 focused days.** None of this touches AX API or app hiding — the
expensive bits are deferred until the scaffolding is proven.

---

## 7. Publishing & signing

Both Tilr and Marq publish under **Ubiqtek Ltd** using the existing Apple
Developer Program enrollment. Consolidating both apps under one identity
avoids a second $99/yr personal enrollment and keeps branding consistent.

| Item | Value |
|---|---|
| Team / signer | Ubiqtek Ltd (existing org enrollment) |
| Bundle ID | `io.ubiqtek.tilr` (reverse DNS of `ubiqtek.io`) |
| Signing | Developer ID Application cert, notarized via `notarytool`, stapled |
| Distribution | Homebrew cask, served from `ubiqtek/tap` (see below) |

### Homebrew tap migration (prerequisite, affects Marq too)

Marq currently lives at `jimbarritt/tap/marq` (GitHub: `jimbarritt/homebrew-tap`).
Before Tilr's first release, transfer the tap to the Ubiqtek org:

1. GitHub → `jimbarritt/homebrew-tap` → Settings → **Transfer ownership** to
   `ubiqtek`. GitHub auto-redirects the old URL, so existing
   `brew install jimbarritt/tap/marq` keeps working for current users.
2. Update Marq's README + cask metadata to show `brew tap ubiqtek/tap`.
3. Tilr's cask lands in the transferred repo under its new canonical name.

Existing Marq users aren't forced to re-tap (redirect handles it); nudge them
in release notes.

---

## 8. Resolved decisions

- **Bundle ID** → `io.ubiqtek.tilr`
- **Code signing** → Ubiqtek Developer ID (see §7)
- **Distribution** → `ubiqtek/tap` via Homebrew cask
- **Project location** → new repo `github.com/ubiqtek/tilr`
- **Config schema** → fresh and minimal; don't reuse the Lua shape. We'll
  converge later if we go full native.
- **Menu bar format** → bracket-wrapped active space name: `[Coding]`,
  `[Reference]`, `[Scratch]`. Plain text, no icon.
- **Hammerspoon coexistence** → not shared; when running Swift Tilr, the user
  quits Hammerspoon first. This means Swift Tilr can use the same hotkeys as
  the Lua version (`cmd+alt+1`, etc.) without collision.

---

## 9. Next step

Transfer the Homebrew tap to Ubiqtek, create `github.com/ubiqtek/tilr`, then
start **Delta 0**. Each delta ships a demoable increment, so we can
re-evaluate scope after any of them.
