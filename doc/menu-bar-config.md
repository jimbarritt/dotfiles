# macOS Menu Bar Configuration

Notes on taming the macOS menu bar: rearranging icons, hiding system items, and dealing with apps that refuse to get out of the way.

## Rearranging icons

Hold **⌘ (Cmd)** and drag any menu bar icon left/right. Works for:

- System icons (Wi-Fi, Battery, Clock, Control Center, Spotlight, Focus, etc.)
- Most well-behaved third-party status items

It does **not** work for:

- The Apple menu or app menus on the left (they're fixed)
- Control Center itself (pinned next to the clock, but individual Control Center *modules* can be dragged out onto the menu bar)
- Spotlight can be moved but always wants to stay near the right edge
- Poorly-behaved third-party apps that use non-standard status item APIs

## Hiding system items

Everything system-provided lives under **System Settings → Control Center**. Each module has a "Show in Menu Bar" dropdown:

- **Don't Show** — hidden from the menu bar entirely
- **Show in Menu Bar** — always visible
- **Always Show in Menu Bar** / **Show When Active** — depends on the module

The "Show in Control Center" toggle is separate — you can hide an icon from the bar but still reach it by clicking the Control Center icon.

### Common things to hide

| Item | Why you might hide it |
|---|---|
| **Spotlight** | If you have a keybinding (⌘-Space or Raycast's ⌥-Space), the icon is redundant |
| **Fast User Switching** | Only useful if you actually switch accounts — otherwise reachable via Apple menu → Lock Screen / Log Out |
| **Focus** | If you use Do Not Disturb via keyboard shortcut or Control Center |
| **AirDrop / AirPlay** | Usually reachable via Control Center |

### Spotlight fallback (if the toggle is missing)

Some macOS releases grey out or omit the Spotlight "Show in Menu Bar" setting. Terminal workaround:

```bash
defaults write com.apple.Spotlight MenuItemHidden -int 1
killall SystemUIServer
```

Reverse with `-int 0`.

## Per-user, not system-wide

All menu bar configuration is **per-user** — it's written to `~/Library/Preferences/` and each account on the Mac has its own layout, Control Center modules, clock format, Focus modes, and so on. Changing yours will never affect another user on the same machine.

The only genuinely system-wide bits of the UI live in `/Library/LaunchDaemons`, `/etc/`, and kernel extensions — none of which touch the menu bar.

## When System Settings isn't enough: Ice

Some apps don't expose a "hide from menu bar" option at all. Known offenders:

- **Sophos** (security agent — no visibility toggle)
- **Logi Tune** (Logitech camera utility — no visibility toggle)
- Various vendor-installed helpers

For these, you need a menu bar manager. The common options:

| Tool | Free? | Notes |
|---|---|---|
| [Ice](https://github.com/jordanbaird/Ice) | Yes, open source | Actively maintained, handles notch overflow well |
| [Hidden Bar](https://github.com/dwarvesf/hidden) | Yes, open source | Simpler — just hides items behind a toggle |
| [Bartender](https://www.macbartender.com/) | Paid | Commercial, longest track record, most features |

### How Ice organises things

Ice splits the menu bar into three sections, each with its own chevron:

| Section | Behaviour |
|---|---|
| **Visible** | Always shown |
| **Hidden** | Tucked behind a `‹` chevron — click to reveal, click again to collapse |
| **Always-Hidden** | Tucked behind a second chevron, optionally with a keyboard shortcut to reveal |

Drag icons between sections to organise. The "Hidden" section is the "folder" pattern — all your noisy-but-occasionally-needed icons behind one click.

### Install

```bash
brew install --cask ice
```

## 1Password and Touch ID — the myth

A common worry: "if I hide 1Password from the menu bar, will Touch ID stop working?"

**No.** Touch ID is handled by the **1Password Helper** background LaunchAgent, not by the menu bar icon. The icon is just a visible surface. As long as the app is running (which it will be, whether or not the icon is shown), Touch ID unlock works for the app, browser extension, CLI, and SSH agent.

| Action | Touch ID still works? |
|---|---|
| Hide the menu bar icon via Ice / Bartender | Yes |
| Uncheck "Always keep 1Password in the menu bar" in 1Password settings | Yes |
| Hide the Dock icon as well (run headless) | Yes |
| Fully **quit** 1Password (⌘Q) | No |
| Disable the 1Password Helper LaunchAgent | No |

The myth comes from 1Password 7 and early 1Password 8, where hiding the menu bar icon was the accidental path to the app fully exiting. Current 1Password 8 decouples visibility from liveness — you can hide it from both the menu bar and the Dock and it stays alive.

Verify on your machine:

```bash
launchctl list | grep -i 1password
```

You should see `com.1password.*` entries. The setting that actually matters is **1Password → Settings → Security → Unlock using Touch ID** — not the menu bar visibility one.

## Example layout (this machine)

After cleanup, the visible row is:

```
[Claude]  ~~~audio~~~  ▲  S  🔋  📶  ⚙  Sun 12 Apr  03:45
```

- `[Claude]` — FlashSpace workspace label (context without using a real icon slot)
- Audio waveform — **Logi Tune** (can't be hidden natively — Ice candidate)
- Triangle — **Sophos** antivirus (can't be hidden natively — Ice candidate)
- S — another Sophos component (endpoint/self-service, also not hideable)
- Battery, Wi-Fi, Control Center, clock — system

Items removed during cleanup:

- Raycast (reachable via ⌥-Space)
- Insomnia/Caffeine (reachable via its own keybinding)
- 1Password (Touch ID still works)
- Do Not Disturb / Focus (reachable via Control Center)
- Spotlight (reachable via ⌘-Space)
- Fast User Switching (reachable via Apple menu → Lock Screen)

### Target layout once Ice is installed

| Section | Items |
|---|---|
| **Visible** | `[Claude]`, battery, Wi-Fi, Control Center, clock |
| **Hidden** | Logi Tune, both Sophos icons |

Leaves five visible items plus a chevron for the rest.

## Related

- [universal-keybinding-window-layers.md](universal-keybinding-window-layers.md) — keyboard-first navigation strategy that makes many menu bar icons redundant
