# yabai — reference

Tiling window manager for macOS. C daemon + CLI + optional code-injection into
`Dock.app` ("scripting addition") for operations the public window APIs cannot
reach. The scripting addition is what forces the SIP conversation.

Sources: <https://github.com/koekeishiya/yabai> (now mirrored/maintained by
`asmvik` fork on recent releases — current brew tap is `asmvik/formulae/yabai`,
v7.1.18 as of Mar 2026).

---

## Architecture

| Component | Role |
|---|---|
| `yabai` daemon | Long-running process. Tracks windows/spaces/displays via Accessibility + private SkyLight APIs. |
| `yabai -m <domain> <command>` CLI | Sends messages over a Unix socket (`/tmp/yabai_$USER.socket`). Same binary, different mode. |
| Scripting Addition (SA) | Mach-O dylib injected into `Dock.app`. Calls private SkyLight functions in-process to manipulate Spaces, layering, transparency, sticky windows. |
| skhd (separate project) | Companion hotkey daemon. Optional — Hammerspoon does this fine. |

Message protocol: plain text, domain/command/args. `yabai -m query --windows`
returns JSON. Trivial to script from any language; `hs.execute("yabai -m ...")`
works directly.

---

## SIP requirements

yabai works in two modes. The line is sharp.

### Without SIP changes

- Window tiling (bsp / float, focus, swap, warp, resize)
- Window queries (`yabai -m query --windows --spaces --displays`)
- Per-app rules (managed/unmanaged/float)
- Multi-display window movement
- Signals (event subscriptions)

### Requires SIP partial-disable + scripting addition loaded

- Create / destroy / focus / move / reorder Spaces
- Move window to Space (reliably)
- Sticky windows (visible on all Spaces)
- Window opacity, shadow removal, layering
- Picture-in-picture toggle
- Scratchpads (the SA-backed kind)

If you only need tiling on existing Spaces, **you do not need to disable SIP**.
The moment you want yabai to manage Spaces themselves, you do.

### `csrutil` recipe (Apple Silicon, macOS 13+ incl. Sequoia)

From the Recovery Terminal:

```sh
csrutil enable --without fs --without debug --without nvram
```

Then, booted normally:

```sh
sudo nvram boot-args=-arm64e_preview_abi
```

Disables: filesystem protection, debug restrictions, NVRAM protection.
Leaves enabled: kext signing, base system protection, AMFI.

**What you give up by disabling these:**

- `--without fs`: unsigned/modified processes can patch protected system files
  and tamper with `/System`. Malware that gains user-level execution has a
  larger attack surface for persistence.
- `--without debug`: any process can attach a debugger to any other process,
  including Apple-signed ones. Enables runtime patching of system binaries.
- `--without nvram`: NVRAM variables (including `boot-args`) become writable.
  Required for the `arm64e_preview_abi` flag the SA needs.

This is the configuration upstream documents and the community runs with.
It is meaningfully weaker than stock macOS — not catastrophic, but real.

### Reversibility

`csrutil enable` from Recovery restores full SIP. `sudo nvram -d boot-args`
clears the boot arg. No filesystem changes to undo.

---

## Features

| Area | Notes |
|---|---|
| Layouts | `bsp` (binary space partition), `stack` (tabs), `float` |
| Tiling ops | focus / swap / warp / resize / split-direction / balance / mirror |
| Spaces | create, destroy, focus, label, move-to-display (SA required) |
| Window→Space | `yabai -m window --space <SEL>` (SA required for reliable cross-Space move) |
| Rules | `yabai -m rule --add app="^Zen$" manage=off` — match by app/title/role/subrole |
| Signals | Subscribe to events: `window_created`, `window_focused`, `space_changed`, `display_added`, `application_launched`, etc. Action is a shell command. |
| Scratchpads | Hidden-by-default windows toggled by name (SA required) |
| Multi-display | First-class. `yabai -m display --focus next`, per-display Spaces, move window to display. |
| Mouse | `mouse_modifier` + drag to move/resize, `focus_follows_mouse` |
| Query | `yabai -m query --windows|--spaces|--displays` returns JSON |

---

## Configuration

`~/.config/yabai/yabairc` — a `chmod +x` shell script run on daemon start.
Anything you can run from the CLI you can put in here. Reload: `yabai --restart-service`
or `yabai -m config <key> <value>` at runtime.

Minimal working config (no SIP required):

```sh
#!/usr/bin/env sh

yabai -m config layout                  bsp
yabai -m config window_gap              8
yabai -m config top_padding             8
yabai -m config bottom_padding          8
yabai -m config left_padding            8
yabai -m config right_padding           8
yabai -m config mouse_modifier          fn
yabai -m config focus_follows_mouse     autofocus
yabai -m config mouse_follows_focus     on

# Float by default for these
yabai -m rule --add app="^System Settings$"  manage=off
yabai -m rule --add app="^Finder$"           manage=off
yabai -m rule --add app="^1Password$"        manage=off

echo "yabai configuration loaded.."
```

Add this if SA is loaded (Spaces control etc.):

```sh
yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
sudo yabai --load-sa
```

---

## Composition with Hammerspoon

The two compose cleanly. Responsibility split:

| Concern | Owner |
|---|---|
| Window layout, tiling rules, Space ops | yabai |
| Hotkey bindings | Hammerspoon (`hs.hotkey.bind`) |
| Workspace semantics (named app sets, per-display state) | Hammerspoon |
| UI overlays, menu bar items, choosers | Hammerspoon |
| Event-driven glue (e.g. on-focus rules) | yabai signals → shell → Hammerspoon URL handler |

Patterns:

```lua
-- Hammerspoon → yabai
hs.execute("/opt/homebrew/bin/yabai -m window --focus west")

-- yabai → Hammerspoon (via signal)
-- in yabairc:
-- yabai -m signal --add event=space_changed \
--   action='open "hammerspoon://space-changed?id=$YABAI_SPACE_ID"'
```

This is more powerful than yabai's built-in `skhd` pairing because Hammerspoon
gives you arbitrary Lua, not just keymaps. yabai becomes a layout primitive
behind your own workspace model.

---

## Multi-monitor on Sequoia + Apple Silicon

Per-display Spaces are the supported mode and require **"Displays have separate
Spaces" = ON** (which you already have). yabai's per-display query and movement
work; behaviour to be aware of:

- Hot-plug recovery is partial. Reattaching an external display sometimes
  leaves windows on a phantom Space. `yabai --restart-service` is the
  documented fix.
- Display identity in yabai is by index (1, 2, 3 ordered by AppKit), not
  serial. Reorder in System Settings → display arrangement → indices shift.
- macOS Sequoia's "Window Tiling" snap feature is independent of yabai and
  does not interfere; the SA injection itself is not affected by Sequoia
  point releases as of v7.1.x.

### Recent open issues to be aware of

| # | Title | Relevance |
|---|---|---|
| [#2777](https://github.com/koekeishiya/yabai/issues) | Sending window to new space doesn't work | Affects Space ops with SA |
| [#2776](https://github.com/koekeishiya/yabai/issues) | Windowed-fullscreen ignores external bar | Multi-display fullscreen |
| [#2762](https://github.com/koekeishiya/yabai/issues) | Managed windows appear faded (Tahoe 26.3) | Future-OS rendering |
| [#2761](https://github.com/koekeishiya/yabai/issues) | Buffer overflow with >32 windows in a Space | Edge case but real |

The repo is now under active maintenance via the `asmvik` fork; release
cadence has held through Sequoia/Tahoe.

---

## Installation

```sh
# Daemon
brew install asmvik/formulae/yabai
yabai --start-service                     # prompts for Accessibility

# Permissions to grant in System Settings → Privacy & Security:
#   - Accessibility:      yabai
#   - Screen Recording:   yabai (only needed for window animations / borders)

# Scripting addition (only if you want SIP-gated features)
sudo yabai --install-sa                   # installs into /Library/.../Dock
sudo yabai --load-sa                      # injects into running Dock

# Password-less sudo for --load-sa, so dock_did_restart signal can re-inject
echo "$(whoami) ALL=(root) NOPASSWD: sha256:$(shasum -a 256 $(which yabai) | cut -d ' ' -f 1) $(which yabai) --load-sa" \
  | sudo tee /private/etc/sudoers.d/yabai
```

The sudoers entry pins the binary hash. Any yabai upgrade requires regenerating
it, or `--load-sa` will silently prompt and the signal handler will fail.

---

## Debugging

| Command | Purpose |
|---|---|
| `yabai --start-service` / `--stop-service` / `--restart-service` | Lifecycle |
| `yabai -m query --windows` | All windows as JSON |
| `yabai -m query --spaces --display` | Spaces on focused display |
| `yabai -m query --displays` | All displays |
| `yabai -m signal --list` | Active signals |
| `yabai -m rule --list` | Active rules |
| `tail -f /tmp/yabai_$USER.out.log` | Daemon stdout (when started via `brew services` / `--start-service`) |
| `tail -f /tmp/yabai_$USER.err.log` | Daemon stderr |
| `yabai --uninstall-sa` | Remove the scripting addition cleanly |

`launchctl print gui/$(id -u)/com.koekeishiya.yabai` shows service state.

---

## Risks

- **SIP weakening is real** (see SIP section). For a personal dev machine on
  trusted networks the threat model is acceptable to most people who run yabai;
  for a managed/work machine it is usually a non-starter.
- **Private SkyLight APIs.** Every macOS major (and sometimes minor) release
  can break the SA. yabai usually ships a fix within days, but you will be
  running broken Spaces for that window.
- **Maintainer cadence.** Project moved from `koekeishiya` to `asmvik` fork
  for recent releases. Bus factor is small; community has been forking
  responsibly so far.
- **Sudoers hash drift.** Brew upgrades change the binary hash and break
  `sudo yabai --load-sa` silently. Regenerate the sudoers line on upgrade
  (or wrap in a helper).
- **Uninstall:** `yabai --uninstall-sa`, `brew uninstall yabai`,
  `rm /private/etc/sudoers.d/yabai`, restore SIP from Recovery if desired.

---

## vs FlashSpace

| Dimension | yabai | FlashSpace |
|---|---|---|
| SIP | Partial disable for Space ops; none for tiling-only | None |
| Granularity | Per-window | Per-app (`hide()`/`unhide()` whole process) |
| Layout | First-class tiling (bsp/stack) | None — visibility only |
| Spaces model | Manipulates real macOS Spaces | All workspaces share one native Space |
| Multi-monitor | Per-display Spaces, hot-plug fragile | Display matched by `localizedName`, multi-monitor unreliable in your testing |
| Mission Control | Clean (uses real Spaces) | Clean (single Space) |
| Config | Shell script (`yabairc`) | TOML, partly GUI-managed |
| Perf | Native C daemon, very fast | Swift, fast enough |
| Failure mode | SA breaks on OS update; tiling keeps working | Wipe-on-decode bug, silent display matching failures |

The two are not direct competitors. yabai is a tiling WM that also moves
windows between Spaces. FlashSpace is an app-visibility orchestrator that
doesn't touch Spaces or layout. AeroSpace is the closer comparator to yabai
(see [bespoke-poc-with-hammerspoon.md](./bespoke-poc-with-hammerspoon.md) §2).

---

## Verdict for this project

Framed as input, not a final call. Your stated constraints: reliability above
all, multi-monitor desk-A, Sequoia, per-window workspaces preferred,
Hammerspoon for hotkeys regardless.

**The case for yabai:**

- Per-window membership is what you actually want — FlashSpace's app-granularity
  is the root cause of your hide/show pain on multi-monitor.
- Tiling is a solved problem under yabai; you stop building it in Hammerspoon.
- Composes well with Hammerspoon for the workspace-naming layer you'd build
  anyway.
- Tiling-only mode (no SIP) is usable as a stepping stone — try that first,
  decide on SA later.

**The case against:**

- The SIP recipe is a real reduction in system security. On a personal M2 Pro,
  most yabai users accept it; you have to make that call deliberately.
- Private API fragility. Any Sequoia point release can break SA features for
  a few days. Tiling-only stays working.
- Hot-plug recovery on multi-monitor is imperfect (issue #2777-class
  problems). `yabai --restart-service` is a real part of the workflow.
- One more daemon to run, sudoers entry to maintain, hash to refresh on upgrade.

**Suggested path:** install yabai, **do not disable SIP yet**, run tiling-only
on your existing native Spaces with Hammerspoon hotkeys driving it. If after
a week the per-window tiling alone solves the FlashSpace pain, stop there.
If you find yourself wanting yabai-managed Spaces (sticky windows, programmatic
Space creation, scratchpads), then make the SIP call with eyes open.

This is materially different from your previous yabai attempt: you are no
longer committing to the full SIP-disabled experience as a precondition.

---

## References

- <https://github.com/koekeishiya/yabai>
- <https://github.com/koekeishiya/yabai/wiki/Installing-yabai-(latest-release)>
- <https://github.com/koekeishiya/yabai/wiki/Disabling-System-Integrity-Protection>
- <https://github.com/koekeishiya/yabai/wiki/Configuration>
- <https://github.com/koekeishiya/yabai/issues>
- [macos-native-spaces.md](./macos-native-spaces.md) — Spaces API landscape
- [flashspace-reference.md](./flashspace-reference.md) — comparator
- [bespoke-poc-with-hammerspoon.md](./bespoke-poc-with-hammerspoon.md) — Hammerspoon glue
