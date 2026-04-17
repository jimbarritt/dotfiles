# macOS Tips & Gotchas

Miscellaneous macOS behaviours that are non-obvious or easy to forget.

## Hot Corners

macOS can trigger actions when you move the mouse to a screen corner (**System Settings > Desktop & Dock > Hot Corners**).

**Quick Note** is often set by default on the bottom-right corner — hovering there makes a small Notes icon appear; clicking it opens a Quick Note in the Notes app. Easy to mistake for a stuck widget or desktop shortcut.

To disable: set that corner to **"—"** in the Hot Corners dialog.

## Diagnosing apps that prevent the screensaver / sleep

If the screensaver stops triggering, an app is likely holding a power assertion. Check what's currently asserting:

```sh
pmset -g assertions
```

For a historical log (useful if the problem only happens when you're away):

```sh
pmset -g log | grep -E "PreventUserIdle|UserIsActive" | tail -40
```

Things to look for:

- **`PreventUserIdleDisplaySleep`** — directly blocks the screensaver and display sleep
- **`PreventUserIdleSystemSleep`** — keeps the system awake (less likely to block screensaver alone)
- **`UserIsActive`** — resets the idle timer; if something keeps triggering this, the screensaver never fires

Common culprits: Microsoft Teams (holds network/storage wake locks even when idle), any app playing audio (`coreaudiod` holds assertions while audio is active including Voice Memos recordings), video conferencing apps.
