# Vim mode in Claude Code

Claude Code's input works like a single-line editor. By default, `Escape` exits the input (cancels insert mode), but if you're used to vim-style `Ctrl+G` to exit insert mode, you need to remap at the terminal level.

## Why terminal-level remapping?

Claude Code keybindings can map keys to actions, but `chat:cancel` (exit input) relies on receiving the actual escape keycode (`\x1b`). Remapping `ctrl+g` to `chat:cancel` in `~/.claude/keybindings.json` doesn't work because the app never sees the escape keycode — it sees `\x07` (BEL).

The fix is to make the terminal send escape when you press `Ctrl+G`.

## Ghostty config

Add this to `~/.config/ghostty/config`:

```
# ctrl+g sends escape (for exiting insert mode in Claude Code)
keybind = ctrl+g=text:\x1b
```

Reload with `Cmd+Shift+,` or restart ghostty.

## Claude Code keybindings

With the terminal handling the remap, you don't need any `ctrl+g` entry in `~/.claude/keybindings.json`. The default `escape` → `chat:cancel` binding does the rest.

Other useful keybindings in `~/.claude/keybindings.json`:

```json
{
  "$schema": "https://www.schemastore.org/claude-code-keybindings.json",
  "$docs": "https://code.claude.com/docs/en/keybindings",
  "bindings": [
    {
      "context": "Chat",
      "bindings": {
        "ctrl+x ctrl+e": "chat:externalEditor",
        "alt+enter": "chat:newline"
      }
    }
  ]
}
```

- `Ctrl+X Ctrl+E` — open the input in your `$EDITOR` (replaces the default `Ctrl+G` which we remapped)
- `Alt+Enter` — insert a newline in the input

## Note

This remap is global to ghostty — `Ctrl+G` will act as escape in all terminal apps, not just Claude Code. This is generally fine since `Ctrl+G` has the same meaning in vim/nvim.
