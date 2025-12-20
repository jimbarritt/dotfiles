# UK Keyboard: Swapping # and £ Keys

## Problem

On a UK physical keyboard with macOS British layout:
- `Shift+3` produces `£` (pound sign)
- `Option+3` produces `#` (hash/number sign)

For developers, this is backwards - we want:
- `Shift+3` → `#` (more commonly used in programming)
- `Option+3` → `£` (less frequently needed)

## Solution Overview

The solution requires **two configurations**:
1. **Karabiner-Elements**: Swap the key outputs at the system level
2. **Kitty terminal**: Additional mapping to handle the remapped keys correctly

## Why Both Are Needed

Karabiner swaps the keys system-wide, but terminal emulators interpret `Option` key presses as escape sequences (Alt key behavior). This causes the remapped `Shift+3` (which Karabiner sends as `Option+3`) to be interpreted as `ESC+3` in Kitty, which can trigger unwanted behavior like exiting insert mode in Neovim.

## Configuration

### 1. Karabiner-Elements Configuration

**File:** `~/.config/karabiner/karabiner.json`

Add this to your complex modifications:

```json
{
    "description": "Swap # and £ on UK keyboard",
    "manipulators": [
        {
            "description": "Shift+3 → # (swap from £)",
            "from": {
                "key_code": "3",
                "modifiers": { 
                    "mandatory": ["shift"],
                    "optional": ["caps_lock"]
                }
            },
            "to": [
                {
                    "key_code": "3",
                    "modifiers": ["option"]
                }
            ],
            "type": "basic"
        },
        {
            "description": "Option+3 → £ (swap from #)",
            "from": {
                "key_code": "3",
                "modifiers": { 
                    "mandatory": ["option"],
                    "optional": ["caps_lock"]
                }
            },
            "to": [
                {
                    "key_code": "3",
                    "modifiers": ["shift"]
                }
            ],
            "type": "basic"
        }
    ]
}
```

**What this does:**
- Intercepts `Shift+3` and sends `Option+3` (which outputs `#` on UK layout)
- Intercepts `Option+3` and sends `Shift+3` (which outputs `£` on UK layout)

### 2. Kitty Terminal Configuration

**File:** `~/.config/kitty/kitty.conf`

Add these lines:

```conf
# Intercept the remapped Shift+3 (which Karabiner sends as Option+3)
# and output # character directly
map alt+3 send_text all #

# Ensure Option is not treated as Alt modifier
macos_option_as_alt no
```

**What this does:**
- `map alt+3 send_text all #`: When Kitty receives `Option+3` (from Karabiner's remapped `Shift+3`), output `#` directly instead of treating it as an escape sequence
- `macos_option_as_alt no`: Prevents Kitty from interpreting Option keys as Alt modifiers, which would send escape sequences

### 3. Reload Configurations

**Karabiner:**
- Changes are automatically applied when you save the JSON file
- Or restart Karabiner-Elements

**Kitty:**
- Press `Ctrl+Shift+F5` (or `Cmd+Shift+F5` on macOS)
- Or restart Kitty

## Result

After applying both configurations:

**In all applications (browser, text editors, etc.):**
- `Shift+3` → `#`
- `Option+3` → `£`

**In Kitty terminal (including Neovim, shell, etc.):**
- `Shift+3` → `#`
- `Option+3` → `£`

## Verification

Test in different contexts:

```bash
# At the shell prompt
echo [press Shift+3]  # Should output: #
echo [press Option+3]  # Should output: £

# In Neovim insert mode
# Type Shift+3 - should insert # without exiting insert mode
# Type Option+3 - should insert £
```

## Troubleshooting

### Shift+3 exits insert mode in Neovim
**Cause:** Kitty is treating `Option+3` as an escape sequence

**Fix:** Ensure `map alt+3 send_text all #` is in your `kitty.conf` and reload

### Option+3 produces # instead of £
**Cause:** The Kitty mapping is interfering with the actual `Option+3` press

**Fix:** This should work correctly if both Karabiner manipulators are in place. Check your Karabiner rules.

### Works in browser but not in Kitty
**Cause:** Missing the Kitty-specific configuration

**Fix:** Add the `map alt+3 send_text all #` line to `kitty.conf`

## Notes

- This configuration is specific to UK keyboard layout with macOS
- The approach works because Karabiner operates at the system level, while Kitty needs additional configuration to handle terminal-specific behavior
- `Option` key in macOS terminals typically sends escape sequences (ESC+key), which is why the Kitty mapping is necessary
- Other terminal emulators (iTerm2, Alacritty, etc.) may require similar workarounds

## Related Configuration

If you're also using other Karabiner mappings (like Control key modifications), ensure they don't conflict with these rules. The `"optional": ["caps_lock"]` in the Karabiner rules ensures the swap works regardless of Caps Lock state.
