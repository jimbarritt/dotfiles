# Symlinking notes

## `ln -shf` vs `ln -sf`

`do.sh` uses `ln -shf` to create symlinks. The `-h` flag is important when re-running `just link` after initial setup.

### The problem with `ln -sf`

`ln -sf A B` behaves differently depending on what already exists at `B`:

| State of `B` | Behaviour |
|---|---|
| Doesn't exist | Creates `B → A` ✓ |
| Exists as a symlink to a file | Replaces it with `B → A` ✓ |
| Exists as a symlink to a directory | Follows the symlink into the dir and creates `A` inside it ✗ |
| Exists as a real directory | Fails with "is a directory" (both `-sf` and `-shf`) |

The third case is the gotcha. When `~/.config/nvim` already exists as a symlink pointing at `config/nvim/` (a directory), running `ln -sf config/nvim ~/.config/nvim` doesn't replace the symlink — it follows it and creates `config/nvim/nvim → …/config/nvim` inside the repo. A recursive self-reference.

### Why `-h` fixes it

`-h` tells `ln` not to follow symlinks at the destination — treat `B` as a plain path and replace whatever is there, regardless of where it points.

### Why it only bites on the second run

The first time `just link` runs, `~/.config/nvim` doesn't exist yet, so `ln -sf` just creates it cleanly. The bug only surfaces when you re-run `just link` after the symlinks are already in place.
