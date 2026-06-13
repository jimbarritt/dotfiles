# Claude Config

This directory (`home/claude/`) is the source of truth for Claude Code config. Files here are
symlinked into `~/.claude/` — but **`do.sh` does not handle Claude linking yet**. All symlinks
must be created manually (tracked below until `do.sh` is updated).

---

## Manual symlinks required

Run these when setting up a new machine, or after adding new skills/themes.

### Top-level files

```sh
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/settings.json ~/.claude/settings.json
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/keybindings.json ~/.claude/keybindings.json
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/statusline-command.sh ~/.claude/statusline-command.sh
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/CLAUDE.md ~/.claude/CLAUDE.md
```

### Skills

Each skill is a directory. Symlink the whole directory, not individual files inside it.

```sh
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/skills/init-plan ~/.claude/skills/init-plan
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/skills/load-plan ~/.claude/skills/load-plan
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/skills/update-plan ~/.claude/skills/update-plan
```

> **Note:** `proofread/` and `publish-crate/` in `~/.claude/skills/` are real directories (not
> symlinked to dotfiles). If you want them version-controlled, copy them here and add symlinks.

### Themes

```sh
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/themes/green-tinted-dark.json ~/.claude/themes/green-tinted-dark.json
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/themes/green-tinted-light.json ~/.claude/themes/green-tinted-light.json
```

---

## TODO: wire into do.sh

Add a `link_claude` function to `do.sh` so new-machine setup is a single command.
The function should symlink: `settings.json`, `keybindings.json`, `statusline-command.sh`,
`CLAUDE.md`, all `skills/*` directories, and all `themes/*` files.
