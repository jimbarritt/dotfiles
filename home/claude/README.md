# Claude Config

This directory (`home/claude/`) is the source of truth for Claude Code config. Files here are
symlinked into `~/.claude/` by `./do.sh link-claude` (also part of `./do.sh link`), which covers:
`settings.json`, `keybindings.json`, `statusline-command.sh`, `CLAUDE.md`, all `skills/*`
directories, all `hooks/*.sh` files, and all `themes/*` files.

The global `CLAUDE.md` is also linked as GitHub Copilot CLI's global instructions file by
`./do.sh link-copilot` — see `doc/claude-copilot-compatibility.md`.

---

## Manual symlink reference

Equivalent `ln` commands, if you need to create a link by hand.

### Top-level files

```sh
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/settings.json ~/.claude/settings.json
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/keybindings.json ~/.claude/keybindings.json
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/statusline-command.sh ~/.claude/statusline-command.sh
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/CLAUDE.md ~/.claude/CLAUDE.md
```

### Copilot CLI global instructions and skills

Same file as the global `CLAUDE.md`, linked where Copilot CLI looks for it, plus each
skill directory linked into `~/.copilot/skills/` (Copilot's personal-skills location):

```sh
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/CLAUDE.md ~/.copilot/copilot-instructions.md
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/skills/<skill-name> ~/.copilot/skills/<skill-name>   # per skill
```

### Skills

Each skill is a directory. Symlink the whole directory, not individual files inside it.
Every `SKILL.md` must have YAML frontmatter (`name` matching the directory, plus a
`description`) — required by Copilot, accepted by Claude Code.

```sh
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/skills/init-plan ~/.claude/skills/init-plan
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/skills/load-plan ~/.claude/skills/load-plan
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/skills/update-plan ~/.claude/skills/update-plan
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/skills/proofread ~/.claude/skills/proofread
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/skills/publish-crate ~/.claude/skills/publish-crate
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/skills/save-session ~/.claude/skills/save-session
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/skills/copilot-repo-init ~/.claude/skills/copilot-repo-init
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/skills/restore-session ~/.claude/skills/restore-session
```

### Hooks

```sh
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/hooks/canary-inject.sh ~/.claude/hooks/canary-inject.sh
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/hooks/pre-tool-use-filter.sh ~/.claude/hooks/pre-tool-use-filter.sh
```

### Themes

```sh
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/themes/green-tinted-dark.json ~/.claude/themes/green-tinted-dark.json
ln -shf ~/Code/github/jimbarritt/dotfiles/home/claude/themes/green-tinted-light.json ~/.claude/themes/green-tinted-light.json
```
