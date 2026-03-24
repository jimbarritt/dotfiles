# Installing Claude Code "YOLO Mode"

YOLO mode lets Claude Code run autonomously (bypassing per-tool permission prompts) while a safety-net hook blocks dangerous commands. This gives you fast, hands-off operation without risking destructive side-effects.

## What gets installed

All files live under `home/claude/` in this repo and are symlinked to `~/.claude/` by `just link`.

| File | Purpose |
|------|---------|
| `settings.json` | Main Claude Code settings — permissions, hooks, model, status line |
| `hooks/pre-tool-use-filter.sh` | PreToolUse hook that blocks dangerous Bash commands |
| `statusline-command.sh` | Custom status line showing cwd, git branch, model, and context % |
| `commands/proofread.md` | Custom `/proofread` slash command |

## Quick start (new machine)

The claude config is **not** auto-linked by `just link` — it needs manual setup to avoid overwriting existing Claude Code state.

```sh
# 1. Clone the dotfiles repo
git clone git@github.com:jimbarritt/dotfiles.git ~/Code/github/jimbarritt/dotfiles
cd ~/Code/github/jimbarritt/dotfiles

# 2. Create the hooks directory
mkdir -p ~/.claude/hooks

# 3. Copy the files into place
cp home/claude/settings.json        ~/.claude/settings.json
cp home/claude/hooks/pre-tool-use-filter.sh ~/.claude/hooks/
cp home/claude/statusline-command.sh ~/.claude/
chmod +x ~/.claude/hooks/pre-tool-use-filter.sh

# 4. Optionally copy the custom slash command
mkdir -p ~/.claude/commands
cp home/claude/commands/proofread.md ~/.claude/commands/
```

The next time you launch `claude`, it will pick up the settings.

## What the settings do

### `skipDangerousModePermissionPrompt: true`

Suppresses the confirmation dialog when launching Claude Code with `--dangerously-skip-permissions` (aka YOLO mode). Without this you get a scary prompt every time.

### Deny list (built-in permissions layer)

A first line of defence in the settings file itself:

- `Bash(rm *)` / `Bash(rm -rf *)` / `Bash(sudo rm *)` — no file deletion
- `Bash(git push --force*)` — no force-pushing

### PreToolUse hook (`hooks/pre-tool-use-filter.sh`)

The main safety net. Runs before every Bash tool call and blocks:

- **File destruction** — `rm`, `rmdir`, `shred`, `truncate`
- **Git push** (all variants) — you push manually
- **Git history rewriting** — `reset --hard`, `clean -f`, `rebase`, `branch -D`, `tag -d`
- **Privilege escalation** — `sudo`, `su`
- **Package publishing** — `npm publish`, `cargo publish`, `wrangler deploy`
- **Pipe-to-shell** — `curl ... | bash` and friends
- **Process killing** — `kill -9`, `pkill`, `killall`
- **Disk operations** — `dd`, `mkfs`, `diskutil erase`
- **Cloud destructive ops** — `aws s3 rm`, `aws ec2 terminate-instances`
- **Crontab wipe** — `crontab -r`

If a command matches, the hook returns a JSON deny response and Claude sees a rejection message explaining why.

### Other settings

| Setting | Value | Notes |
|---------|-------|-------|
| `model` | `opus` | Default model |
| `effortLevel` | `medium` | Reasoning effort |
| `voiceEnabled` | `true` | Voice input enabled |
| `CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS` | `1` | Enables agent teams feature |
| `statusLine` | custom script | Shows cwd, git branch, model, context % |

## Prerequisites

The hook script requires `jq` to parse the JSON payload from Claude Code:

```sh
# macOS
brew install jq

# Debian/Ubuntu
sudo apt-get install jq
```

## Customising

To add more blocked commands, edit `home/claude/hooks/pre-tool-use-filter.sh` and add a new `grep` + `deny` block following the existing pattern. Run `just link` to update the symlink if needed (though since it's a symlink, edits in the repo take effect immediately).

To change settings like model or effort level, edit `home/claude/settings.json`.

## Running in YOLO mode

```sh
# Launch with autonomous permissions
claude --dangerously-skip-permissions

# Or set an alias
alias yolo='claude --dangerously-skip-permissions'
```

The hook ensures that even in this mode, destructive commands are still blocked.
