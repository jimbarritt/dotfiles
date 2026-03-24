# Installing the Claude Code Status Line

A custom status line for Claude Code that shows useful context at a glance, styled to match the green-tinted zsh theme.

## What it shows

```
dotfiles [main] - Claude Opus 4.6 - ctx:87%
```

- **Directory** (green) — basename of the current working directory
- **Git branch** (cyan) — current branch or short SHA if detached
- **Model** (dim) — which Claude model is active
- **Context remaining** (dim) — percentage of context window left

## Requirements

- **Claude Code v2.1.42 or later** — the `statusLine` setting was introduced around this version (stable, not experimental)
- `jq` for parsing the JSON payload Claude Code sends to the status line script.

```sh
# macOS
brew install jq

# Debian/Ubuntu
sudo apt-get install jq
```

## Installation

```sh
# Copy the script into place
cp home/claude/statusline-command.sh ~/.claude/statusline-command.sh
```

Then add the `statusLine` setting to `~/.claude/settings.json`:

```json
{
  "statusLine": {
    "type": "command",
    "command": "sh ~/.claude/statusline-command.sh"
  }
}
```

If the file already exists, merge the `statusLine` key into it. The change takes effect next time you launch `claude`.

## How it works

Claude Code pipes a JSON object to the script's stdin on each status line refresh. The script extracts:

- `workspace.current_dir` — the working directory
- `model.display_name` — the active model name
- `context_window.remaining_percentage` — how much context is left

It then runs `git symbolic-ref` (with a 2-second timeout to avoid hangs on slow filesystems) to get the current branch, and formats everything with ANSI colour codes.

## Customising

Edit `home/claude/statusline-command.sh` (or `~/.claude/statusline-command.sh` directly). The colour variables at the top are standard ANSI escape codes — change them to match your terminal theme.

To see what fields are available in the JSON input, you can temporarily add `echo "$input" > /tmp/claude-statusline-debug.json` near the top of the script and inspect the output.
