# Claude Code permissions

How to move from yolo mode to an explicit allow/deny model.

## Status

Applied 2026-05-25. `skipDangerousModePermissionPrompt` set to `false` in `~/.claude/settings.json`.

Allow and deny lists added 2026-05-25 as documented below.

## How the permission model works

Claude Code uses a three-tier model: **deny > allow > prompt**.

- **deny** — always blocked, no override
- **allow** — runs silently, no prompt
- **prompt** — asks once; "don't ask again" auto-adds to settings

`skipDangerousModePermissionPrompt: true` (yolo mode) bypasses the prompt tier entirely — everything not denied runs silently. Setting it to `false` restores the prompt tier.

Hooks (`PreToolUse` etc.) continue to run regardless of yolo mode.

## Pattern syntax

Patterns are `Tool(argument-pattern)`. For Bash, use `:*` as the wildcard suffix:

```
Bash(git status:*)     # matches "git status" with any args
Bash(git status)       # matches only the literal "git status" (no args)
Bash(git:*)            # matches ANY git subcommand — too broad for push/reset
```

- Compound commands (`a && b`, `a | b`) are matched as a whole string. `Bash(ls:*)` will not auto-allow `ls && rm foo` — the prompt fires on the compound.
- Deny always wins over allow, regardless of order.
- Settings cascade: user `~/.claude/settings.json` → project `.claude/settings.json` → project `.claude/settings.local.json`. Project-local additions merge with user-level.

## RTK compatibility

RTK rewrites Bash commands via a `PreToolUse` hook (e.g. `ls -la` → `rtk ls -la`). In practice the permission check is applied to the **rewritten** command, not the original — observed when a `git log` invocation prompted with pattern `rtk git *` despite `Bash(git log:*)` being allowed.

This means every allow entry needs an `rtk `-prefixed twin (e.g. `Bash(ls:*)` AND `Bash(rtk ls:*)`). A broad `Bash(rtk:*)` would work but is too coarse — it'd silently allow `rtk rm`, `rtk git push --force`, etc. Deny rules likely also match the rewritten form, but mirroring allow entries is the safer construction.

## Current config (`~/.claude/settings.json`)

### Allow list

```json
"allow": [
  "Bash(ls:*)",
  "Bash(rtk ls:*)",
  "Bash(find:*)",
  "Bash(rtk find:*)",
  "Bash(rg:*)",
  "Bash(rtk rg:*)",
  "Bash(grep:*)",
  "Bash(rtk grep:*)",
  "Bash(cat:*)",
  "Bash(rtk cat:*)",
  "Bash(head:*)",
  "Bash(rtk head:*)",
  "Bash(tail:*)",
  "Bash(rtk tail:*)",
  "Bash(wc:*)",
  "Bash(rtk wc:*)",
  "Bash(file:*)",
  "Bash(rtk file:*)",
  "Bash(stat:*)",
  "Bash(rtk stat:*)",
  "Bash(tree:*)",
  "Bash(rtk tree:*)",
  "Bash(which:*)",
  "Bash(rtk which:*)",
  "Bash(type:*)",
  "Bash(rtk type:*)",
  "Bash(readlink:*)",
  "Bash(rtk readlink:*)",
  "Bash(realpath:*)",
  "Bash(rtk realpath:*)",
  "Bash(pwd)",
  "Bash(rtk pwd)",
  "Bash(git status:*)",
  "Bash(rtk git status:*)",
  "Bash(git status)",
  "Bash(rtk git status)",
  "Bash(git diff:*)",
  "Bash(rtk git diff:*)",
  "Bash(git log:*)",
  "Bash(rtk git log:*)",
  "Bash(git show:*)",
  "Bash(rtk git show:*)",
  "Bash(git branch:*)",
  "Bash(rtk git branch:*)",
  "Bash(git remote:*)",
  "Bash(rtk git remote:*)",
  "Bash(git config --get:*)",
  "Bash(rtk git config --get:*)",
  "Bash(git config --list:*)",
  "Bash(rtk git config --list:*)",
  "Bash(git config --list)",
  "Bash(rtk git config --list)",
  "Bash(git ls-files:*)",
  "Bash(rtk git ls-files:*)",
  "Bash(git rev-parse:*)",
  "Bash(rtk git rev-parse:*)",
  "Bash(git stash list:*)",
  "Bash(rtk git stash list:*)",
  "Bash(git blame:*)",
  "Bash(rtk git blame:*)",
  "Bash(git fetch:*)",
  "Bash(rtk git fetch:*)",
  "Bash(just:*)",
  "Bash(rtk just:*)",
  "Bash(make:*)",
  "Bash(rtk make:*)",
  "Bash(brew list:*)",
  "Bash(rtk brew list:*)",
  "Bash(brew info:*)",
  "Bash(rtk brew info:*)",
  "Bash(brew search:*)",
  "Bash(rtk brew search:*)",
  "Bash(brew doctor)",
  "Bash(rtk brew doctor)",
  "Bash(brew --prefix:*)",
  "Bash(rtk brew --prefix:*)",
  "Bash(brew --prefix)",
  "Bash(rtk brew --prefix)",
  "Bash(brew config)",
  "Bash(rtk brew config)",
  "Bash(gh pr view:*)",
  "Bash(rtk gh pr view:*)",
  "Bash(gh pr list:*)",
  "Bash(rtk gh pr list:*)",
  "Bash(gh pr diff:*)",
  "Bash(rtk gh pr diff:*)",
  "Bash(gh pr checks:*)",
  "Bash(rtk gh pr checks:*)",
  "Bash(gh issue view:*)",
  "Bash(rtk gh issue view:*)",
  "Bash(gh issue list:*)",
  "Bash(rtk gh issue list:*)",
  "Bash(gh repo view:*)",
  "Bash(rtk gh repo view:*)",
  "Bash(gh auth status)",
  "Bash(rtk gh auth status)",
  "Bash(gh run list:*)",
  "Bash(rtk gh run list:*)",
  "Bash(gh run view:*)",
  "Bash(rtk gh run view:*)",
  "Bash(open -a Marq:*)",
  "Bash(rtk open -a Marq:*)",
  "Bash(defaults read:*)",
  "Bash(rtk defaults read:*)",
  "Bash(sw_vers:*)",
  "Bash(rtk sw_vers:*)",
  "Bash(uname:*)",
  "Bash(rtk uname:*)",
  "Bash(env)",
  "Bash(rtk env)",
  "Bash(printenv:*)",
  "Bash(rtk printenv:*)",
  "Bash(date:*)",
  "Bash(rtk date:*)",
  "Read(**)",
  "Edit(**)",
  "Write(**)",
  "WebSearch",
  "WebFetch(domain:*)"   // kept but ineffective — see WebFetch note below
]
```

**WebFetch note:** `WebFetch(domain:*)` is a confirmed bug ([#11972](https://github.com/anthropics/claude-code/issues/11972)) — the wildcard never matches, so WebFetch always falls through to the prompt tier. Workaround: a `PermissionRequest` hook that auto-allows all WebFetch calls (see hooks section below).

Git is deliberately narrow — `Bash(git:*)` would silently allow push, reset, commit. `just` and `make` are allowed because anything they do is project-defined in the repo.

`Read`/`Edit`/`Write` are globally permitted via `**`. Path-scoped entries (e.g. `Edit(/Users/jmdb/Code/github/jimbarritt/dotfiles/**)`) at user scope were observed not to match in practice, and per-project scoping every repo is too much friction. Destructive shell operations are still gated by the Bash deny list (`rm`, `sudo`, etc.), so file edits are the only ungated filesystem op — which matches the prior yolo-mode trust level for editing.

### Deny list

```json
"deny": [
  "Bash(rm:*)",
  "Bash(sudo:*)",
  "Bash(git push --force:*)",
  "Bash(git push -f:*)",
  "Bash(git push --force-with-lease:*)",
  "Bash(git add:*)",
  "Bash(./do.sh:*)",
  "Bash(do.sh:*)",
  "Bash(./bin/:*)",
  "Bash(bin/:*)"
]
```

Notes:
- `Bash(rm:*)` covers `rm -rf` — no need for a separate entry
- `Bash(sudo:*)` blocks all sudo, not just `sudo rm`
- Three force-push entries cover `--force`, `-f`, and `--force-with-lease`
- `do.sh` and `bin/` entries enforce the CLAUDE.md rule: never run these autonomously
- Optional additions if needed: `Bash(git reset --hard:*)`, `Bash(git clean -f:*)`, `Bash(git checkout -- :*)`

### Hooks

```json
"PreToolUse": [
  {
    "matcher": "WebFetch",
    "hooks": [
      {
        "type": "command",
        "command": "echo '{\"hookSpecificOutput\": {\"hookEventName\": \"PreToolUse\", \"permissionDecision\": \"allow\"}}'",
        "timeout": 5
      }
    ]
  }
]
```

A `PreToolUse` hook for `WebFetch` that returns `permissionDecision: allow` before the prompt fires. Necessary because `WebFetch(domain:*)` in the allow list is bugged and non-functional.

**Why not `PermissionRequest`?** `permissionDecision` is only supported in `PreToolUse` hooks — `PermissionRequest` hooks do not honour it and the prompt still fires.

**Reload required:** Hook changes mid-session are not picked up automatically. Open `/hooks` once to reload config, or restart the session.

## What stays behind a prompt

Not blocked, not allowed — requires one tap each time:

- `git commit`, `git push`, `git checkout`, `git switch`, `git merge`, `git rebase`, `git reset`, `git pull`, `git stash push`, `git tag`
- `brew install`, `brew upgrade`, `brew uninstall`, `brew cleanup`
- `mv`, `cp`, `chmod`, `chown`, `ln -s`
- `curl`, `wget`
- `defaults write`

## Growing the allow list over time

When prompted for a command you trust, choose "Yes, and don't ask again" — Claude Code auto-adds it to `~/.claude/settings.json` (user scope) or `.claude/settings.local.json` (project scope). Periodically review auto-added entries and promote useful ones here.

## See also

- `doc/installing-claude-yolo-mode.md` — original yolo mode setup
- `doc/claude-context-management.md` — context window costs and optimisations
