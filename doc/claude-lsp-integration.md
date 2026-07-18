# Claude Code LSP integration

Claude Code can talk to Language Server Protocol servers directly (go-to-definition, find-references, real-time diagnostics after edits) instead of relying purely on grep. This is configured via plugins — either the official marketplace ones, or a custom `.lsp.json`.

## How it works

A plugin declares LSP servers in `.lsp.json` at its root (or inline in `plugin.json` under `lspServers`):

```json
{
  "<language>": {
    "command": "<binary-on-PATH>",
    "args": ["..."],
    "extensionToLanguage": { ".ext": "<language>" },
    "shutdownTimeout": 5000,
    "restartOnCrash": true,
    "maxRestarts": 3
  }
}
```

Key fields:

- `command` / `extensionToLanguage` — required. The binary must already be on `PATH`; the plugin only tells Claude how to talk to it, it doesn't install anything.
- `shutdownTimeout` — max ms to wait for graceful shutdown before Claude Code force-kills the process. **Leave this unset and Claude Code will wait indefinitely**, which is how LSP processes end up hanging around.
- `restartOnCrash` / `maxRestarts` — whether/how many times to restart a crashed server.
- Both `shutdownTimeout` and `restartOnCrash` require **Claude Code v2.1.205 or later** — earlier versions silently skip a server entirely if either field is set, with the reason visible only in `claude --debug`.

If two enabled LSP servers claim the same file extension, the first one registered wins and the other never starts — `/plugin` shows a warning naming which plugin is active.

Personal (non-shared) plugins can live under `~/.claude/skills/<name>/` with a `.claude-plugin/plugin.json` — same auto-load mechanism as skills, no marketplace step needed. In this repo, `do.sh link-claude` symlinks every directory in `home/claude/skills/` into `~/.claude/skills/`, so any plugin added there is picked up automatically after linking.

## The hanging-process problem

Claude Code has a history of leaking LSP server processes — confirmed by [anthropics/claude-code#26752](https://github.com/anthropics/claude-code/issues/26752), where a `rust-analyzer` instance was spawned on every new chat/`/clear` and never cleaned up, consuming gigabytes of RAM per orphaned process. The `shutdownTimeout`/`restartOnCrash` fields exist specifically to address this, but only work from v2.1.205 onward.

This matters doubly if you also run the same language's LSP inside nvim (see `config/nvim/lua/plugins/mason.lua`): Claude Code's LSP instance is entirely separate from nvim's — they don't share a process — so an unbounded Claude-side server is extra memory pressure on top of whatever nvim already has running, not a replacement for it.

**Rule of thumb:** don't install an official marketplace LSP plugin without checking whether it lets you set `shutdownTimeout`. If it doesn't (the official ones are black boxes — no way to edit their `.lsp.json`), prefer a small custom plugin like the Rust one below where you control the timeout explicitly.

## Rust (rust-analyzer)

Custom plugin: `home/claude/skills/rust-analyzer-lsp/` — not the official `rust-analyzer-lsp@claude-plugins-official` marketplace plugin, so `shutdownTimeout: 5000` and `restartOnCrash` can be set explicitly rather than trusting the official plugin's (unconfigurable) defaults.

```json
// home/claude/skills/rust-analyzer-lsp/.lsp.json
{
  "rust": {
    "command": "rust-analyzer",
    "extensionToLanguage": { ".rs": "rust" },
    "shutdownTimeout": 5000,
    "restartOnCrash": true,
    "maxRestarts": 3
  }
}
```

No `brew install` needed — `rust-analyzer` is already on `PATH` at `~/.cargo/bin/rust-analyzer` (installed via rustup/cargo). This is a *different* binary from the one nvim uses (nvim's is Mason-managed, at `~/.local/share/nvim/mason/bin/rust-analyzer`) — the two run as independent processes against the same workspace, which is expected and not a conflict.

To activate: run `./do.sh link-claude` (or `./do.sh link`) to symlink the plugin into `~/.claude/skills/`, then start a new Claude Code session in a Rust project.

If it ever seems to be leaking processes despite the timeout, check `ps aux | grep rust-analyzer` for stragglers and `claude --debug` for why a server was skipped or failed to shut down.

## Other languages

Anthropic maintains official pre-built plugins for the common ones — install via `/plugin install <name>@claude-plugins-official`:

| Plugin | Language server | Binary install |
|---|---|---|
| `pyright-lsp` | Pyright (Python) | `pip install pyright` or `npm install -g pyright` |
| `typescript-lsp` | TypeScript Language Server | `npm install -g typescript-language-server typescript` |
| `rust-analyzer-lsp` | rust-analyzer | see [rust-analyzer install docs](https://rust-analyzer.github.io/manual.html#installation) |

These are fine to use as-is if you're not hitting the hanging-process issue — the custom-plugin approach above is only worth the extra setup when you specifically want to pin `shutdownTimeout`.

## nvim LSPs (separate from Claude Code's)

The LSP servers above are Claude Code's own — entirely separate processes from the ones nvim runs for editing. Most of nvim's are Mason-managed (`:Mason`, press `U` to update all, or `:MasonUpdate`), but Kotlin is the exception:

### Kotlin (kotlin-lsp)

Installed via Homebrew cask, not Mason — JetBrains' CDN blocks Mason downloads. To upgrade:

```bash
brew upgrade --cask kotlin-lsp
xattr -r -d com.apple.quarantine /opt/homebrew/Caskroom/kotlin-lsp
```

Then restart nvim — `config/nvim/lua/plugins/kotlin.lua` auto-detects the new cask version on startup, no config changes needed. The `xattr` step is required after every upgrade: macOS re-quarantines the newly-downloaded cask contents, which blocks the LSP's unsigned native libraries (`libfilewatcher_jni.dylib`) from loading.

Full setup/troubleshooting detail: `config/nvim/doc/kotlin-lsp-setup.md`.

## Sources

- [Claude Code plugins reference — LSP servers](https://code.claude.com/docs/en/plugins-reference)
- [Claude Code plugins guide — Add LSP servers to your plugin](https://code.claude.com/docs/en/plugins)
- [rust-analyzer spawned per session, unbounded memory — anthropics/claude-code#26752](https://github.com/anthropics/claude-code/issues/26752)
