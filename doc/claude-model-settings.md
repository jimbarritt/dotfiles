# Claude Code: how the `model` setting persists

## Short version

Changing your model in Claude Code **writes the `model` field into your user
settings** (`~/.claude/settings.json`). Removing the key does **not** stop this —
it gets re-added the next time you switch model.

## The mechanism

As of Claude Code **v2.1.153**, `/model` saves your choice as the default for new
sessions by writing the `model` field in your **user** settings.

In the picker:

- **`Enter`** — switch model **and save as your default** (writes to settings.json)
- **`s`** — switch model **for this session only** (no write)

Typing `/model <name>` directly behaves like `Enter`.

Because `~/.claude/settings.json` is your user settings file, this is exactly
where the write lands — which is why a dotfiles clone tracking that file shows a
diff every time you change model.

## When it takes effect

The `model` key is read **once at session start**. To switch mid-session, use
`/model`. (Contrast with `permissions`/`hooks`, which hot-reload.)

## Ways to avoid the churn

- In the picker, press **`s`** (session-only) instead of `Enter`.
- Launch with **`claude --model <alias|name>`** — session-only, no write.
- Set **`ANTHROPIC_MODEL=<alias|name>`** — session-only, no write.
- Set the model in **project** (`.claude/settings.json`) or **managed** settings —
  these take precedence and reapply on launch, though a user `/model` + `Enter`
  would still write to the user file.

## Precedence (highest first)

1. Managed settings
2. Command-line args (`--model`)
3. Local settings (`.claude/settings.local.json`)
4. Project settings (`.claude/settings.json`)
5. User settings (`~/.claude/settings.json`)

## Source

- <https://code.claude.com/docs/en/model-config>
- <https://code.claude.com/docs/en/settings>
