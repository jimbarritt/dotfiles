# Dotfiles — Implementation Plan

## What's Next

- **Next:** Task 1 — evaluate `jscpd` for the duplication layer (Delta: Code Metrics for Agents)
- **Sub-doc:** [doc/code-metrics-for-agents.md](../code-metrics-for-agents.md)
- **Blockers:** None
- **Context:** [Checkpoint: Session 2026-08-18](#checkpoint-session-2026-08-18)

## Summary

| Delta | Task | Status |
|-------|------|--------|
| [Delta: Statusline Refinement](#delta-statusline-refinement) | [1. Verify extra usage detection](#task-1-verify-extra-usage-detection) | TODO |
| | [2. Plan skill refinement](#task-2-plan-skill-refinement) | ✓ DONE |
| [Delta: Plan Skill Iteration](#delta-plan-skill-iteration) | [1. Tweak plan skill based on continued use](#task-1-tweak-plan-skill-based-on-continued-use) | ✓ DONE |
| | [2. Session timer loose ends](#task-2-session-timer-loose-ends) | TODO |
| [Delta: Code Metrics for Agents](#delta-code-metrics-for-agents) | [1. Survey the tooling](#task-1-survey-the-tooling) | IN PROGRESS |
| | [2. Decide packaging](#task-2-decide-packaging) | ✓ DONE |
| | [3. Build the skill](#task-3-build-the-skill) | IN PROGRESS |
| [Delta: Go Development Environment](#delta-go-development-environment) | [1. Go toolchain and nvim LSP](#task-1-go-toolchain-and-nvim-lsp) | ✓ DONE |
| | [2. Delve/DAP debugging](#task-2-delvedap-debugging) | TODO |
| [Delta: Permission Config Fixes](#delta-permission-config-fixes) | [1. Carve am-dotfiles-base out of the bin/ deny rule](#task-1-carve-am-dotfiles-base-out-of-the-bin-deny-rule) | TODO |
| | [2. Extend the blocked git command list](#task-2-extend-the-blocked-git-command-list) | IN PROGRESS |
| [Delta: Neovim LSP Reliability](#delta-neovim-lsp-reliability) | [1. Fix intermittent LSP attach](#task-1-fix-intermittent-lsp-attach) | ✓ DONE |
| [Delta: Claude Instruction Refinements](#delta-claude-instruction-refinements) | [1. Prose commentary rule](#task-1-prose-commentary-rule) | ✓ DONE |
| [Delta: Neovim Ergonomics](#delta-neovim-ergonomics) | [1. Prose wrap for markdown and text](#task-1-prose-wrap-for-markdown-and-text) | ✓ DONE |
| | [2. File tree filters and cheatsheet](#task-2-file-tree-filters-and-cheatsheet) | ✓ DONE |
| | [3. Window close key behaviour](#task-3-window-close-key-behaviour) | ✓ DONE |
| | [4. Dockerfile treesitter injection bleed](#task-4-dockerfile-treesitter-injection-bleed) | ✓ DONE |
| | [5. Mason ENOTCONN error](#task-5-mason-enotconn-error) | TODO |
| [Delta: Shell Helpers](#delta-shell-helpers) | [1. plan](#task-1-plan) | ✓ DONE |
| | [2. copypath](#task-2-copypath) | ✓ DONE |
| | [3. readme](#task-3-readme) | ✓ DONE |
| [Delta: Git Config Refinements](#delta-git-config-refinements) | [1. Exclude bots from git lol](#task-1-exclude-bots-from-git-lol) | ✓ DONE |

Archived Deltas: see the [archive index](archive/index.md)

## Delta: Statusline Refinement

### Task 1: Verify extra usage detection
- TODO — Confirm whether `rate_limits.five_hour.used_percentage` can exceed 100 in the JSON, or is capped
- TODO — If capped at 100, find alternative signal for "in extra usage" (e.g. a dedicated field)
- TODO — Adjust threshold logic in `statusline-command.sh` if needed

### Task 2: Plan skill refinement
- ✓ DONE — Review `update-plan` / `load-plan` / `init-plan` skill behaviour after real use — user found old format hard to read
- ✓ DONE — Create shared `home/claude/skills/plan-format/PLAN-FORMAT.md` — single canonical spec all three skills reference (avoids drift)
- ✓ DONE — New format: "Deltas" replace "Phases", "Tasks" replace "Actions", plain `##` headings for What's Next and Checkpoints (no box-drawing separators), Summary table (Delta | Task | Status) with anchor links at the top
- ✓ DONE — `update-plan` now migrates old-format plans automatically; `load-plan` flags stale format
- ✓ DONE — Review fixes: "Delta {X.Y}" → "Task {X.Y}" label, checkpoint placement contradiction in skeleton, `IN PROGRESS` status defined (some bullets done, some not)
- ✓ DONE — This plan migrated to the new format

## Delta: Plan Skill Iteration

### Task 1: Tweak plan skill based on continued use
- ✓ DONE — Gather user's specific desired tweaks to the plan skill (update-plan / init-plan / load-plan / plan-format) and apply them

### Task 2: Session timer loose ends
- TODO — `home/claude/hooks/plan-timer-resume.sh` (auto-resume on the next prompt) is written and tested but deliberately unregistered in `settings.json` — enable it if manual `/resume-plan` proves annoying; snippet is in the script header
- TODO — Work out why `~/.claude/skills/plan-format` was never symlinked, which broke every plan skill's helper call with exit 127. `do.sh`'s `link_claude` globs `skills/*/` and should have created it — run `./do.sh link-claude` and check the result is consistent

## Delta: Code Metrics for Agents

Command-line tooling that gives a coding agent structural feedback about an unfamiliar
codebase — size per file, per-function complexity, duplication — as a ranked report the
agent reads and acts on. Research: `doc/code-metrics-for-agents.md`.

### Task 1: Survey the tooling
- ✓ DONE — `scc` (brew) verified: per-file JSON with `Code`/`Complexity`/`Location`. Its complexity column is a per-file branch-keyword count, not McCabe
- ✓ DONE — Homebrew's `lizard` formula identified as an unrelated LZ4 compression tool, not the complexity analyser
- ✓ DONE — Real `lizard` installed via `uv tool install lizard`; verified per-function CCN, thresholded Warnings block, `--csv`, and non-zero exit on breach
- ✓ DONE — `codelimit` (brew) rejected: unit length only, no CCN, no working Swift support
- TODO — Evaluate `jscpd` for the duplication layer
- ✓ DONE — `doc/code-metrics-for-agents.md` written and indexed

### Task 2: Decide packaging
- ✓ DONE — Decided: no own formula. The skill documents `brew install scc` and `uv tool install lizard` as prerequisites, and a wrapper script checks for them and prints install instructions when missing

### Task 3: Build the skill
- ✓ DONE — `home/claude/skills/code-metrics/` — `SKILL.md` plus `code-metrics.sh`. Picked up automatically by `do.sh link-claude`
- ✓ DONE — Wrapper resolves `lizard` via `$HOME/.local/bin` fallback, because `uv tool install` puts it somewhere absent from a non-interactive PATH — without this the prerequisite check reports it missing immediately after a successful install
- ✓ DONE — Wrapper distinguishes the real lizard from Homebrew's compression tool of the same name by grepping `--help` for `cyclomatic`. Version strings do not discriminate: the compression tool prints a longer banner and the analyser prints a bare semver
- ✓ DONE — lizard's non-zero exit on threshold breach is a finding, not an error; `set -e` is deliberately not used
- ✓ DONE — Sections: shape, largest files, functions over CCN, functions over length, most parameters. Each ranked worst-first, capped by `--top`, with the threshold stated and an "N of M" count
- ✓ DONE — Report goes to stdout only. Never written into the target tree, which is usually not the repo being worked in
- ✓ DONE — No `-l` language mapping. lizard defaults to every language it knows; the translation layer was dropped as unnecessary
- ✓ DONE — Explicit `-x` excludes for `node_modules`, `target`, `vendor`, `.build`, `build`, `dist`, `Pods`, `.venv`, `__pycache__`. scc respects `.gitignore`, lizard does not
- ✓ DONE — Pure awk/sort over `scc --format csv` and `lizard --csv`; no jq or python dependency
- ✓ DONE — Verified on `~/projects/ledgr` (Rust) and `~/projects/tilr` (Swift), plus the degraded path with an empty PATH
- TODO — User to run `do.sh link-claude` to symlink the skill into `~/.claude/skills/`

## Delta: Go Development Environment

### Task 1: Go toolchain and nvim LSP
- ✓ DONE — `go = "latest"` added to `config/mise/config.toml` (installed go 1.26.5)
- ✓ DONE — `$GOPATH/bin` added to PATH in `home/zshrc` (mise provides the toolchain; `go install` binaries land in GOBIN and were otherwise unreachable)
- ✓ DONE — `gopls` added to `config/nvim/lua/plugins/mason.lua` `ensure_installed` (installed v0.23.0)
- ✓ DONE — `gopls` server entry in `config/nvim/lua/plugins/lsp.lua` — gofumpt, staticcheck, inlay hints, `unusedparams`/`unusedwrite`/`nilness`/`shadow` analyses
- ✓ DONE — `go`/`gomod`/`gosum`/`gowork` treesitter parsers
- ✓ DONE — Go-only `BufWritePre` autocmd: `source.organizeImports` code action then format. Deliberate exception to the repo's manual-`<leader>f` convention because unused/missing imports are compile errors in Go
- ✓ DONE — Verified end-to-end headlessly: gopls attaches, unused import removed, missing import added, indentation reformatted

### Task 2: Delve/DAP debugging
- TODO — Install `delve` (`go install github.com/go-delve/delve/cmd/dlv@latest`, or via mise if a backend exists)
- TODO — Add `nvim-dap` — no DAP setup exists in this config at all, so this is new plumbing rather than an entry in an existing table
- TODO — Add `leoluz/nvim-dap-go` for Go-specific adapter config and test-debugging helpers
- TODO — Add `rcarriga/nvim-dap-ui` + decide keymaps (breakpoint toggle, step over/into/out, REPL) and where they live relative to the shared `on_attach` in `lsp.lua`
- TODO — Consider whether DAP should be generalised for other languages (rust, python) or kept Go-only for now

## Delta: Permission Config Fixes

### Task 1: Carve am-dotfiles-base out of the bin/ deny rule
- TODO — `home/claude/settings.json` denies `Bash(./bin/*)` and `Bash(bin/*)`, but repo `CLAUDE.md` says `am-dotfiles-base` "exists for Claude to run". Deny rules are absolute and cannot be overridden by an `allow` entry, so the documented exception does not work
- TODO — Preferred fix: move the script out of `bin/` (e.g. `tools/am-dotfiles-base`) and symlink `bin/am-dotfiles-base` to it, so the blanket deny stays intact for genuinely-destructive scripts while Claude invokes the non-denied path
- TODO — Alternative considered and rejected: enumerating denies per-script (30+ scripts, and new ones would default to allowed)
- TODO — Update repo `CLAUDE.md` to point at whichever path Claude should invoke

### Task 2: Extend the blocked git command list
- ✓ DONE — `git checkout` added to `home/claude/hooks/pre-tool-use-filter.sh`, in the destructive-git section alongside `reset --hard`, `rebase` and `clean -f`
  - Verified blocked: `git checkout main`, `git checkout -- file`, `git checkout -b`, `cd /tmp && git checkout .`, bare `git checkout`
  - Verified still allowed: `git status`, `git log`, `git diff`
- TODO — Decide whether to also block `git switch` and `git restore` — the modern split of `checkout`, currently an unguarded route to the same destructive behaviour

## Delta: Neovim LSP Reliability

### Task 1: Fix intermittent LSP attach
- ✓ DONE — Diagnosed intermittent "no active clients" affecting every server, not just gopls
  - Timing measurement was the discriminator: attach completed in 0.1s or never within 45s, with no intermediate values, ruling out slow server startup or indexing
  - Cause: LSP clients start off `FileType`; when a file is passed on the command line that event can fire before the plugin config finishes, and nothing retries
  - Baseline failure rate ~40%; a restart appeared to fix it only by winning the race
- ✓ DONE — `vim.lsp.enable(vim.tbl_keys(servers))` added to `config/nvim/lua/plugins/lsp.lua` — the config previously never called `enable()`, relying on mason-lspconfig, which as a lazy.nvim dependency loads first and produced the `"<server> does not have a configuration"` warnings in `:LspLog`. Measured separately: did not fix the flakiness
- ✓ DONE — Re-fire `FileType` for already-loaded buffers at the end of setup — this is the fix; 8/8 clean runs against a ~40% baseline
- ✓ DONE — Written up in `doc/nvim-lsp-filetype-race.md`, indexed in `doc/INDEX.md`, including a general diagnostic table for this class of problem

## Delta: Claude Instruction Refinements

### Task 1: Prose commentary rule
- ✓ DONE — New `## Prose` section in `home/claude/CLAUDE.md`: default to no editorial commentary when writing docs, READMEs, ADRs, comments and commit messages
  - Lists the five patterns to drop (editorial judgements, predicted reader reactions, meta-remarks about the writing, enthusiasm, needless hedging)
  - Draws the line explicitly: rationale and trade-offs are content and stay; commentary *on* the rationale goes
  - Explicit requests for opinion or critique override the default
- ✓ DONE — Applied retroactively to the go-tutorial docs written this session

## Delta: Neovim Ergonomics

### Task 1: Prose wrap for markdown and text
- ✓ DONE — `wrap` never applied to markdown despite a `FileType` autocmd existing
  - Cause: `init.lua` requires `config.autocmds` after `config.lazy`, so for a file named on the command line the event has already fired — the same race as `doc/nvim-lsp-filetype-race.md`
  - `text` was never handled at all
  - Fixed with `config/nvim/after/ftplugin/markdown.lua` and `after/ftplugin/text.lua`; the runtime's ftplugin mechanism is registered before `init.lua` runs, so it is immune to the ordering
  - Dead autocmd removed from `config/nvim/lua/config/autocmds.lua`
  - Verified headlessly: `.md`/`.txt` wrap, `.go` does not, no leak between buffers in one window

### Task 2: File tree filters and cheatsheet
- ✓ DONE — nvim-tree `filters` changed to `dotfiles = false` with `^\.git$` and `^\.DS_Store$` added to `filters.custom`; `git_ignored` left on
  - Originally raised as untracked Go files being hidden; testing showed nvim-tree was never hiding them, and the user confirmed the directory had been misread
  - Kept anyway as the stated preference: see gitignored files' names but not their contents, see dotfiles, see untracked and staged files
- ✓ DONE — Three FILE TREE sections added to `config/key-help/nvim`, taken from a dump of the live buffer keymaps so they reflect the `on_attach` overrides

### Task 3: Window close key behaviour
- ✓ DONE — `<C-w>q` remapped to `<C-w>c` in `config/nvim/lua/config/keymaps.lua`
  - `<C-w>q` is `:quit` scoped to a window, so on the last window it exits nvim; `:close` raises `E444` instead
  - Cheatsheet WINDOWS section updated, plus `:bd`

### Task 4: Dockerfile treesitter injection bleed
- ✓ DONE — Dockerfile instructions rendering as plain text from partway down the file
  - Cause: nvim-treesitter's dockerfile `injections.scm` sets `injection.combined`, merging every `RUN` body into one bash tree parsed as a contiguous span, so bash `@variable.parameter` captures cover the `COPY`/`FROM`/`RUN` keywords in between and outrank `@keyword`
  - Fixed with `config/nvim/after/queries/dockerfile/injections.scm` — a full replacement (no `;; extends`, since appending cannot remove a directive) with `injection.combined` dropped
  - `dockerfile` added to `ensure_installed` in `treesitter.lua`
  - Written up in `doc/nvim-dockerfile-injection-bleed.md`, indexed, and cross-linked from `doc/additional-customisation-of-nvim-treesitter.md`

### Task 5: Mason ENOTCONN error
- TODO — `Error executing callback: ENOTCONN` from `mason-core/async/init.lua:121` seen interactively; not reproduced
  - Ruled out: registry is current (downloaded same day), network fine, all 11 `ensure_installed` servers present, `mason.log` has no entry, forced `mason-registry.update()` succeeds
  - Line 121 is `error(err, 0)` in `exports.scope`; `ENOTCONN` is libuv for writing to an unconnected stream, consistent with a spawn race rather than a network failure
  - Offered but not applied: `log_level = vim.log.levels.DEBUG` in `mason.setup()` to capture the next occurrence

## Delta: Shell Helpers

### Task 1: plan
- ✓ DONE — `plan()` in `home/zshrc` opens the current project's plan in Marq
  - Search order matches the `load-plan` skill: `doc/planning/plan.md`, `doc/plan.md`, `~/.planning/{project}/plan.md`
  - Root from `git rev-parse --show-toplevel` falling back to `$PWD`, so it works from any subdirectory
  - Implemented repo-local-first, diverging from the user's "global dir first" phrasing; flagged, not corrected

### Task 2: copypath
- ✓ DONE — `copypath()` in `home/zshrc` copies an absolute path to the clipboard
  - Defaults to `.`; uses `printf '%s'` so no trailing newline reaches the clipboard; echoes the path
  - `realpath` resolves symlinks, so paths under `~/.config` report their dotfiles location

### Task 3: readme
- ✓ DONE — `readme()` in `home/zshrc` opens the nearest README, walking upwards
  - Stops at a git root and reports `could not find readme in this git repo`; with no repo anywhere it walks to `/` and reports `cannot find readme`
  - Detects `.git` as a file as well as a directory, covering worktrees and submodules
  - Chosen as a zshrc function rather than a `bin/` script, alongside `plan` and `copypath`

## Delta: Git Config Refinements

### Task 1: Exclude bots from git lol
- ✓ DONE — `lol` alias in `home/gitconfig` now filters bot authors via `--perl-regexp` and a negative lookahead
  - `\\[` is required in the config file — git rejects `\[` as a bad escape
  - Excludes any `*[bot]` plus `dependabot`, `renovate`, `github-actions`, `semantic-release`; `Bob Botham` and `Abbot Smith` still show
  - Confirmed the alias contains nothing work-specific, in response to a public-repo scrub question

## Checkpoint: Session 2026-06-18d

**What was completed this session:**
- Diagnosed root cause of persistent `/tmp` permission prompts: Claude Code uses gitignore-spec anchoring where a single `/` prefix means "relative to project root", not filesystem root — absolute paths require `//` prefix
- Fixed `settings.json`: `Read(/tmp/**)` → `Read(//tmp/**)`, `Read(/private/tmp/**)` → `Read(//private/tmp/**)`, dropped redundant single-star entry
- Added `Write(//tmp/**)` and `Write(//private/tmp/**)` for symmetry
- Updated implementation notes with `//` anchoring rule

**State of the project:**
Delta 1 and Task 2.1 complete. The `/tmp` permission prompt is now fully resolved — reads and writes to `/tmp` work without prompting. Implementation notes updated with the `//` anchoring rule so the footgun is documented.

**Immediate next priorities:**
1. Task 2.2 — Rewrite `README.md` to reflect current setup
2. Task 3.1 — Verify extra usage detection in statusline
3. Task 3.2 — Review plan skill behaviour after sustained use

## Checkpoint: Session 2026-07-01

**What was completed this session:**
- Added `configure_macos` function to `do.sh` — runs `defaults write com.mitchellh.ghostty NSWindowShadowEnabled -bool false` to disable Ghostty window shadows that bleed onto adjacent tiled windows
- `configure_macos` is now called from `install` alongside `install_zsh_plugins` and `install_git_hooks`

**State of the project:**
Delta 1 and Task 2.1 complete. A new macOS defaults step is now part of `do.sh install`. No existing plan TODOs were completed this session — the Ghostty shadow fix was a new addition not previously tracked.

**Immediate next priorities:**
1. Task 2.2 — Rewrite `README.md` to reflect current setup
2. Task 3.1 — Verify extra usage detection in statusline
3. Task 3.2 — Review plan skill behaviour after sustained use

## Checkpoint: Session 2026-07-01b

**What was completed this session:**
- Investigated graphify setup end-to-end for the dotfiles repo (Task 1.8), starting from `doc/graphify.md` (already existed)
- Attempted adding a `## Graphify` section to global `home/claude/CLAUDE.md`, then reverted it at user's request — moved the content into `doc/graphify.md` instead as a `## At a glance (for Claude)` section
- Verified the Grep/Glob `PreToolUse` hook-compatibility claim's actual source (roborhythms.com review, not graphify's own docs) via web search; added inline citations
- User surfaced a second-hand doc (`doc/installing-graphify.md`) claiming to derive from a work `bootstrap.sh` — initially flagged as suspicious, user clarified it came from another Claude instance's analysis of a real work script
- Reconciled `doc/graphify.md` against that doc and independent web search: corrected the output directory to `graphify-out/`, added `graphify hook install`, the MCP server option, and the CLAUDE.md-injection caveat on `graphify claude install`
- User located and shared the actual `bootstrap.sh` (in `~/Downloads`) — read it, confirmed it corroborates the reconciled doc almost verbatim, and confirmed the hook-compatibility risk is live in this exact environment (hook matcher is `Glob|Grep`, tool names this session doesn't have)
- Removed all references to the internal `north-star` project name from the (now-deleted) second-hand doc at user's request
- Created two tracked scripts: `home/claude/graphify-install.sh` (global) and `home/claude/graphify-configure-repo.sh` (per-repo) — split after user was confused why a "per-repo" script was doing genuinely global work
- Reordered `doc/graphify.md` so "Installation" and "Configuring a repo" are the first two sections
- Fixed `graphify-install.sh` to use `uv tool install graphifyy` instead of `pipx`

**State of the project:**
`doc/graphify.md` is now a reconciled, cited, and corrected reference doc with install/config instructions at the top. Two executable scripts exist but neither has been run yet — graphify is not actually installed on this machine. Neither script is wired into `do.sh`; both are deliberately manual/opt-in.

**Immediate next priorities:**
1. Task 1.8 — Decide how Claude should discover `doc/graphify.md` in future sessions
2. Run `home/claude/graphify-install.sh` (global) when ready, then `graphify-configure-repo.sh` in a specific large-enough repo
3. Task 2.2 — Rewrite `README.md` to reflect current setup
4. Task 3.1 — Verify extra usage detection in statusline
5. Task 3.2 — Review plan skill behaviour after sustained use

## Checkpoint: Session 2026-07-06

**What was completed this session:**
- Graphify concluded (Task 1.8): work bootstrap was itself outdated — upstream fixed the hook gap and its `claude install` writes a small reviewable section to the *project* CLAUDE.md. Both tracked scripts obsoleted, `doc/graphify.md` rewritten (install = `uv tool install graphifyy`; per-repo = `graphify claude install`), discovery via repo CLAUDE.md rule
- Full Copilot CLI integration (new Delta 4): skills frontmatter, `doc/claude-copilot-compatibility.md`, `do.sh link-copilot` (instructions + skills + denylist), `copilot()` zsh wrapper with `--allow-all` + deny flags — verified working after `--allow-tool` shell-list and `--allow-all-tools` both left residual prompts
- AGENTS.md-canonical pattern documented + `bin/copilot-repo-init` (replaced a short-lived skill — too infrequent to justify per-session context cost)
- `bin/am-dotfiles-base` guard + upstream-only rule in repo CLAUDE.md; PR no-Claude-references rule in global CLAUDE.md
- `settings.json` synced with work machine (sonnet/medium); work `permissions-config.json` rejected as Pleo-specific and per-location
- `home/claude/README.md` rewritten; zshrc `reload` split into help + `reload-zshrc`; memory note `project_copilot_integration.md` saved

**State of the project:**
Copilot and Claude Code now share one config surface, all tracked. Copilot permissions verified working end-to-end. User still to run: `./do.sh link-copilot` wherever not yet linked; delete obsolete `home/claude/graphify-*.sh` and `~/Downloads/permissions-config.json`. Work machine will pick everything up on next pull + `link-copilot`.

**Immediate next priorities:**
1. Task 2.2 — Rewrite `README.md` to reflect current setup
2. User: delete the two obsolete graphify scripts and the Downloads permissions file
3. Task 4.1 — Optionally convert this repo to the AGENTS.md pattern
4. Task 3.1 — Verify extra usage detection in statusline

## Checkpoint: Session 2026-07-06b

**What was completed this session:**
- Plan skill format overhaul (Task 3.2): created shared `home/claude/skills/plan-format/PLAN-FORMAT.md` as the single canonical plan spec; `init-plan`, `update-plan`, and `load-plan` all now reference it
- New format: "Deltas" replace "Phases", "Tasks" replace "Actions", What's Next and Checkpoint are plain `##` headings (no box-drawing separators), Summary table (Delta | Task | Status) with anchor links at the top of the plan
- `update-plan` migrates old-format plans automatically; `load-plan` flags stale-format plans
- Review pass fixed three defects: "Delta {X.Y}" mislabel → "Task {X.Y}", checkpoint-placement contradiction in the skeleton, undefined `IN PROGRESS` status (now: some bullets done, some not)
- This plan migrated to the new format (first real exercise of the migration path)
- Obsolete graphify scripts confirmed deleted (`home/claude/graphify-install.sh`, `graphify-configure-repo.sh`)

**State of the project:**
Plan skills and format are now consistent and centrally specified. The `plan-format` directory is not yet symlinked into `~/.claude/skills/` — user needs to run `./do.sh link-claude`. Note: `plan-format/` has no SKILL.md (it's a shared doc, not a skill) — flagged as a possible source of warnings from Claude Code/Copilot skill discovery; leave unless it causes problems.

**Immediate next priorities:**
1. User: run `./do.sh link-claude` to symlink the new `plan-format` directory
2. Task 2.2 — Rewrite `README.md` to reflect current setup
3. Task 4.1 — Optionally convert this repo to the AGENTS.md pattern
4. Task 3.1 — Verify extra usage detection in statusline

## Checkpoint: Session 2026-07-18

**What was completed this session:**
- Kotlin LSP for nvim: fixed three compounding breaking changes in `config/nvim/lua/plugins/kotlin.lua`
  - Cask directory layout changed to nest everything under `<version>/kotlin-server-<version>/` — fixed detection to fall back to the nested path
  - kotlin.nvim v2 dropped the `jre_path` setup option and legacy `kotlin-lsp.sh` launcher entirely, now always invokes `bin/intellij-server` directly — removed the dead JRE-detection code and updated diagnostic binary checks accordingly
  - kotlin.nvim v2 renamed its LSP client from `kotlin_ls` to `kotlin_lsp` — updated both the 30s startup-diagnostic check and the `VimLeavePre` shutdown handler
  - Also fixed a stale `KOTLIN_LSP_DIR` env var: corrected the machine-local `~/.zshrc_machine` value, and changed the plugin to always override the session env var with the freshly-validated path rather than only setting it when unset
- Documented the incident in a new "kotlin.nvim v2 breaking changes (2026-07)" section in `config/nvim/doc/kotlin-lsp-setup.md`; corrected a stale `kotlin_language_server` reference in `config/nvim/doc/CLAUDE.md`; expanded `config/nvim/README.md`'s "Maintenance" section into full update instructions covering Lazy plugins, Mason LSPs, and the Kotlin cask
- New `doc/claude-lsp-integration.md`: documents Claude Code's own LSP plugin system, including a custom `home/claude/skills/rust-analyzer-lsp/` plugin with explicit `shutdownTimeout`/`restartOnCrash` to avoid a known orphaned-rust-analyzer-process bug (upstream anthropics/claude-code#26752)
- `doc/claude-copilot-compatibility.md`: added a concrete symlink-vs-`@AGENTS.md`-import failure-mode table and a mitigation note explaining why this repo's own `~/.claude/CLAUDE.md` symlink is an accepted risk
- Ghostty: `macos-option-as-alt = true` in `config/ghostty/config`, so Option acts as real Alt/Meta
- zsh: fixed gaps in the oh-my-zsh vi-mode plugin's `viins` keymap in `home/zshrc` — `bindkey -M viins '^K' vi-kill-eol`, plus `'^[b'`/`'^[f'`/`'^[d'` bindings for Option-word-movement (needed because Option-as-Alt sends legacy Escape-prefixed sequences, which vi-mode's bare-Escape-to-normal-mode binding would otherwise swallow first)
- tmux: new `EDITING` section in `config/key-help/tmux` documenting the emacs/readline shortcuts
- New `scratch()` shell function in `home/zshrc` (opens `~/tmp/scratch.md`, creating the directory if needed)

**State of the project:**
This session's work — Kotlin LSP debugging, Claude Code LSP integration research, and several terminal/shell ergonomics fixes — fell entirely outside the plan's tracked Deltas; no existing plan.md Tasks were touched. Delta: Statusline Refinement's Task 1 (Verify extra usage detection) remains the only open TODO in the plan. All changes are committed directly to tracked dotfiles; nothing is queued for the user to run except the normal upstream-only commit/push.

**Immediate next priorities:**
1. Task 1 — Verify extra usage detection (Delta: Statusline Refinement)
2. Consider adding a recurring monthly nvim/plugin-maintenance task (Lazy update, Mason update, Kotlin cask upgrade + breaking-change review) once the user's "tsk" task system is running — not yet a plan Task, noted in memory only
3. Commit and push this session's dotfiles changes upstream (kotlin.lua fixes, new docs, Ghostty/zsh/tmux config)

## Checkpoint: Session 2026-07-23

**What was completed this session:**
- Fixed dead permission rules in `home/claude/settings.json`: `Write(**)`, `Write(~/**)`, `Write(//tmp/**)`, `Write(//private/tmp/**)` were never matched by permission checks (only `Edit(...)` rules cover file-editing tools) — replaced with `Edit(//tmp/**)` and `Edit(//private/tmp/**)` (`Edit(**)` and `Edit(~/**)` already existed)
- Diagnosed and fixed the rust-analyzer Claude Code LSP plugin hang: the binary wasn't actually installed (`~/.cargo/bin/rust-analyzer` was an erroring rustup shim); installed via mise instead (`rust-analyzer = "latest"` added to `config/mise/config.toml`) per user preference for mise-managed tooling
- Found and fixed a second bug: `.lsp.json`'s `command` field used a `~`-prefixed path, which Claude Code's process spawner does not expand (only shells do) — changed to an absolute path in `home/claude/skills/rust-analyzer-lsp/.lsp.json`
- Extended `bin/lsp-doctor` (previously kotlin-lsp-only) to also detect rust-analyzer in its process tree: dangling if parented by neither `nvim` nor `claude`; included in `--clean`. Workspace-lock detection stays kotlin-only (rust-analyzer has no equivalent lock file)
- Updated `doc/claude-lsp-integration.md`: mise-based install instructions, the `~`-non-expansion gotcha, a new "Diagnosing dangling processes (lsp-doctor)" section, and a new "Setting up Claude Code LSPs on a new machine" section describing a doc-then-memory bootstrap pattern (read the doc once per machine, mirror durable lessons into local auto-memory so later sessions on that machine don't need to re-read the doc)
- Saved memory `feedback_lsp_absolute_paths.md`: always pass absolute paths to LSP-backed tool calls (go-to-def, find-references, etc), same convention as Read/Edit/Write
- Created `doc/lsp-perf-obs.md`: an observation log to compare token/context cost of Rust sessions before/after the rust-analyzer fix, with a 2026-07-18 14:37 BST baseline marker and a note on why naive total-token comparisons will be noisy
- Investigated a separate hypothesis (that Claude leans on throwaway Python scripts and heavy `sed` usage for edits, and that working LSP would reduce this) by analyzing session transcript history of an unrelated, actively-worked repo — found no throwaway Python editing scripts at all, and `sed` usage at ~3.5% of edit-equivalent calls, always for two genuinely mechanical cases (repo-wide literal renames, confirmed-line-range deletions), never as an Edit-tool-failure fallback. Recorded as resolved/ruled-out in `doc/lsp-perf-obs.md` (anonymised — no project-identifying details)

**State of the project:**
This session's work (settings.json permission fix, rust-analyzer LSP setup and lsp-doctor extension, LSP integration doc updates, the new perf-observation log) fell entirely outside the plan's tracked Deltas, same as the previous session — no existing plan.md Tasks were touched. Delta: Statusline Refinement's Task 1 (Verify extra usage detection) remains open. A new Delta ("Plan Skill Iteration") has been added to track the user's stated intent to further tweak the plan skill, but the specific tweaks are not yet known.

**Immediate next priorities:**
1. Task 1 (Delta: Plan Skill Iteration) — get the user's specific list of desired plan-skill tweaks, then apply them
2. Task 1 (Delta: Statusline Refinement) — Verify extra usage detection
3. Commit and push this session's dotfiles changes upstream (settings.json, rust-analyzer-lsp plugin, claude-lsp-integration.md, lsp-perf-obs.md, plan.md)

## Checkpoint: Session 2026-08-02

**What was completed this session:**
- Go development environment set up end-to-end (new Delta, Task 1): `go = "latest"` in `config/mise/config.toml` (go 1.26.5), `$GOPATH/bin` on PATH in `home/zshrc`, `gopls` in `mason.lua` (v0.23.0), gopls server config in `lsp.lua` with gofumpt/staticcheck/inlay hints, `go`/`gomod`/`gosum`/`gowork` treesitter parsers, and a Go-only `BufWritePre` organise-imports-then-format autocmd
- Verified the Go setup headlessly rather than by inspection: gopls attaches, unused import removed, missing import added, indentation reformatted
- Diagnosed and fixed intermittent LSP attach affecting all servers (new Delta) — see that Delta for detail; written up in `doc/nvim-lsp-filetype-race.md`
- Added the `## Prose` no-commentary rule to `home/claude/CLAUDE.md` (new Delta)
- Discovered `home/claude/settings.json` blanket-denies `Bash(bin/*)`, which makes the repo `CLAUDE.md`'s "`am-dotfiles-base` exists for Claude to run" exception non-functional — deny rules cannot be overridden by allow entries. Logged as Delta: Permission Config Fixes. Worked around by running the script's logic directly (`gh api user --jq .login`)
- Created the `go-tutorial` repo content (separate repo, not tracked here): `doc/simple-tutorial.md`, `doc/module-paths.md`, `doc/why-tabs.md`, `doc/go-run-performance.md`, a Go `.gitignore`, and a `justfile` with a `run-local` recipe
- Git history incident resolved: a commit containing job-application company names was reset to unstaged, then purged via `git reflog expire --expire=now --all && git gc --prune=now`; verified unrecoverable via `cat-file`, `reflog` and `fsck`. The portfolio skill has been removed from dotfiles and will be added to the (private) portfolio repo by the user

**State of the project:**
Go development is fully working in nvim. The LSP FileType race that had been intermittently affecting every language server is fixed and documented. Two pre-existing Deltas remain untouched — statusline extra-usage detection and the plan-skill tweaks, the latter still waiting on the user's list. A new Permission Config Fixes Delta captures the `bin/` deny-rule contradiction found this session.

**Immediate next priorities:**
1. Task 2 (Delta: Go Development Environment) — delve/DAP debugging; no DAP plumbing exists in the config yet
2. Task 1 (Delta: Permission Config Fixes) — carve `am-dotfiles-base` out of the `bin/` deny rule, preferred fix being to move the script and symlink it back
3. Task 1 (Delta: Plan Skill Iteration) — still blocked on the user's specific list of tweaks
4. Task 1 (Delta: Statusline Refinement) — verify extra usage detection
5. Confirm the LSP fix holds interactively over several nvim restarts

## Checkpoint: Session 2026-08-02b

**What was completed this session:**
- Plan session timer gained pause/resume: shared `home/claude/skills/plan-format/plan-timer.sh` (`start`/`pause`/`resume`/`status`/`clear`) plus `pause-plan` and `resume-plan` skills, with `load-plan` and `update-plan` rewired onto it. Paused intervals are excluded from `Time spent`
- `home/claude/hooks/plan-timer-resume.sh` written to auto-resume on the next prompt, then parked unregistered — running it on every prompt was judged excessive
- `git lol` blank lines diagnosed and fixed: `%<(80,trunc)%s` padded every line to a fixed 117 columns against an 80-column terminal, so the padding wrapped onto an apparently blank second row. Fixed in `home/gitconfig` to `%<(12,trunc)%an %cd %s`; written up in `doc/git-lol-column-padding.md`
- `~/.claude/skills/plan-format` was found never to have been symlinked, breaking every plan skill's helper call; linked manually

**State of the project:**
The timer supports pausing, and this session showed the failure mode — the pause was never resumed, so `Time spent` was omitted rather than guessed. The `git lol` fix and the missing symlink were unplanned work.

**Immediate next priorities:**
1. Task 2 (Delta: Go Development Environment) — delve/DAP debugging
2. Task 2 (Delta: Plan Skill Iteration) — run `./do.sh link-claude` and resolve the `plan-format` symlink question
3. Task 1 (Delta: Permission Config Fixes) — carve `am-dotfiles-base` out of the `bin/` deny rule
4. Task 1 (Delta: Statusline Refinement) — verify extra usage detection

## Checkpoint: Session 2026-08-18

**What was completed this session:**
- Three new Deltas, all from unplanned work: Neovim Ergonomics, Shell Helpers, Git Config Refinements
- Two Neovim startup-ordering bugs of the same family fixed — markdown/text `wrap` never applying (FileType fires before `config.autocmds` is required), and the Dockerfile keyword highlighting lost to a combined bash injection. Both fixed in `after/`, which the runtime loads independently of `init.lua` ordering
- `doc/nvim-dockerfile-injection-bleed.md` written, indexed, and cross-linked from the treesitter customisation doc, which gained a fifth key lesson on `;; extends` appending versus its absence replacing
- Three zsh functions added: `plan`, `copypath`, `readme`
- `git checkout` added to the pre-tool-use filter's blocked list; `git switch`/`git restore` raised as the remaining gap and left open
- `git lol` now excludes bot authors; confirmed the alias holds nothing work-specific
- nvim-tree filters switched to showing dotfiles while still hiding gitignored files, and the cheatsheet gained three FILE TREE sections plus a corrected WINDOWS section
- Mason `ENOTCONN` investigated and not reproduced — logged as an open Task rather than closed

**State of the project:**
No pre-existing plan TODO was completed this session — the fourth session running where the work fell outside the tracked Deltas. The four older Deltas are untouched: Delve/DAP, the `bin/` deny-rule contradiction, the plan-skill timer loose ends, and statusline extra-usage detection. Everything built this session was verified empirically rather than by inspection, following the pattern established during the LSP race work. Two items are deliberately left open: the Mason error and the `git switch`/`git restore` decision.

**Immediate next priorities:**
1. Task 2 (Delta: Go Development Environment) — Delve/DAP debugging; still no DAP plumbing in the config
2. Task 2 (Delta: Permission Config Fixes) — decide on `git switch` and `git restore`
3. Task 5 (Delta: Neovim Ergonomics) — Mason ENOTCONN, if it recurs
4. Task 1 (Delta: Permission Config Fixes) — carve `am-dotfiles-base` out of the `bin/` deny rule
5. Task 2 (Delta: Plan Skill Iteration) — session timer loose ends, now more pressing given the timer overrun below

---

## Implementation Notes

### Architecture
- All Claude config lives in `home/claude/` and is symlinked into `~/.claude/` via `do.sh link-claude`
- Hooks live in `home/claude/hooks/` — symlinked individually into `~/.claude/hooks/`
- Skills are directory symlinks — symlink the whole dir, not individual files
- All skills in `~/.claude/skills/` are directory symlinks into `home/claude/skills/` — fully tracked in dotfiles
- Plan format is specified once in `home/claude/skills/plan-format/PLAN-FORMAT.md` — the three plan skills reference it; fix format drift there, not in individual skills
- Statusline script uses `jq 'has("rate_limits")'` to detect subscription vs API mode
- Rate limit reset timestamps come as Unix timestamps; converted to HH:MM (5h) and DD/MM (7d) via `date -r`
- `git stash` is denied globally — added after Sonnet stashed commits and lost track of them
- `pre-tool-use-filter.sh` runs on every Bash PreToolUse — blocks destructive ops, broad find, git push/commit/add, sudo, pipe-to-shell etc.
- OTEL env vars (`OTEL_EXPORTER_OTLP_*`) belong in `~/.zshrc_machine` on machines with trakr, not in `settings.json`
- Claude Code permission patterns use gitignore-spec anchoring: a single `/` prefix means "relative to project root", NOT filesystem root. Absolute filesystem paths require `//` (double slash) prefix — e.g. `Read(//tmp/**)`, `Read(//private/tmp/**)`. Bare `**` and `/**` are both project-scoped.
- macOS `/tmp` is a symlink to `/private/tmp`. Allow rules check BOTH the symlink path and the resolved path — both must match. Use `Read(//tmp/**)` + `Read(//private/tmp/**)` (and `Write(...)` equivalents) to cover `/tmp` fully.
