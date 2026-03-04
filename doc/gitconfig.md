# Git Configuration

## What is `.gitconfig`?

The `.gitconfig` file is Git's per-user configuration file, stored at `~/.gitconfig`. It controls the behaviour of Git across all repositories on your machine. Settings here can be overridden at the repository level via `.git/config` inside a specific repo.

Git reads configuration from three scopes, each narrower scope overriding the broader one:

1. **System** — `/etc/gitconfig` (applies to all users on the machine)
2. **Global** — `~/.gitconfig` (applies to your user account)
3. **Local** — `.git/config` inside a repo (applies only to that repo)

Full reference: [git-config documentation](https://git-scm.com/docs/git-config)

---

## The Config

```ini
[user]
    name = Your Name
    email = you@example.com
    signingkey = GPGKEYID

[init]
    defaultBranch = main

[color]
    diff = auto
    status = auto
    branch = auto
    interactive = auto

[alias]
    sm = submodule
    st = status
    br = branch
    serve = !git daemon --reuseaddr --verbose  --base-path=. --export-all ./.git
    lol = !git log --reverse --format='%C(yellow) %h %Creset %<(10)%an %<(12)%cd %<(80,trunc)%s' --date=format:'%y-%m-%d %H:%M' -15

[push]
    default = matching

[core]
    pager = delta
    autocrlf = input

[delta]
    # Appearance
    navigate = true    # Use n/N to navigate between diff sections
    light = false      # Set to true if you use a light terminal theme
    side-by-side = false  # Set to true if you prefer side-by-side diffs
    line-numbers = true
    syntax-theme = Monokai Extended

    # Minimal visual noise
    minus-style = "syntax #3f0001"
    minus-emph-style = "syntax #901011"
    plus-style = "syntax #002800"
    plus-emph-style = "syntax #006000"

[merge]
    conflictstyle = diff3

[diff]
    colorMoved = default

[filter "lfs"]
    clean = git-lfs clean %f
    smudge = git-lfs smudge %f
    required = true

[credential]
    helper = !aws codecommit credential-helper $@
    UseHttpPath = true

[pager]
    branch = false
    log = false

# Include work config if it exists (checked last, so it overrides)
[include]
    path = ~/.gitconfig-work
```

---

## Section Breakdown

### `[user]`

Identifies you as the author of commits. `name` and `email` appear in every commit you make. `signingkey` links to a GPG key used for signing commits and tags.

- [git-commit docs](https://git-scm.com/docs/git-commit)
- [GPG signing](https://git-scm.com/book/en/v2/Git-Tools-Signing-Your-Work)

### `[init]`

`defaultBranch = main` sets the name of the branch created when you run `git init`, replacing the old default of `master`.

### `[color]`

Enables terminal colour output for diff, status, branch, and interactive commands. `auto` means colour is used when output goes to a terminal, but not when piped.

- [color config reference](https://git-scm.com/docs/git-config#Documentation/git-config.txt-colorui)

### `[alias]`

Shortcuts for common commands. Aliases prefixed with `!` run shell commands rather than git subcommands.

| Alias | Expands to | Notes |
|-------|------------|-------|
| `sm`  | `submodule` | Shorthand for submodule operations |
| `st`  | `status` | |
| `br`  | `branch` | |
| `serve` | `git daemon ...` | Serves the repo over the git protocol on your local machine, useful for quick local sharing |
| `lol` | `git log ...` | A compact, coloured one-line log of the last 15 commits with author, date, and truncated subject |

### `[push]`

`default = matching` pushes the current branch to a remote branch of the same name, but only if that remote branch already exists. This is the pre-Git-2.0 default; `simple` is now the upstream default (pushes only the current tracking branch).

- [push.default](https://git-scm.com/docs/git-config#Documentation/git-config.txt-pushdefault)

### `[core]`

- `pager = delta` — replaces the default pager (`less`) with [delta](https://github.com/dandavison/delta) for syntax-highlighted diffs.
- `autocrlf = input` — on input (commit), converts CRLF line endings to LF, but leaves files unchanged on checkout. Appropriate for macOS/Linux to avoid accidentally committing Windows line endings.

### `[delta]`

Configuration for the [delta](https://github.com/dandavison/delta) diff pager. Delta adds syntax highlighting, line numbers, and better diff presentation on top of standard git output.

- `navigate = true` — press `n`/`N` to jump between diff hunks
- `light = false` — use the dark terminal colour scheme
- `side-by-side = false` — unified diff view (set to `true` for side-by-side)
- `line-numbers = true` — show line numbers in the gutter
- `syntax-theme = Monokai Extended` — syntax highlighting colour scheme
- `minus-style`, `minus-emph-style`, `plus-style`, `plus-emph-style` — custom background colours for removed/added lines, tuned for low visual noise

### `[merge]`

`conflictstyle = diff3` changes how merge conflicts are displayed. Instead of the two-way `<<<<`/`====`/`>>>>` markers, diff3 adds a third section showing the common ancestor, making it easier to understand what changed on each side.

- [merge.conflictstyle](https://git-scm.com/docs/git-config#Documentation/git-config.txt-mergeconflictStyle)

### `[diff]`

`colorMoved = default` highlights lines that were moved (rather than added/deleted) in a different colour, making refactors easier to read.

- [diff.colorMoved](https://git-scm.com/docs/git-config#Documentation/git-config.txt-diffcolorMoved)

### `[filter "lfs"]`

Configures [Git LFS](https://git-lfs.com) (Large File Storage). The `clean` and `smudge` filters run when staging and checking out files respectively, transparently replacing large file contents with LFS pointers. `required = true` means Git will error if the LFS filter is missing.

### `[credential]`

- `helper = !aws codecommit credential-helper $@` — uses the AWS CLI's built-in credential helper to authenticate against AWS CodeCommit repositories.
- `UseHttpPath = true` — passes the full HTTP path to the credential helper, which is required when accessing multiple CodeCommit repos across different AWS accounts.

### `[pager]`

Disables the pager for `git branch` and `git log`, so output goes directly to the terminal rather than opening in `less`/`delta`. Useful when you want quick, scrollable output in your shell.

### `[include]`

Includes an additional config file if it exists. `~/.gitconfig-work` is loaded last, meaning any settings it defines override the ones above. This is a clean way to separate work-specific settings (different email, signing key, etc.) without cluttering the main config.

- [Conditional includes](https://git-scm.com/docs/git-config#_includes)

---

## Lazygit

[Lazygit](https://github.com/jesseduffield/lazygit) is a terminal UI for Git that makes common operations (staging, branching, rebasing, resolving conflicts) fast and visual without leaving the terminal. It respects your `.gitconfig` settings, including aliases and delta for diffs.

It pairs well with this config — delta's enhanced diff output renders inside lazygit's diff panel, and the `merge.conflictstyle = diff3` setting improves the conflict resolution view.

Install with Homebrew:

```sh
brew install lazygit
```

Run it from inside any repo:

```sh
lazygit
```
