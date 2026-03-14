# Shell Launch Guard

A lightweight `.zshrc` defence that prompts for a password when the shell is
launched from an unrecognised context — e.g. a malicious downloaded script or
a double-clicked `.command` file.

## Motivation

Most shell-based malware is generic and opportunistic. It relies on the shell
starting silently with no user interaction. Adding a password check that only
triggers outside of known terminals defeats this class of attack without
affecting normal usage.

This is a speed bump, not a lock. A targeted attacker who knows your setup
could spoof the env vars checked below. Gatekeeper and macOS quarantine flags
are your real first line of defence — this is a second layer.

## How it works

A whitelist of known terminal environments is checked via two signals: the env
vars those terminals set, and the parent process name. Both must match. This
defeats the attack where a malicious script launches Kitty (which sets
`$KITTY_WINDOW_ID`) to trick the guard — the parent process would be `sh` or
`zsh`, not `kitty`, so the check fails.

If neither signal matches and the shell is interactive, a password is required
to continue. The password is stored in `~/.shellpwd` (not checked in anywhere).

```sh
_process_tree_contains() {
  local name=$1 pid=$PPID
  for _level in 1 2 3; do
    local comm=$(ps -o comm= -p $pid 2>/dev/null)
    [[ "$comm" == *$name* ]] && return 0
    pid=$(ps -o ppid= -p $pid 2>/dev/null | tr -d ' ')
    [[ -z "$pid" || "$pid" == "1" ]] && break
  done
  return 1
}

_is_known_terminal() {
  [[ -n "$KITTY_WINDOW_ID" ]] && _process_tree_contains "kitty" && return 0
  [[ "$TERM_PROGRAM" == "vscode" ]] && _process_tree_contains "electron" && return 0
  [[ "$TERM_PROGRAM" == "JetBrains-JediTerm" ]] && _process_tree_contains "java" && return 0
  [[ -n "$INSIDE_EMACS" ]] && _process_tree_contains "emacs" && return 0
  [[ -n "$TMUX" ]] && _process_tree_contains "tmux" && return 0
  [[ -n "$SSH_CONNECTION" ]] && return 0
  return 1
}

if [[ $- == *i* ]] && ! _is_known_terminal; then
  echo "  shell guard: unrecognised launch context (TERM_PROGRAM=${TERM_PROGRAM:-unset})"

  if [[ ! -f "$HOME/.shellpwd" ]]; then
    echo "  abort: ~/.shellpwd not found."
    exit 1
  fi

  trap 'echo; echo "  abort: interrupted."; exit 1' INT
  read -rs "  password: " _entered
  echo
  trap - INT

  if [[ "$_entered" != "$(cat "$HOME/.shellpwd")" ]]; then
    echo "  abort: wrong password."
    exit 1
  fi
  unset _entered
fi
```

## Setup

```sh
echo 'your-password-here' > ~/.shellpwd && chmod 600 ~/.shellpwd
```

`chmod 600` ensures only your user can read the file.

## macOS first line of defence: Gatekeeper + Quarantine

Before your shell even starts, macOS has two built-in protections worth knowing about.

**Quarantine flag (`com.apple.quarantine`)**
Every file downloaded via a browser, email client, or any app that uses the
macOS download APIs gets tagged with a quarantine extended attribute. You can
see it with:

```sh
xattr -l ~/Downloads/suspicious-script.sh
# com.apple.quarantine: 0083;...
```

When you try to run a quarantined executable, macOS intercepts it and shows a
warning dialog ("This file was downloaded from the internet..."). This is what
saved you — the oh-my-zsh installer likely triggered this check.

You can inspect or remove the flag manually:
```sh
xattr -l file.sh          # inspect
xattr -d com.apple.quarantine file.sh  # remove (only do this if you trust the file)
```

**Gatekeeper**
Sits on top of quarantine. Checks that executables are either from the App
Store or signed by a registered Apple Developer ID. Unsigned binaries from the
internet are blocked by default. Scripts (`.sh`, `.command`) aren't code-signed
in the same way but still get the quarantine dialog treatment.

**Why the shell guard is still useful**
Quarantine only applies to files downloaded via co-operating apps. A file
copied over AirDrop, cloned from git, or downloaded with `curl` in a terminal
won't have the quarantine flag. The shell launch guard covers that gap.

More importantly: Gatekeeper can be bypassed by the user. The download page
just says "click Allow to install" and most people do — including careful ones,
in a moment of inattention. Once you've clicked through, Gatekeeper is done.
The shell guard is a second prompt that fires *after* Gatekeeper, inside the
shell itself, and requires something the malicious script cannot know or
automate: your password.

## Adding a new terminal to the whitelist

Find the env var your terminal sets and add a line to `_is_known_terminal`.
Common ones:

| Terminal           | Env var check                                         | Parent process  |
|--------------------|-------------------------------------------------------|-----------------|
| Kitty              | `[[ -n "$KITTY_WINDOW_ID" ]]`                        | `*kitty*`       |
| VS Code            | `[[ "$TERM_PROGRAM" == "vscode" ]]`                  | `*electron*`    |
| IntelliJ/JetBrains | `[[ "$TERM_PROGRAM" == "JetBrains-JediTerm" ]]`      | `*java*`        |
| Emacs              | `[[ -n "$INSIDE_EMACS" ]]`                           | `*emacs*`       |
| tmux               | `[[ -n "$TMUX" ]]`                                   | `*tmux*`        |
| SSH                | `[[ -n "$SSH_CONNECTION" ]]`                         | (skip — sshd)   |
| Terminal.app       | `[[ "$TERM_PROGRAM" == "Apple_Terminal" ]]`          | `*Terminal*`    |
