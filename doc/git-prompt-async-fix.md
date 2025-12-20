# Oh-My-Zsh Async Git Prompt Fix

## Problem

When using oh-my-zsh with git repositories, the git branch information would appear with a noticeable delay after the prompt was displayed. This created a distracting "pop-in" effect:

1. `cd` into a git directory
2. Prompt appears immediately: `~/my-repo $`
3. 50-100ms later, git info pops in: `~/my-repo [main] $`

Additionally, when leaving git directories, stale git info would briefly flash before disappearing.

## Root Cause

Oh-my-zsh uses asynchronous git status checks by default via the `_omz_async_request` precmd hook. This was designed to prevent blocking the prompt in large repositories, but creates visual inconsistency and cognitive load from the delayed rendering.

### Diagnostic Commands Used

```bash
# Check which precmd hooks are registered
echo $precmd_functions
# Output showed: _omz_async_request omz_termsupport_precmd ...

# Check which chpwd hooks are registered  
echo $chpwd_functions
```

## Solution

Force oh-my-zsh to use **synchronous** git prompt updates by setting the async threshold to 0.

### Configuration

Add this to `.zshrc` **before** sourcing oh-my-zsh:

```bash
ZSH_THEME="green-tinted"
ZSH_DISABLE_COMPFIX="true"

# Force synchronous git prompt updates - eliminates delayed pop-in effect
OMZ_ASYNC_THRESHOLD=0

VI_MODE_SET_CURSOR=true
VI_MODE_RESET_PROMPT_ON_MODE_CHANGE=true

# ... rest of config
source $ZSH/oh-my-zsh.sh
```

### What This Does

`OMZ_ASYNC_THRESHOLD=0` tells oh-my-zsh to **always** complete git status checks before rendering the prompt, regardless of repository size. The prompt waits for git info to be ready, eliminating the visual delay and "pop-in" effect.

## Trade-offs

### Before (Async)
- ❌ Visual "pop-in" when entering git directories
- ❌ Flashing stale info when leaving git directories
- ❌ Inconsistent prompt rendering (cognitive load)
- ✅ Technically faster in very large repos

### After (Synchronous)
- ✅ Consistent, predictable prompt rendering
- ✅ No visual delays or flashing
- ✅ Lower cognitive load
- ⚠️ Small blocking delay (20-50ms) in git directories

The synchronous approach prioritizes visual consistency over raw speed, which aligns better with workflows focused on minimizing cognitive load and context switching.

## Performance Optimization

To keep the synchronous approach fast, also ensure untracked file checking is disabled:

```bash
# Already set in .zshrc - keeps git status checks fast
DISABLE_UNTRACKED_FILES_DIRTY="true"
```

This prevents git from scanning for untracked files, significantly speeding up the status check in repositories with many files.

## Alternative Approaches Tried

### ❌ Using precmd to cache git info
```bash
# This caused stale info to flash when leaving directories
precmd() {
    GIT_INFO="$(git_prompt_info)"
}
PROMPT='%{$fg[green]%}%~ %{$fg[cyan]%}${GIT_INFO}%{$reset_color%}
$ '
```

### ❌ Removing the async hook entirely
```bash
# This broke git prompt completely
precmd_functions=("${(@)precmd_functions:#_omz_async_request}")
```

### ✅ Setting async threshold to 0 (final solution)
```bash
OMZ_ASYNC_THRESHOLD=0
```

## Context

- **Shell**: zsh with oh-my-zsh
- **Theme**: green-tinted (custom colors only, no prompt definition)
- **Prompt definition**: Defined directly in `.zshrc`
- **Git prompt**: Uses oh-my-zsh's built-in `git_prompt_info()` function
- **Terminal**: Kitty (not Emacs vterm)

## References

- Oh-my-zsh async prompt implementation: `_omz_async_request` precmd hook
- Git prompt function: `git_prompt_info()` from oh-my-zsh lib
- Performance setting: `DISABLE_UNTRACKED_FILES_DIRTY="true"`
