# Tips and tricks about configuring zsh via .zshrc

# Shell Completion Issue: fpath Duplication

## Problem

Tab completion stopped working (typing `cd do<TAB>` did nothing).

**Root cause:** The `$fpath` variable accumulated massive duplicates from repeatedly using `source ~/.zshrc` to reload configuration. Instead of ~15 entries, it had hundreds of duplicate paths, breaking the completion system.

## Solution

### 1. Prevent fpath duplicates

Add this near the top of `.zshrc` (right after `export ZSH=`):
```zsh
export ZSH="$HOME/.oh-my-zsh"

# Prevent fpath and path duplicates
typeset -U path fpath
```

The `typeset -U` flag makes these arrays unique, automatically removing duplicates even when sourcing `.zshrc` multiple times.

### 2. Use `exec zsh` instead of `source ~/.zshrc`

**Bad practice:**
```zsh
source ~/.zshrc  # Accumulates state, doesn't truly reload
```

**Good practice:**
```zsh
exec zsh  # Clean reload, replaces current shell process
```

**Why `exec zsh` is better:**
- Complete fresh start - no accumulated state
- Can't build up duplicates in environment variables
- True reload - like opening a new terminal
- Removes things you've deleted from config

### 3. Add convenience alias
```zsh
# Add to .zshrc
alias reload="exec zsh"

# Then just use:
reload
```

## Verification

After applying the fix:
```zsh
# Check fpath size (should be ~15-20 entries)
echo $fpath | tr ' ' '\n' | wc -l

# Test completion
cd do<TAB>  # Should complete to 'doc'
```
