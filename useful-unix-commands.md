# Useful Unix Commands

## GREP

Look for something within files recursively in a dir. This one lists out the line number aswell

```
grep -rn --include "*.el" "live-load" .
```

## Search bash history

    cat ~/.bash_history | grep -e "^knife" | sort | uniq -u
