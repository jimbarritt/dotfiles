# Useful Unix Commands

## GREP

Look for something within files recursively in a dir. This one lists out the line number aswell

```
grep -rn --include "*.el" "live-load" .
```

## Search bash history

    cat ~/.bash_history | grep -e "^knife" | sort | uniq -u

## Find all files of a certain type

    find . -name '*.pl'

## Move a set of files listed in a file to a certain directory

    cat files.txt | xargs -I {} mv {} to/
