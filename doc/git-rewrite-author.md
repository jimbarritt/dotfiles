# Rewriting Git Commit Authors

## Problem

Commits made by Claude Code are authored as `Claude <noreply@anthropic.com>`. You may want to rewrite these to your own name/email.

## Recommended tool: git-filter-repo

`git-filter-repo` is the officially recommended replacement for `git filter-branch`. Simon Willison covers it in his TILs and agentic engineering guide.

```sh
brew install git-filter-repo
```

### Using a mailmap file

Create a `mailmap.txt`:

```
Jim Barritt <jim.barritt@gmail.com> Claude <noreply@anthropic.com>
```

Run:

```sh
git filter-repo --mailmap mailmap.txt --force
```

`--force` is needed if the repo isn't a fresh clone.

### Using a commit callback (more targeted)

```sh
git filter-repo --commit-callback '
if commit.author_email == b"noreply@anthropic.com":
    commit.author_name = b"Jim Barritt"
    commit.author_email = b"jim.barritt@gmail.com"
    commit.committer_name = b"Jim Barritt"
    commit.committer_email = b"jim.barritt@gmail.com"
' --force
```

## After rewriting

- All commit SHAs from the rewritten commits onward will change
- You must force push: `git push --force`
- Anyone with existing clones will need to re-clone or `git pull --rebase`

## References

- [git-filter-repo (Simon Willison's TIL)](https://til.simonwillison.net/git/git-filter-repo)
- [Using Git with coding agents (Simon Willison)](https://simonwillison.net/guides/agentic-engineering-patterns/using-git-with-coding-agents/)
- [git-filter-repo GitHub](https://github.com/newren/git-filter-repo)
- [git-rewrite-author (cytopia)](https://github.com/cytopia/git-rewrite-author) — simpler shell-script alternative
