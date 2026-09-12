# Git Worktrees and Rebasing

Two related habits: keep each branch in its own directory, and keep each branch
on top of `main`. Both need configuration to be safe.

---

## Worktrees

A worktree is a second checkout of the same repository. Each worktree holds one
branch, in its own directory, sharing one `.git` object store. You switch branch
by changing directory, so nothing rebuilds and nothing reinstalls.

`git worktree add` puts the new directory wherever you point it, and the usual
answer is inside the repository. That is the wrong place: every tool that walks
the tree then sees a second copy of the codebase, so each repository needs a
`.gitignore` entry and each linter needs an exclude rule.

`bin/git-wt` fixes the placement and the setup. Git treats any executable named
`git-<name>` on `PATH` as a subcommand, so the file runs as `git wt`.

```
git wt new <branch> [start-point]   create a worktree, ready to run
git wt list                         list this repository's worktrees
git wt path <branch>                print a worktree's path
git wt rm <branch>                  remove a worktree, if it is clean
git wt clean                        remove worktrees whose branch is merged
git wt prune                        forget worktrees whose directory is gone
```

Worktrees live at `$GIT_WORKTREES_ROOT/<repo>/<branch>`, which defaults to
`~/Code/git-worktrees`. That is outside every repository, so nothing rooted at a
repository can see them.

`new` does three things `git worktree add` does not:

- It copies gitignored `.env`, `.env.local` and `.env.*.local` files from the
  main worktree. A worktree without them does not run.
- It runs `pnpm install` when it finds a `pnpm-lock.yaml`. Pass `--no-install`
  to skip it.
- It copies the new path to the clipboard.

A branch name may hold a slash. `new` replaces each slash with a hyphen, so a
worktree is always exactly one level below the repository directory.

`clean` asks the forge as well as the history. A squash or rebase merge leaves
no ancestry, so `git merge-base --is-ancestor` reports nothing merged. The
script falls back to `gh pr list --state merged`.

Two limits are worth knowing. `git wt --help` fails, because git rewrites
`--help` into a man-page lookup — run `git wt` alone for usage. And a subcommand
cannot change the calling shell's directory, so `new` prints the path and you
`cd` there yourself.

---

## Rebasing onto main

```
git rebase-main
```

The alias runs `git pull --rebase origin main`. It fetches `main` and replays
your commits on top of it.

```ini
[alias]
    rebase-main = !git pull --rebase origin main
```

Rebase and merge both bring `main`'s changes in, and the resulting content is
the same. The difference is history. A rebase moves your commits on top of
`main`, so the branch reads linearly and its diff shows only your work. A merge
records a merge commit, and the branch keeps its original commits.

Rebase when the branch is yours alone. Merge when someone else works on the
branch, because a rebase rewrites commits they already hold.

After a rebase the branch has new commit IDs, so a push needs
`--force-with-lease`. Never plain `--force`: `--force-with-lease` refuses if
someone pushed in the meantime.

### Two settings that make it safe

```ini
[rebase]
    autoStash = true

[rerere]
    enabled = true
```

`rebase.autoStash` stashes uncommitted work before the rebase and restores it
after. Without it, a rebase with a dirty tree refuses to start.

`rerere` records how you resolved a conflict and replays that resolution when
the same conflict appears again. A long-lived branch hits the same conflict on
every rebase, and `rerere` resolves it after the first time.

### A merge commit breaks a rebase

`git rebase` without `--rebase-merges` flattens merge commits. If your branch
merged `main` earlier, the rebase queues every commit that merge brought in, and
the cherry-pick equivalence check compares against the old base rather than
`main`. A branch with two real commits can queue eighty-nine.

The way out is to rebuild the branch linearly: reset it to `main` and cherry-pick
its own commits.

```
git branch -f rebuilt origin/main
git cherry-pick <commit> <commit>
```

So pick one style per branch. If a branch will be rebased, do not merge `main`
into it.

### diff3 conflict markers

`merge.conflictstyle = diff3` adds a third section to each conflict, showing the
common ancestor:

```
<<<<<<< HEAD
your side
||||||| base
the common ancestor
=======
their side
>>>>>>> origin/main
```

The ancestor tells you what each side changed, rather than leaving you to guess.
Any script that strips conflict markers must handle all four, not three — a
stray `|||||||` line is easy to commit by accident.

### Which branch a bare push sends

```ini
[push]
    default = simple
```

`simple` pushes the current branch only. The older `matching` pushes every local
branch that has a same-named branch on the remote, so a bare `git push` from a
feature branch also pushes `main`.

---

## See also

- [gitconfig.md](gitconfig.md) — the full git configuration
- [bin-scripts.md](bin-scripts.md) — the scripts in `bin/`
- [git-worktree](https://git-scm.com/docs/git-worktree)
- [git-rerere](https://git-scm.com/docs/git-rerere)
