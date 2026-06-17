# Claude: Avoid Broad Filesystem Scans

## The problem

When Claude needs to find a file — source code for a dependency, a generated client, or a related library — it sometimes reaches for broad `find` or `xargs` commands rooted at the home directory or cache directories:

```bash
find ~ -name "SomeApi.kt"
find ~/.gradle -name "*.jar" | xargs -I{} jar tf {} | grep SomeClass
```

These commands are expensive: they can touch hundreds of thousands of inodes, extract every jar in a local cache, or scan deeply nested directories that have nothing to do with the task. The wall-clock cost on a large developer machine can be seconds to minutes.

## Why it happens

Claude fails to check obvious local sources first. In a multi-repo workspace, related code often lives in a sibling directory that is documented in `CLAUDE.md`. Claude may not connect "I need to find the API routes for this generated client" with "the source for that client is in `../some-client-repo`" — and instead defaults to a breadth-first filesystem search.

Typical failure pattern:

1. Needs a file from a dependency (e.g. a generated API client)
2. File isn't in the current project
3. Tries `find ~` or cache extraction instead of checking documented sibling repos
4. Gets an empty result or a match after scanning gigabytes of irrelevant paths

## Prevention

### 1. Document sibling repos in CLAUDE.md

List every related repo by its relative path and what it contains:

```markdown
## Related repos

| Repo | Path | What's in it |
|------|------|--------------|
| api-client | `../api-client` | Generated OpenAPI client — source of truth for HTTP routes |
| shared-models | `../shared-models` | Shared domain types |
```

### 2. Add an explicit lookup rule to CLAUDE.md

```markdown
## Finding external code

When looking for generated client code, API route definitions, or shared library sources:
1. Check the sibling repos listed in this file first
2. Never use `find` rooted at `~`, `/`, or a cache directory (`~/.gradle`, `~/.m2`, etc.)
3. If the source cannot be found locally, ask rather than scanning
```

### 3. Add a hook to block dangerous find commands

In `.claude/settings.json`, add a `PreToolUse` hook that rejects `find` commands rooted outside the project:

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "echo \"$CLAUDE_TOOL_INPUT\" | grep -qE 'find (~/|/Users/|/home/)' && echo 'BLOCKED: use project-relative paths or check sibling repos' && exit 1 || exit 0"
          }
        ]
      }
    ]
  }
}
```

This makes the bad behaviour visibly fail rather than silently succeed after burning I/O.

### 4. Ask instead of searching

If the sibling repos listed in CLAUDE.md don't contain what's needed, Claude should say so and ask — not start a broader search. A one-line question costs nothing compared to a full home-directory scan.

## Summary

| Approach | Effectiveness | Effort |
|----------|--------------|--------|
| Document sibling repos in CLAUDE.md | High — removes ambiguity | Low |
| Add explicit lookup rule to CLAUDE.md | High — direct instruction | Low |
| Pre-tool hook to block wide `find` | Medium — hard backstop | Medium |
| Ask the user when stuck | Always correct | Zero |
