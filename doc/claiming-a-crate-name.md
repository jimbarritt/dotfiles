# Claiming a crate name on crates.io

Steps to publish an early/placeholder version of a crate purely to claim
its name on crates.io, before the crate is finished (or even started).
See `~/projects/ubiq-architecture/publishing-crates.md` for the
`{project-name}-bin` naming convention to use if the desired name is
already taken.

## 1. Log in

```sh
cargo login
```

Prompts for a crates.io API token — get one at
https://crates.io/settings/tokens

## 2. Publish each crate, in dependency order

If publishing a workspace with multiple crates, publish dependencies
before dependents — crates.io requires a published dependency to
resolve by version, not by local path, so a crate that depends on a
sibling crate via `{ path = "...", version = "..." }` can't publish
until that sibling is live on crates.io with a matching version.

```sh
cd <crate-dir>
cargo publish --dry-run
cargo publish
```

## Prerequisites

- The crate's `repository` URL (in `Cargo.toml`) must point to a
  **public** GitHub repo at publish time — crates.io fetches it.
- `Cargo.toml` needs `description`, `license`, and (for discoverability)
  `keywords`/`categories` filled in — see the `{project-name}-bin`
  template in `publishing-crates.md` for the fields crates.io expects.

## Verify afterwards

```sh
cargo search <crate-name>
```

`cargo search` has been unreliable in some sandboxed/CI environments
(crates.io API blocking automated requests, empty results even for
crates that published fine) — if it looks wrong, check directly at
`https://crates.io/crates/<crate-name>` before assuming the publish
failed.
