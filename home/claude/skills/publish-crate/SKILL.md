---
name: publish-crate
description: Set up and publish a Rust crate to crates.io following the ubiq-architecture publishing conventions. Use when asked to publish, reserve, or scaffold a new crate.
---

# Publish Crate

Set up and publish a Rust crate to crates.io, following the conventions in `~/projects/ubiq-architecture/publishing-crates.md`. For the general "just claim the name now" flow (no architecture-doc access needed), see `~/projects/dotfiles/doc/claiming-a-crate-name.md`.

## Instructions

Read `~/projects/ubiq-architecture/publishing-crates.md` before doing anything else.

The user wants to publish a Rust crate. Work through the following:

1. **Check name availability** — ask the user what crate name they want, then check if it is taken. `cargo search <name>` has proven unreliable (crates.io blocks/rate-limits automated requests, sometimes returning empty even for taken names) — cross-check at `https://crates.io/crates/<name>` directly.

2. **Determine crate naming strategy** — if the name is available, use it directly. If squatted, follow the `-bin` suffix pattern from the architecture doc.

3. **Scaffold the crate, if it doesn't already have real content** — create the directory structure, `Cargo.toml`, and placeholder `src/main.rs` per the patterns in the architecture doc. The placeholder `main.rs` should print a "coming soon" message with the GitHub repo URL. If the crate already exists with real code (e.g. publishing an existing workspace member just to claim its name), skip scaffolding — publish what's there.

4. **Verify it builds and runs** — show the user the command but let them run it:
   ```sh
   cargo run
   ```

5. **Give step-by-step publish instructions** — following the "How to Publish" section of the architecture doc (also summarised in `~/projects/dotfiles/doc/claiming-a-crate-name.md`):
   - Confirm the GitHub repo is public
   - `cargo login`
   - Publish workspace dependencies before dependents — a crate depending on a sibling via `{ path = ..., version = ... }` can't publish until that sibling is live on crates.io at a matching version
   - `cargo publish` (there is no separate "reserve/init" step — the first successful `cargo publish` for a name both creates and claims it)
   - `cargo search <name>` to verify, falling back to the crates.io page directly if search looks wrong

Do not run `cargo publish` yourself — give the steps to the user to execute.
