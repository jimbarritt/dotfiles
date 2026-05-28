# Publish Crate

Set up and publish a Rust crate to crates.io, following the conventions in `~/projects/ubiq-architecture/publishing-crates.md`.

## Instructions

Read `~/projects/ubiq-architecture/publishing-crates.md` before doing anything else.

The user wants to publish a Rust crate. Work through the following:

1. **Check name availability** — ask the user what crate name they want, then check if it is taken:
   ```sh
   cargo search <name>
   ```

2. **Determine crate naming strategy** — if the name is available, use it directly. If squatted, follow the `-bin` suffix pattern from the architecture doc.

3. **Scaffold the crate** — create the directory structure, `Cargo.toml`, and placeholder `src/main.rs` per the patterns in the architecture doc. The placeholder `main.rs` should print a "coming soon" message with the GitHub repo URL.

4. **Verify it builds and runs** — show the user the command but let them run it:
   ```sh
   cargo run
   ```

5. **Give step-by-step publish instructions** — following the "How to Publish" section of the architecture doc:
   - Confirm the GitHub repo is public
   - `cargo login`
   - `cargo publish`
   - `cargo search <name>` to verify

Do not run `cargo publish` yourself — give the steps to the user to execute.
