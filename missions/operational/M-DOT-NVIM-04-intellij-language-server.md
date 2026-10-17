# Mission: IntelliJ language server for Java and Kotlin

| Field | Value |
|---|---|
| ID | M-DOT-NVIM-04 |
| Status | TODO |
| Blocked by | none |

## Objective

- Java and Kotlin files in Neovim use JetBrains' IntelliJ language server (`intellij-server`) through one Neovim plugin.
- `jdtls` and `kotlin.nvim`, with its cask detection code, are removed from the config.
- The nvim docs describe the install, the licence and troubleshooting for the new server.
- The setup works on each machine Jim uses for Java and Kotlin.

## Purpose

JetBrains released "Java and Kotlin by IntelliJ IDEA" 1.0 in October 2026: one LSP server, built from IntelliJ IDEA, for Java and Kotlin. It replaces two servers in this config: `jdtls` for Java and the alpha `kotlin-lsp` for Kotlin. The mission is also a trial of tsk for a short mission.

## Constraints

- The server needs an IntelliJ IDEA Ultimate or All Products Pack subscription after a 30-day trial.
- Jim's work licence may not be used outside work.
- The 1.0 licence covers human use only. Do not start the server from Claude Code or another agent until JetBrains publishes agent licensing.
- Changes go upstream in the dotfiles repo only, and plugin versions are pinned in `lazy-lock.json`.

## Out of scope

- Claude Code's own LSP plugins (`doc/claude-lsp-integration.md`).
- Language servers for other languages.

## Decisions

- Run this as a tsk mission, to try tsk on a short mission. Jim, 2026-10-17
- Do not start this mission yet. The licence is unresolved. Jim, 2026-10-17

## Open questions

- Pay for a personal IntelliJ IDEA Ultimate licence (see Pricing in the intel), or keep the free `kotlin-lsp` and `jdtls` outside work?
- Which Neovim plugin: the official `JetBrains/intellij-lsp.nvim` (pre-alpha, Neovim 0.11+), or `AlexandrosAlexiou/intellij-server.nvim` (by the `kotlin.nvim` author, Neovim 0.12+)?
- Which Neovim version runs on each machine?
- Keep the free `kotlin-lsp` cask as a fallback if the licence lapses, or remove it?
- Is the `bin/intellij-server` in the `kotlin-lsp` cask the same product as the 1.0 bundle, under the same licence?

## Plan

| ID | Task | Status | Blocked by |
|---|---|---|---|
| T-01 | Confirm the licence on each machine | TODO | |
| T-02 | Check the Neovim version on each machine | TODO | |
| T-03 | Choose the Neovim plugin | TODO | |
| T-04 | Install and configure the plugin for Java and Kotlin, and remove jdtls and kotlin.nvim | TODO | T-01, T-02, T-03 |
| T-05 | Verify on a Kotlin Gradle project and a Java project: attach, completion, go-to-definition, rename, format, diagnostics | TODO | T-04 |
| T-06 | Update the nvim docs and lsp-doctor for the new server | TODO | T-05 |

## Intel

- Research on the server, its licence, its pricing and the two Neovim plugins: `missions/operational/M-DOT-NVIM-04/intel/intellij-server-research.md`

## Answered questions

- Q-01: Is a licence available on each machine, and does a work licence cover a personal machine? Jim has a licence at work, but its terms forbid use outside work. Jim, 2026-10-17
