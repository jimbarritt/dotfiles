# M-DOT-NVIM-04 intel: the IntelliJ language server

Gathered 2026-10-17. The JetBrains blog and jetbrains.com were not reachable from the cloud session. The blog facts below come from search result summaries of those pages. The plugin facts come from the GitHub READMEs, read directly.

## The product

- Name: "Java and Kotlin by IntelliJ IDEA". The extension ID on Open VSX is `JetBrains/intellij-server`.
- 1.0 is the first stable release, announced in October 2026. It follows a free preview announced in August 2026 ("IntelliJ IDEA Goes LSP").
- It is an LSP server for Java and Kotlin, built from IntelliJ IDEA. The extension targets VS Code and Cursor. The server is a standalone process that speaks LSP over stdio, started by `bin/intellij-server`, with its own bundled JBR. No separate JDK is needed to run it.
- Build systems: Maven, Gradle and Bazel. The JetBrains Neovim plugin also lists JPS.
- Features: completion, navigation, diagnostics, quick fixes, refactorings, formatting with IntelliJ's code style, type and call hierarchy, decompiled `jar:` and `jrt:` sources, JUnit test runners, and debugging with async stack traces.

## Licence

- 1.0 needs an IntelliJ IDEA Ultimate or All Products Pack subscription after a 30-day free trial. The free preview ended with 1.0.
- 1.0 is licensed for human use only. Agent use, including terminal agents such as Claude Code and Codex, is not covered yet. JetBrains says agent licensing comes later.
- EAP server builds stop working 30 days after their release.
- The bundle has its own EULA (`EULA.txt` in the bundle), which the Neovim plugins make you accept in config.
- The separate Kotlin-only server, `Kotlin/kotlin-lsp`, stays Apache 2.0 and free. Its README says it is alpha and partly closed-source.

## Pricing

Gathered 2026-10-17 from search results. The JetBrains store was not reachable from the cloud session, the sources give USD only, and some are third-party summaries. Check the store at <https://www.jetbrains.com/idea/buy/> for the current GBP price.

| Plan (individual, personal use) | Year 1 | Year 2 | Year 3 on | Monthly |
|---|---|---|---|---|
| IntelliJ IDEA Ultimate | $199 | $159 | $119 | $19.90 |
| All Products Pack | $289 | $231 | $173 | |

- The lower prices in years 2 and 3 are JetBrains continuity discounts for an unbroken subscription.
- JetBrains raised prices on 1 October 2025. One source gives Ultimate year 1 as $169 before the rise. heise.de reported the All Products Pack individual price rising from 289 to 299 euros a year. The sources disagree on the All Products Pack base price: some give $249 a year.
- For comparison, an organisation licence is $719 to $779 per user per year for Ultimate, depending on the source.
- The 30-day trial applies before a subscription is needed. The free Kotlin-only server, `Kotlin/kotlin-lsp`, costs nothing.

## Neovim clients

| | `JetBrains/intellij-lsp.nvim` | `AlexandrosAlexiou/intellij-server.nvim` |
|---|---|---|
| Owner | JetBrains (official) | The author of `kotlin.nvim`, which this config uses now |
| Status | "pre-alpha"; options can change without deprecation; no releases | 56 commits; no releases; pins server 263.7084.0 |
| Plugin licence | Apache 2.0 | MIT |
| Neovim | 0.11+ | 0.12+ |
| Server install | Downloads the bundle on first start; or `server_path` | `:IntellijServerInstall` (lazy `build` step); needs `curl` and `unzip` |
| Licence setup | `accept_eula = true` | Reads a JetBrains Account sign-in from the IDE or Toolbox; else `:IntellijServerLicense` |
| JDK for analysis | `default_sdk` option | Bundled runtime; project JDK from the build |
| Lazy loading | Must load at startup, not on `ft` | `ft = { "java", "kotlin" }` |
| Extras | nvim-dap run and debug, git views, file templates, decompiler, `:checkhealth intellij-lsp` | nvim-dap, run and debug code lenses, shared server over a socket, External Libraries in oil.nvim or snacks, IntelliJ code-style settings |

Minimal lazy.nvim specs from the READMEs:

```lua
{
  'JetBrains/intellij-lsp.nvim',
  dependencies = { 'mfussenegger/nvim-dap' }, -- optional: run and debug
  opts = { accept_eula = true },
}
```

```lua
{
  "AlexandrosAlexiou/intellij-server.nvim",
  ft = { "java", "kotlin" },
  dependencies = { "mfussenegger/nvim-dap" }, -- optional
  build = ":IntellijServerInstall",
  opts = {},
}
```

## Current setup in the dotfiles repo

- Java: `jdtls`, installed by Mason (`config/nvim/lua/plugins/mason.lua`) and set up as `jdtls = {}` in `config/nvim/lua/plugins/lsp.lua`.
- Kotlin: `kotlin.nvim` (`config/nvim/lua/plugins/kotlin.lua`) with the `kotlin-lsp` Homebrew cask, version `262.8190.0` at the last update. kotlin.nvim v2 already starts `bin/intellij-server` from the cask. `kotlin.lua` holds detection code for the cask layout, `KOTLIN_LSP_DIR` in `~/.zshrc_machine`, a startup check and a shutdown handler.
- Docs: `config/nvim/doc/kotlin-lsp-setup.md`, `config/nvim/doc/CLAUDE.md` (language table).
- Tools: `bin/lsp-doctor` finds and cleans dangling kotlin-lsp processes.
- Claude Code: `doc/claude-lsp-integration.md` covers Claude Code's own LSP plugins. The intellij-server licence does not cover that use yet.

## Inline suggestions of whole lines and next edits

Gathered 2026-10-28 from search results and JetBrains help pages. Not verified against the 1.0 extension itself.

Jim saw intelligent suggestions of the next code in IntelliJ IDEA. Two separate IDE features can produce them. Neither is listed as a feature of the LSP or the VS Code extension.

- Full Line Code Completion suggests whole lines, and sometimes several, as grey inline text. It runs a local deep learning model and sends no code over the internet. The IntelliJ IDEA Ultimate subscription includes it. The plugin is bundled and enabled by default, with bundled Java and Kotlin models. It needs a 64-bit processor, or an x86 processor with AVX2. It does not run in Remote Development.
- Next edit suggestions belong to the AI Assistant plugin, not Full Line. They suggest the next edit to the code. The models are cloud (Gemini, OpenAI, Anthropic) or a custom local model. Whether they ship with Ultimate, and whether they can run locally, is not established.
- The extension's official feature list names completion and signature help. Full Line is not mentioned there. In Neovim, the LSP gives completion items and signature help only. Inline whole-line suggestions need a separate plugin.
- To tell which feature was in use: check Settings, Plugins, Installed for "Full Line Code Completion", "AI Assistant" and "GitHub Copilot". Grey inline text with no popup points to Full Line or next edit suggestions.

## Sources

- [IntelliJ IDEA's Java and Kotlin for Cursor and VS Code Hits 1.0](https://blog.jetbrains.com/idea/2026/10/intellij-idea-s-java-and-kotlin-for-cursor-and-vs-code-hits-1-0/)
- [IntelliJ IDEA Goes LSP](https://blog.jetbrains.com/idea/2026/08/intellij-idea-goes-lsp/)
- [About Java and Kotlin by IntelliJ IDEA](https://www.jetbrains.com/help/intellij-vscode/About-instance.html)
- [Open VSX: Java and Kotlin by IntelliJ IDEA](https://open-vsx.org/extension/JetBrains/intellij-server)
- [JetBrains/intellij-lsp.nvim](https://github.com/JetBrains/intellij-lsp.nvim)
- [AlexandrosAlexiou/intellij-server.nvim](https://github.com/AlexandrosAlexiou/intellij-server.nvim)
- [Kotlin/kotlin-lsp](https://github.com/Kotlin/kotlin-lsp)
- [Buy IntelliJ IDEA](https://www.jetbrains.com/idea/buy/)
- [JetBrains: Subscription options and pricing](https://jetbrains.com/buy)
- [Capterra: IntelliJ IDEA pricing](https://www.capterra.com/p/136010/IntelliJ-IDEA/pricing/)
- [heise.de: Price jump for development environments from October 1](https://heise.de/-10506695)
- [Full Line code completion](https://jetbrains.com/help/idea/full-line-code-completion.html)
- [Full Line Code Completion in JetBrains IDEs: All You Need to Know](https://blog.jetbrains.com/?p=458703/)
- [IntelliJ IDEA 2026.2 Help: AI Assistant](https://www.jetbrains.com.cn/en-us/help/idea/ai-assistant.html)
