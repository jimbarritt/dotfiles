2026-09-26T06-44-23Z-collaborative-markdown-editing-with-git-backed-history.v1.md
project: tsk
# Collaborative markdown editing with git-backed history

## Requirement
UI similar to Notion or Obsidian. Comment mode and suggestion mode, anchored to a word or line. Version and comment history stored alongside the markdown in git.

## Findings

No single product meets all three requirements. The closest options:

### CriticMarkup plugins for Obsidian
Comments and suggestions are written as inline markdown syntax in the file itself (for example `{>>comment<<}`, `{++insertion++}`). Git tracks this history for free, because the data is plain text in the file.

- **obsidian-criticmarkup** (Fevol) — suggestion mode, comment mode, vault-wide suggestion index.
- **agent-comments** (andrewroxby) — same CriticMarkup base, adds threaded replies, built with terminal agents in mind.

Limitation: the editor is Obsidian's own interface with an overlay, not a Notion-style UI.

### CollabMD
Repo: https://github.com/andes90/collabmd

Points at an existing git-backed markdown folder or Obsidian-style vault and adds a real-time collaborative web UI: file tree, live preview, presence, source-anchored comments. Watches the filesystem and reconciles external git changes back into the live session.

- Closer match on UI quality.
- The comment and history storage model (what lands in git versus a side database) is not fully confirmed from the search result. Check the repo directly.

### HackMD / HedgeDoc
Real-time collaborative markdown editor with comments. The document lives in HackMD's own database. Git involvement is push/pull to GitHub, not the live storage layer.

Rules this out if git must be the source of truth for comment history, not just a mirror target.

## Take
If git-native comment history is the hard requirement, CriticMarkup is the safer choice, because the comment data is plain text under version control by construction. If UI quality matters more and the CollabMD storage model turns out to be acceptable, that is worth a closer look.
