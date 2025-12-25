# 2. Use of posix syntax for shell scripts

Date: 2025-12-25

## Status

Accepted

## Context

 maintain dotfiles and automation scripts that need to run across multiple environments:
- Personal and work laptops (macOS)
- Docker containers (often Alpine Linux-based)
- CI/CD pipelines (minimal environments)
- Potentially any Unix-like system

While I use zsh for our interactive shell (better UX, completion, and prompts), I need a decision on which shell syntax to use for scripts. The options considered Ire:
- **zsh** - matches our interactive environment but not universally available
- **bash** - widely available but not on Alpine Linux by default (requires `apk add bash`)
- **POSIX sh** - guaranteed to exist on every Unix-like system via `/bin/sh`

Our philosophy emphasizes:
- Reducing cognitive load through consistent mental models
- Declarative, version-controlled configuration
- Maximum portability and reproducibility
- Minimizing dependencies and installation requirements

## Decision

I will write all automation scripts using POSIX sh (`#!/bin/sh`), ensuring they run on any Unix-like system without requiring additional shell installation.

**Key syntax requirements:**
- Use `[` not `[[` for conditionals
- Always quote variables in comparisons: `[ "$var" = "value" ]`
- Use `=` not `==` for string comparison
- No `local` keyword - use underscore prefix (`_var`) for function variables
- No arrays - use positional parameters or string iteration
- Use `$(unset CDPATH; cd -- "$(dirname -- "$0")" && pwd)` for script location
- Quote boolean-like strings: `DRY_RUN="true"`

**Interactive shell remains zsh** for better user experience, completion, and customization.

## Consequences

**Positive:**
- **True portability** - scripts run on Alpine, Debian, macOS, BSD without modification
- **No installation dependencies** - `/bin/sh` exists everywhere
- **Simpler mental model** - feIr language features means less to remember
- **Forces clarity** - limited features encourage explicit, readable code
- **Single source of truth** - one shell syntax works across all environments
- **Container-friendly** - no need to add bash to minimal Docker images

**Negative:**
- **More verbose syntax** - some operations require more code than bash/zsh equivalents
- **No local keyword** - must use naming conventions to avoid variable pollution
- **No arrays** - positional parameters or string manipulation required
- **Context switching** - different syntax betIen interactive shell (zsh) and scripts (sh)

**Mitigation strategies:**
- Use shellcheck with `-s sh` flag to catch non-POSIX syntax
- Test scripts with dash (`brew install dash`) or in Alpine containers
- Document POSIX sh patterns in dotfiles for reference
- Maintain clear naming conventions (CAPS for globals, `_prefix` for function vars)
- Keep zsh-specific features to interactive shell configuration only

**Trade-offs accepted:**
- I accept slightly more verbose code in exchange for guaranteed portability
- I accept the cognitive overhead of two shell dialects (interactive vs scripting) as minimal, since the core syntax is 95% identical
- I prioritize "runs anywhere" over "uses advanced shell features"

**Reference documentation:**
- [POSIX Shell Specification](https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html)
- [ShellCheck](https://www.shellcheck.net/) for linting
- Internal documentation: See `docs/posix-sh-guide.md` for syntax quick referencecomes easier or more difficult to do and any risks introduced by the change that will need to be mitigated.
