set shell := ["sh", "-c"]

# List available recipes
default:
    @just --list

# Install prerequisites and git hooks
install:
    ./do.sh install

# Link all dotfiles into your home dir (pass --dry-run to preview)
link *args:
    ./do.sh link {{args}}

# Check all the scripts pass shellcheck
test:
    @shellcheck do.sh && echo "\033[32m[OK] do.sh passed shellcheck\033[0m"
    @shellcheck hooks/pre-commit && echo "\033[32m[OK] hooks/pre-commit passed shellcheck\033[0m"
