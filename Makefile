.PHONY: help install test link link-dry-run

.DEFAULT_GOAL := help

# Color codes
GREEN := \033[32m
RED := \033[31m
RESET := \033[0m

help: ## Display this help
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n\nTargets:\n"} \
		/^[a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2 } \
		/^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) }' $(MAKEFILE_LIST)

##@ Fresh machine setup

install: ## Install preququisites and git hooks
	@./do.sh install

link: ## Link all dotfiles into your home dir
	@./do.sh link 

link-dry-run: ## Do a dry run to show you what would happen for linking
	@./do.sh link --dry-run

##@ Testing 

test:  ## Check all the scripts pass shellcheck
	@shellcheck do.sh
	@echo "$(GREEN)[OK] do.sh passed shellcheck$(RESET)"
	@shellcheck hooks/pre-commit
	@echo "$(GREEN)[OK] hooks/pre-commit passed shellcheck$(RESET)"
