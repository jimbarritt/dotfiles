.PHONY: help install test link link-dry-run

# Color codes
GREEN := \033[32m
RED := \033[31m
RESET := \033[0m

help:
	@./do.sh help

install:
	@./do.sh install

test:
	@if shellcheck do.sh; then \
		echo "$(GREEN)[OK] do.sh passed shellcheck$(RESET)"; \
	else \
		echo "$(RED)[!!] shellcheck found issues$(RESET)"; \
	fi
