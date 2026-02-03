# Claude Code unattended test-fixing loop
#
# Usage:
#   make login                        # One-time: authenticate with Max plan
#   make check-key                    # Verify auth works
#   make fix TEST=examples/prose      # Fix until tests pass
#   make test TEST=examples           # Run tests once
#   make shell                        # Debug shell
#   make clean                        # Remove image and auth volume
#
# Customization:
#   make fix TEST=examples/prose PROMPT="..." # Custom prompt
#   make fix TEST=examples/accessibility MODEL=opus # Use opus model

IMAGE_NAME := tw-claude
MODEL := sonnet
TEST ?=
CLAUDE_HOME := $(HOME)/.claude-container

# Test command (computed from TEST)
TEST_CMD = opam exec -- dune test --force $(TEST)

# Default prompt - can be overridden with PROMPT="..."
# Note: Cosmetic formatting differences ARE real differences and MUST be fixed.
# Priority: Fix cssdiff/parser bugs BEFORE fixing utility implementation bugs.
define DEFAULT_PROMPT
Run '$(TEST_CMD)' and fix any failures.

CRITICAL PRIORITIES (in order):
1. If cssdiff reports "No structural differences" but there ARE actual CSS differences, STOP and fix the cssdiff tool in lib/tools/css_compare.ml FIRST.
2. If the CSS parser fails to parse valid CSS syntax, fix the parser in lib/css/properties.ml FIRST.
3. Cosmetic formatting differences (whitespace, nesting format, property order) ARE structural differences and MUST be fixed to match Tailwind exactly.

See CLAUDE.md section 12 for cssdiff bug details.
endef
PROMPT ?= $(subst $(newline), ,$(DEFAULT_PROMPT))

# Newline for prompt formatting
define newline


endef

export IMAGE_NAME CLAUDE_HOME

RUN := ./scripts/docker-run.sh

.PHONY: build test fix shell clean check-key login

build:
	docker build $(if $(NOCACHE),--no-cache,) -t $(IMAGE_NAME) .

test: build
	$(RUN) --no-network -- $(TEST_CMD)

fix: build
	@cont=""; attempt=0; \
	while true; do \
		attempt=$$((attempt + 1)); \
		echo "=== Attempt $$attempt ==="; \
		$(RUN) --claude -- claude $$cont -p "$(PROMPT)" \
			--dangerously-skip-permissions --model $(MODEL) --verbose; \
		cont="--continue"; \
		echo "=== Checking ==="; \
		if $(RUN) --no-network -- $(TEST_CMD); then \
			echo "All tests passed after $$attempt attempts!"; \
			exit 0; \
		fi; \
	done

shell: build
	$(RUN) --claude --interactive -- bash

check-key: build
	$(RUN) --claude --interactive -- claude -p "OK" --max-turns 1 --model haiku --verbose

login: build
	@mkdir -p $(CLAUDE_HOME)
	@test -f $(CLAUDE_HOME).json || echo '{}' > $(CLAUDE_HOME).json
	@echo "Run /login inside Claude, authenticate with Max plan, then Ctrl-D"
	$(RUN) --claude --interactive -- claude

clean:
	docker rmi $(IMAGE_NAME) 2>/dev/null || true
