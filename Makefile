# Claude Code unattended test-fixing loop
#
# Usage:
#   make login                        # One-time: authenticate with Max plan
#   make check-key                    # Verify auth works
#   make fix TEST=examples/prose      # Fix until tests pass
#   make test TEST=examples           # Run tests once
#   make lint                         # Run merlint + fix issues with Claude
#   make lint-check                   # Just show lint issues (no fix)
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
define DEFAULT_PROMPT
Run '$(TEST_CMD)' and fix any failures.

STEP 1 - DIAGNOSE FIRST (before changing ANY code):
- Run: dune exec -- tw -s "failing-utility" --diff
- This shows EXACTLY what differs between our output and Tailwind
- Run: dune exec -- tw -s "failing-utility" --tailwind
- This shows what Tailwind actually outputs
- Understand the EXACT difference before touching code

STEP 2 - IDENTIFY ROOT CAUSE:
- Is it a cssdiff bug? (reports "no differences" but there are differences)
  -> Fix lib/tools/css_compare.ml or lib/tools/tree_diff.ml FIRST
- Is it a parser bug? (fails to parse valid CSS)
  -> Fix lib/css/properties.ml FIRST
- Is it wrong ordering? (rules in wrong position)
  -> Fix comparison functions in lib/rules.ml
- Is it wrong values? (property values differ)
  -> Fix the utility implementation

STEP 3 - FIX THE CORRECT WAY:
- Do NOT hack around problems
- Understand and fix root causes
- If ordering is wrong, fix comparison functions properly
- If rules are missing, add them to the right place
- When in doubt, ADD MORE TYPES - let the compiler guide you

FORBIDDEN - NEVER say:
- "This would require significant time"
- "This is complex and needs more work"
- "Leave this for later"
You have UNLIMITED time. Fix EVERYTHING correctly.
endef
PROMPT ?= $(subst $(newline), ,$(DEFAULT_PROMPT))

# Newline for prompt formatting
define newline


endef

export IMAGE_NAME CLAUDE_HOME

RUN := ./scripts/docker-run.sh

.PHONY: build test fix shell clean check-key login lint lint-check

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

lint-check: build
	@echo "Checking for lint issues..."
	$(RUN) --no-network -- opam exec -- merlint lib/ --exclude lib/css/examples/

lint: build
	@echo "Running merlint and fixing issues..."
	@cont=""; attempt=0; \
	while true; do \
		attempt=$$((attempt + 1)); \
		echo "=== Lint attempt $$attempt ==="; \
		$(RUN) --claude -- claude $$cont -p "Run 'opam exec -- merlint lib/ --exclude lib/css/examples/' and fix ALL reported issues. YOU MUST FIX EVERY LINT ISSUE. No excuses about refactoring, API changes, complexity, or time - just fix them correctly. Add types, extract functions, do the refactoring properly. Do NOT run tests. Do NOT mention tests. ONLY fix merlint issues." \
			--dangerously-skip-permissions --model $(MODEL) --verbose; \
		cont="--continue"; \
		echo "=== Checking for remaining issues ==="; \
		if $(RUN) --no-network -- opam exec -- merlint lib/ --exclude lib/css/examples/ --quiet; then \
			echo "All lint issues fixed after $$attempt attempts!"; \
			exit 0; \
		fi; \
	done

clean:
	docker rmi $(IMAGE_NAME) 2>/dev/null || true
