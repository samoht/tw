# Claude Code unattended test-fixing loop
#
# Usage:
#   make login                        # One-time: authenticate with Max plan
#   make check-key                    # Verify auth works
#   make fix TEST=examples/prose      # Fix until tests pass
#   make fix-upstream                  # Fix upstream tests one by one
#   make fix-upstream N=42             # Fix a specific upstream test
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

# Fix upstream tests one at a time
# Usage:
#   make fix-upstream              # Fix all failing upstream tests
#   make fix-upstream N=42         # Fix a specific test number
define UPSTREAM_PROMPT
Fix the failing upstream Tailwind CSS test.

Run: dune exec test/upstream/test.exe test utilities $(N)

This runs a SINGLE upstream test extracted from the tailwindcss repository.
It compares our tw output (inline mode, no layers) against the expected CSS
from Tailwind's own test snapshots.

STEP 1 - UNDERSTAND THE FAILURE:
- Read the test output carefully: it shows expected vs actual CSS
- Check what classes are being tested
- Run: dune exec -- tw -s "<class>" to see what we generate
- The expected CSS comes from https://github.com/tailwindlabs/tailwindcss
  (~/git/tailwindcss/packages/tailwindcss/src/utilities.test.ts)

STEP 2 - CHECK UPSTREAM FOR INSPIRATION:
- Look at how Tailwind implements it: ~/git/tailwindcss/packages/tailwindcss/src/
- Key files: utilities.ts, theme.ts, property-order.ts
- Understand their LOGIC and APPROACH, not the code
- NEVER copy TypeScript code - translate concepts to typed OCaml
- Use their implementation to understand edge cases and expected behavior

STEP 3 - IDENTIFY ROOT CAUSE:
- Is it a missing utility? -> Add it in the right lib/<area>.ml
- Is it wrong CSS output? -> Fix the utility implementation
- Is it wrong property values? -> Check the variable/theme system
- Is it a parsing issue? -> Fix lib/css/ parser
- Is it a comparison tool bug? -> Fix lib/tools/css_compare.ml FIRST

STEP 4 - FIX THE CORRECT WAY:
- Use typed Var and constructors - no raw strings
- Place declarations in the correct layer
- Add types when needed - let the compiler guide you
- Verify fix: dune exec test/upstream/test.exe test utilities $(N)

FORBIDDEN - NEVER say:
- "This would require significant time"
- "This is complex and needs more work"
- "Leave this for later"
You have UNLIMITED time. Fix it correctly.
endef
UPSTREAM_PROMPT_TEXT = $(subst $(newline), ,$(UPSTREAM_PROMPT))

UPSTREAM_TEST = ./_build/default/test/upstream/test.exe

fix-upstream:
	@dune build test/upstream/test.exe
ifndef N
	@echo "Finding first failing upstream test..."; \
	cont=""; \
	while true; do \
		fail=$$($(UPSTREAM_TEST) 2>&1 \
			| grep '\[FAIL\]' | head -1 \
			| sed 's/.*utilities[[:space:]]*//' | sed 's/[[:space:]].*//'); \
		if [ -z "$$fail" ]; then \
			echo "All upstream tests pass!"; \
			exit 0; \
		fi; \
		echo "=== Fixing test $$fail ==="; \
		$(RUN) --claude -- claude $$cont -p "Fix the failing upstream test. Run: dune exec test/upstream/test.exe test utilities $$fail $(UPSTREAM_PROMPT_TEXT)" \
			--dangerously-skip-permissions --model $(MODEL) --verbose; \
		cont="--continue"; \
		dune build test/upstream/test.exe; \
		echo "=== Checking test $$fail ==="; \
		if ! $(UPSTREAM_TEST) test utilities $$fail 2>&1; then \
			echo "Test $$fail still failing, retrying..."; \
			continue; \
		fi; \
		echo "Test $$fail fixed! Finding next failure..."; \
		cont=""; \
	done
else
	@cont=""; attempt=0; \
	while true; do \
		attempt=$$((attempt + 1)); \
		echo "=== Attempt $$attempt for test $(N) ==="; \
		$(RUN) --claude -- claude $$cont -p "Fix the failing upstream test. Run: dune exec test/upstream/test.exe test utilities $(N) $(UPSTREAM_PROMPT_TEXT)" \
			--dangerously-skip-permissions --model $(MODEL) --verbose; \
		cont="--continue"; \
		dune build test/upstream/test.exe; \
		echo "=== Checking test $(N) ==="; \
		if $(UPSTREAM_TEST) test utilities $(N); then \
			echo "Test $(N) fixed after $$attempt attempts!"; \
			exit 0; \
		fi; \
	done
endif

clean:
	docker rmi $(IMAGE_NAME) 2>/dev/null || true
