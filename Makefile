# Claude Code unattended test-fixing loop
#
# Usage:
#   make build                        # Build the container
#   make test                         # Run all tests once
#   make test TEST=examples           # Run specific tests
#   make fix TEST=examples            # Fix until 'dune test examples' passes
#   make fix TEST=examples/prose      # Fix until 'dune test examples/prose' passes
#
# Required: ANTHROPIC_API_KEY environment variable

IMAGE_NAME := tw-claude
MODEL := sonnet
TEST ?=

.PHONY: build test fix shell clean

# Build the container
build:
	docker build -t $(IMAGE_NAME) .

# Run tests once
test: build
	docker run --rm -v $(PWD):/work $(IMAGE_NAME) \
		opam exec -- dune test $(TEST)

# Loop until tests pass (no limit)
fix: build
	@if [ -z "$$ANTHROPIC_API_KEY" ]; then \
		echo "Error: ANTHROPIC_API_KEY not set"; exit 1; \
	fi
	@test_cmd="opam exec -- dune test $(TEST)"; \
	attempt=0; \
	while true; do \
		attempt=$$((attempt + 1)); \
		echo "=== Attempt $$attempt ==="; \
		if [ $$attempt -eq 1 ]; then \
			docker run --rm \
				-e ANTHROPIC_API_KEY \
				-v $(PWD):/work \
				$(IMAGE_NAME) \
				claude -p "Run '$$test_cmd' and fix any failures. Be concise." \
					--dangerously-skip-permissions \
					--model $(MODEL); \
		else \
			docker run --rm \
				-e ANTHROPIC_API_KEY \
				-v $(PWD):/work \
				$(IMAGE_NAME) \
				claude -p "Continue fixing test failures. Run '$$test_cmd' to check." \
					--continue \
					--dangerously-skip-permissions \
					--model $(MODEL); \
		fi; \
		echo "=== Checking if tests pass ==="; \
		if docker run --rm -v $(PWD):/work $(IMAGE_NAME) \
			$$test_cmd 2>&1; then \
			echo "All tests passed after $$attempt attempts!"; \
			exit 0; \
		fi; \
	done

# Interactive shell for debugging
shell: build
	docker run --rm -it \
		-e ANTHROPIC_API_KEY \
		-v $(PWD):/work \
		$(IMAGE_NAME) \
		bash

# Clean up
clean:
	docker rmi $(IMAGE_NAME) 2>/dev/null || true
