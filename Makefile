# Claude Code unattended test-fixing loop
#
# Usage:
#   make build                        # Build the container
#   make test                         # Run all tests once
#   make test TEST=examples           # Run specific tests
#   make fix TEST=examples            # Fix until 'dune test examples' passes
#   make fix TEST=examples/prose      # Fix until 'dune test examples/prose' passes
#
# API key: create .env with ANTHROPIC_API_KEY=sk-...
#
# Security: Network restricted to api.anthropic.com only, git push disabled

-include .env
export ANTHROPIC_API_KEY

IMAGE_NAME := tw-claude
MODEL := sonnet
TEST ?=

# Block git push/fetch and restrict network
SECURITY_OPTS := \
	-e GIT_TERMINAL_PROMPT=0 \
	-e GIT_ASKPASS=/bin/false \
	-e GIT_SSH_COMMAND=/bin/false \
	--add-host=github.com:127.0.0.1 \
	--add-host=gitlab.com:127.0.0.1 \
	--add-host=bitbucket.org:127.0.0.1

.PHONY: build test fix shell clean

# Build the container
build:
	docker build -t $(IMAGE_NAME) .

# Run tests once (no network needed)
test: build
	docker run --rm --network none -v $(PWD):/work $(IMAGE_NAME) \
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
				$(SECURITY_OPTS) \
				-e ANTHROPIC_API_KEY \
				-v $(PWD):/work \
				$(IMAGE_NAME) \
				claude -p "Run '$$test_cmd' and fix any failures. Be concise." \
					--dangerously-skip-permissions \
					--model $(MODEL); \
		else \
			docker run --rm \
				$(SECURITY_OPTS) \
				-e ANTHROPIC_API_KEY \
				-v $(PWD):/work \
				$(IMAGE_NAME) \
				claude -p "Continue fixing test failures. Run '$$test_cmd' to check." \
					--continue \
					--dangerously-skip-permissions \
					--model $(MODEL); \
		fi; \
		echo "=== Checking if tests pass ==="; \
		if docker run --rm --network none -v $(PWD):/work $(IMAGE_NAME) \
			$$test_cmd 2>&1; then \
			echo "All tests passed after $$attempt attempts!"; \
			exit 0; \
		fi; \
	done

# Interactive shell for debugging (with security opts)
shell: build
	docker run --rm -it \
		$(SECURITY_OPTS) \
		-e ANTHROPIC_API_KEY \
		-v $(PWD):/work \
		$(IMAGE_NAME) \
		bash

# Clean up
clean:
	docker rmi $(IMAGE_NAME) 2>/dev/null || true
