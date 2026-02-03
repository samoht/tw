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
# Auth: run `make login` once, persists in ~/.claude-container
# Security: git hosts blocked, tmpfs shadows host node_modules and _build

IMAGE_NAME := tw-claude
MODEL := sonnet
TEST ?=

# Mount container-specific Claude config (run `make login` once to authenticate)
CLAUDE_HOME := $(HOME)/.claude-container
DOCKER_OPTS := \
	-v $(CLAUDE_HOME):/home/opam/.claude \
	-v $(CLAUDE_HOME).json:/home/opam/.claude.json \
	-e GIT_TERMINAL_PROMPT=0 \
	-e GIT_ASKPASS=/bin/false \
	-e GIT_SSH_COMMAND=/bin/false \
	--add-host=github.com:127.0.0.1 \
	--add-host=gitlab.com:127.0.0.1 \
	--add-host=bitbucket.org:127.0.0.1

.PHONY: build test fix shell clean check-key login

# Build the container (use `make build NOCACHE=1` to force rebuild)
build:
	docker build $(if $(NOCACHE),--no-cache,) -t $(IMAGE_NAME) .

# Run tests once (no network needed)
test: build
	docker run --rm --network none \
		-v $(PWD):/work \
		--mount type=tmpfs,destination=/work/node_modules,uid=1000,gid=1000 \
		--mount type=tmpfs,destination=/work/_build,uid=1000,gid=1000 \
		$(IMAGE_NAME) \
		opam exec -- dune test --force $(TEST)

# Loop until tests pass (no limit)
fix: build
	@set -x; \
	test_cmd="opam exec -- dune test --force $(TEST)"; \
	attempt=0; \
	while true; do \
		attempt=$$((attempt + 1)); \
		echo "=== Attempt $$attempt ==="; \
		if [ $$attempt -eq 1 ]; then \
			docker run --rm -t \
				$(DOCKER_OPTS) \
				-v $(PWD):/work \
				--mount type=tmpfs,destination=/work/node_modules \
				--mount type=tmpfs,destination=/work/_build \
				$(IMAGE_NAME) \
				claude -p "Run '$$test_cmd' and fix any failures. Be concise." \
					--dangerously-skip-permissions \
					--model $(MODEL) \
					--verbose; \
		else \
			docker run --rm -t \
				$(DOCKER_OPTS) \
				-v $(PWD):/work \
				--mount type=tmpfs,destination=/work/node_modules \
				--mount type=tmpfs,destination=/work/_build \
				$(IMAGE_NAME) \
				claude -p "Continue fixing test failures. Run '$$test_cmd' to check." \
					--continue \
					--dangerously-skip-permissions \
					--model $(MODEL) \
					--verbose; \
		fi; \
		echo "=== Checking if tests pass ==="; \
		test_output=$$(docker run --rm --network none \
			-v $(PWD):/work \
			--mount type=tmpfs,destination=/work/node_modules,uid=1000,gid=1000 \
			--mount type=tmpfs,destination=/work/_build,uid=1000,gid=1000 \
			$(IMAGE_NAME) \
			$$test_cmd 2>&1); \
		test_exit=$$?; \
		echo "$$test_output"; \
		echo "=== Test exit code: $$test_exit ==="; \
		if [ $$test_exit -eq 0 ]; then \
			echo "All tests passed after $$attempt attempts!"; \
			exit 0; \
		fi; \
	done

# Interactive shell for debugging
shell: build
	docker run --rm -it \
		$(DOCKER_OPTS) \
		-v $(PWD):/work \
		--mount type=tmpfs,destination=/work/node_modules \
		--mount type=tmpfs,destination=/work/_build \
		$(IMAGE_NAME) \
		bash

# Smoke test: verify Claude auth works
check-key: build
	docker run --rm -it \
		$(DOCKER_OPTS) \
		$(IMAGE_NAME) \
		claude -p "OK" --max-turns 1 --model haiku --verbose

# Login to Claude (one-time: run /login inside, then Ctrl-D to exit)
login: build
	@mkdir -p $(CLAUDE_HOME)
	@test -f $(CLAUDE_HOME).json || echo '{}' > $(CLAUDE_HOME).json
	@echo "Run /login inside Claude, authenticate with Max plan, then Ctrl-D"
	docker run --rm -it \
		-v $(CLAUDE_HOME):/home/opam/.claude \
		-v $(CLAUDE_HOME).json:/home/opam/.claude.json \
		$(IMAGE_NAME) \
		claude

# Clean up
clean:
	docker rmi $(IMAGE_NAME) 2>/dev/null || true
