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

CLAUDE_HOME := $(HOME)/.claude-container

# Common docker run options
RUN_OPTS := -v $(PWD):/work \
	--mount type=tmpfs,destination=/work/node_modules,tmpfs-mode=1777 \
	--mount type=tmpfs,destination=/work/_build,tmpfs-mode=1777

CLAUDE_OPTS := -v $(CLAUDE_HOME):/home/opam/.claude \
	-v $(CLAUDE_HOME).json:/home/opam/.claude.json \
	-e GIT_TERMINAL_PROMPT=0 -e GIT_ASKPASS=/bin/false -e GIT_SSH_COMMAND=/bin/false \
	--add-host=github.com:127.0.0.1 --add-host=gitlab.com:127.0.0.1 --add-host=bitbucket.org:127.0.0.1

.PHONY: build test fix shell clean check-key login

build:
	docker build $(if $(NOCACHE),--no-cache,) -t $(IMAGE_NAME) .

test: build
	docker run --rm --network none $(RUN_OPTS) $(IMAGE_NAME) \
		opam exec -- dune test --force $(TEST)

fix: build
	@test_cmd="opam exec -- dune test --force $(TEST)"; \
	cont=""; attempt=0; \
	while true; do \
		attempt=$$((attempt + 1)); \
		echo "=== Attempt $$attempt ==="; \
		docker run --rm -t $(CLAUDE_OPTS) $(RUN_OPTS) $(IMAGE_NAME) \
			claude $$cont -p "Run '$$test_cmd' and fix any failures." \
				--dangerously-skip-permissions --model $(MODEL) --verbose; \
		cont="--continue"; \
		echo "=== Checking ==="; \
		if docker run --rm --network none $(RUN_OPTS) $(IMAGE_NAME) $$test_cmd; then \
			echo "All tests passed after $$attempt attempts!"; \
			exit 0; \
		fi; \
	done

shell: build
	docker run --rm -it $(CLAUDE_OPTS) $(RUN_OPTS) $(IMAGE_NAME) bash

check-key: build
	docker run --rm -it $(CLAUDE_OPTS) $(IMAGE_NAME) \
		claude -p "OK" --max-turns 1 --model haiku --verbose

login: build
	@mkdir -p $(CLAUDE_HOME)
	@test -f $(CLAUDE_HOME).json || echo '{}' > $(CLAUDE_HOME).json
	@echo "Run /login inside Claude, authenticate with Max plan, then Ctrl-D"
	docker run --rm -it $(CLAUDE_OPTS) $(IMAGE_NAME) claude

clean:
	docker rmi $(IMAGE_NAME) 2>/dev/null || true
