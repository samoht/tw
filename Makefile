# Claude Code unattended test-fixing loop
#
# Usage:
#   make login                        # One-time: authenticate with Max plan
#   make check-key                    # Verify auth works
#   make fix TEST=examples/prose      # Fix until tests pass
#   make test TEST=examples           # Run tests once
#   make shell                        # Debug shell
#   make clean                        # Remove image and auth volume

IMAGE_NAME := tw-claude
MODEL := sonnet
TEST ?=
CLAUDE_HOME := $(HOME)/.claude-container

export IMAGE_NAME CLAUDE_HOME

RUN := ./scripts/docker-run.sh

.PHONY: build test fix shell clean check-key login

build:
	docker build $(if $(NOCACHE),--no-cache,) -t $(IMAGE_NAME) .

test: build
	$(RUN) --no-network -- opam exec -- dune test --force $(TEST)

fix: build
	@test_cmd="opam exec -- dune test --force $(TEST)"; \
	cont=""; attempt=0; \
	while true; do \
		attempt=$$((attempt + 1)); \
		echo "=== Attempt $$attempt ==="; \
		$(RUN) --claude -- claude $$cont -p "Run '$$test_cmd' and fix any failures." \
			--dangerously-skip-permissions --model $(MODEL) --verbose; \
		cont="--continue"; \
		echo "=== Checking ==="; \
		if $(RUN) --no-network -- $$test_cmd; then \
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
