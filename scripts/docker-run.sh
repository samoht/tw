#!/bin/bash
# Docker run helper for tw-claude container
# Usage: docker-run.sh [--claude] [--network] [--interactive] -- <command...>

set -e

IMAGE_NAME="${IMAGE_NAME:-tw-claude}"
CLAUDE_HOME="${CLAUDE_HOME:-$HOME/.claude-container}"

# Base options
opts=(--rm)
run_opts=(
    -v "$(pwd):/work"
    --tmpfs /work/node_modules:mode=1777,size=256m
    --tmpfs /work/_build:exec,mode=1777,size=512m
)

# Parse flags
while [[ $# -gt 0 ]]; do
    case $1 in
        --claude)
            opts+=(-t --init)
            run_opts+=(
                -v "$CLAUDE_HOME:/home/opam/.claude"
                -v "$CLAUDE_HOME.json:/home/opam/.claude.json"
                -e GIT_TERMINAL_PROMPT=0
                -e GIT_ASKPASS=/bin/false
                -e GIT_SSH_COMMAND=/bin/false
                --add-host=github.com:127.0.0.1
                --add-host=gitlab.com:127.0.0.1
                --add-host=bitbucket.org:127.0.0.1
            )
            shift
            ;;
        --no-network)
            opts+=(--network none)
            shift
            ;;
        --interactive)
            opts+=(-it)
            shift
            ;;
        --)
            shift
            break
            ;;
        *)
            break
            ;;
    esac
done

exec docker run "${opts[@]}" "${run_opts[@]}" "$IMAGE_NAME" "$@"
