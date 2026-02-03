# Claude Code unattended test-fixing container
# Usage: see Makefile targets

FROM ocaml/opam:ubuntu-24.04-ocaml-5.3

# Install system dependencies
USER root
RUN apt-get update && apt-get install -y \
    curl \
    git \
    nodejs \
    npm \
    jq \
    && rm -rf /var/lib/apt/lists/*

# Install Claude Code CLI
RUN npm install -g @anthropic-ai/claude-code

# Switch back to opam user
USER opam
WORKDIR /work

# Install OCaml dependencies (including test deps and missing ones)
COPY --chown=opam:opam *.opam dune-project ./
RUN opam install . --deps-only --with-test --yes && \
    opam install dune-build-info fmt ocaml-crunch --yes

# Copy source
COPY --chown=opam:opam . .

# Build the project
RUN opam exec -- dune build

# Default: run tests
CMD ["opam", "exec", "--", "dune", "test"]
