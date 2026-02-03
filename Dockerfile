# Claude Code unattended test-fixing container
# Usage: see Makefile targets

FROM ocaml/opam:ubuntu-24.04-ocaml-5.3

# Install system dependencies (including build tools for native npm modules)
USER root
RUN apt-get update && apt-get install -y \
    curl \
    git \
    jq \
    build-essential \
    python3 \
    && rm -rf /var/lib/apt/lists/*

# Install Node.js 20.x
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - && \
    apt-get install -y nodejs

# Install npm packages globally (Claude Code + Tailwind CSS)
RUN npm install -g @anthropic-ai/claude-code \
    @tailwindcss/cli tailwindcss @tailwindcss/forms @tailwindcss/typography

# Set NODE_PATH so tailwindcss can find its modules
ENV NODE_PATH=/usr/lib/node_modules

# Fix npm cache ownership for opam user
RUN mkdir -p /home/opam/.npm && chown -R opam:opam /home/opam/.npm

# Switch to opam user
USER opam
WORKDIR /work

# Install OCaml dependencies (cached unless opam files change)
COPY --chown=opam:opam *.opam dune-project ./
RUN opam install . --deps-only --with-test --yes

# Copy source and build
COPY --chown=opam:opam . .
RUN opam exec -- dune build

# Default: run tests
CMD ["opam", "exec", "--", "dune", "test"]
