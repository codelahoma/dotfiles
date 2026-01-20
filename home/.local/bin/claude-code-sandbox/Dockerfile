# Claude Code Sandbox Container
FROM node:22-bookworm

# Install system dependencies
RUN apt-get update && apt-get install -y \
    git \
    curl \
    openssh-client \
    ca-certificates \
    build-essential \
    python3 \
    python3-pip \
    ripgrep \
    fd-find \
    jq \
    && rm -rf /var/lib/apt/lists/*

# Install Claude Code globally
RUN npm install -g @anthropic-ai/claude-code

# Create non-root user for development
RUN useradd -m -s /bin/bash developer
USER developer
WORKDIR /home/developer

# Configure git defaults
RUN git config --global init.defaultBranch main \
    && git config --global user.email "developer@sandbox.local" \
    && git config --global user.name "Sandbox Developer"

# Create workspace and Claude config directories
RUN mkdir -p /home/developer/workspace /home/developer/.claude

WORKDIR /home/developer/workspace

# Default command
CMD ["claude", "--dangerously-skip-permissions"]
