# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This project provides a Docker-based sandbox for running Claude Code with `--dangerously-skip-permissions` in isolation. It consists of two main components:

- **claude-sandbox**: A bash script (~425 lines) that handles argument parsing, Docker image building, and container orchestration
- **Dockerfile**: Defines the container environment (Node.js 22, Claude Code, dev tools)

## Build and Test Commands

```bash
# Build the Docker image
./claude-sandbox --build

# Run in current git repository (auto-mounts)
./claude-sandbox

# Test with a remote repository
./claude-sandbox https://github.com/user/repo

# Test with explicit mount
./claude-sandbox -m /path/to/repo
```

There are no automated tests - validation is done by running the sandbox with various flag combinations.

## Architecture

The `claude-sandbox` script follows this flow:
1. `parse_args()` - CLI argument processing
2. `check_prerequisites()` - Docker availability and auth method detection (Max subscription vs API key)
3. `build_image()` - Conditional Docker image build
4. `cleanup_container()` - Remove any existing container with the same name
5. `run_container()` - Orchestrate the Docker run with appropriate mounts and environment

Key design decisions:
- Auth via `setup-token` stored in 1Password (or `CLAUDE_CODE_OAUTH_TOKEN` env var)
- Creates `~/.claude.json` with `hasCompletedOnboarding:true` to skip onboarding prompts
- Uses consistent hostname `claude-sandbox` for token binding
- Without `-m`, repos are copied (isolated); with `-m`, repos are mounted (changes persist)
- User config (`~/.claude/`) is mounted to preserve settings, history, and plugins
