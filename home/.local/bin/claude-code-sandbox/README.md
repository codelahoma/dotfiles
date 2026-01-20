# Claude Code Sandbox

Run Claude Code in a sandboxed Docker container with full permissions (`--dangerously-skip-permissions`) without putting your host system at risk.

## Why?

Claude Code's `--dangerously-skip-permissions` flag allows Claude to execute commands without confirmation, which is powerful but risky on your main system. This sandbox approach gives Claude full autonomy within an isolated container, so it can't accidentally (or intentionally) damage your host system.

## Claude Code Assets

The sandbox automatically handles Claude Code configuration files at both project and user levels:

### Project-Level (from your repository)

| Asset | Purpose |
|-------|---------|
| `.claude/` | Project settings, memory, conversation context |
| `CLAUDE.md` | Project-specific instructions for Claude |
| `.claudeignore` | Files/patterns for Claude to ignore |
| `claude.config.json` | Optional project configuration |

These are automatically included when you clone or mount a repository.

### User-Level (from `~/.claude/`)

| Asset | Purpose |
|-------|---------|
| `settings.json` | Your Claude Code preferences |
| `credentials` | API keys and authentication |

User config is mounted by default so Claude remembers your preferences. Use `--no-user-config` to start fresh.

## Installation

1. **Clone or copy this directory** to a location in your PATH:

```bash
# Option 1: Copy to ~/.local/bin
mkdir -p ~/.local/bin
cp -r claude-code-sandbox ~/.local/bin/
ln -s ~/.local/bin/claude-code-sandbox/claude-sandbox ~/.local/bin/claude-sandbox

# Option 2: Add this directory to your PATH
export PATH="$PATH:/path/to/claude-code-sandbox"
```

2. **Build the Docker image** (happens automatically on first run):

```bash
claude-sandbox --build
```

## Usage

### Basic Usage

```bash
# Run in current git repository (mounts it by default)
cd /path/to/your/repo
claude-sandbox

# Clone and work on a GitHub repository
claude-sandbox https://github.com/user/repo

# Work on a local repo without persisting changes
claude-sandbox /path/to/repo

# Mount a local repo (changes persist to your filesystem)
claude-sandbox -m /path/to/repo
```

### Options

| Option | Description |
|--------|-------------|
| `-h, --help` | Show help message |
| `-b, --build` | Force rebuild the Docker image |
| `-m, --mount` | Mount local repo (changes persist) |
| `-n, --name NAME` | Set container name (default: claude-sandbox) |
| `--no-skip-perms` | Don't use `--dangerously-skip-permissions` |
| `--no-user-config` | Don't mount `~/.claude` user configuration |

### Examples

```bash
# Work on your current project with full Claude autonomy
cd ~/projects/my-app
claude-sandbox

# Experiment with a fork without affecting your local copy
claude-sandbox https://github.com/someuser/interesting-project

# Run multiple sandboxes for different projects
claude-sandbox -n project-a https://github.com/user/project-a
claude-sandbox -n project-b https://github.com/user/project-b

# Use Claude Code normally (with permission prompts)
claude-sandbox --no-skip-perms

# Fresh start without your usual Claude preferences
claude-sandbox --no-user-config
```

## Authentication

The sandbox requires a long-lived OAuth token generated via `claude setup-token`. This is stored in 1Password for secure retrieval.

### First-time Setup

1. Generate a container token inside a sandbox with consistent hostname:
   ```bash
   docker run -it --rm --hostname claude-sandbox claude-code-sandbox bash
   claude setup-token  # Follow browser auth flow
   # Copy the sk-ant-oat01-... token
   ```

2. Save the token to 1Password:
   - Item name: `Claude Code Container Token`
   - Field: `credential`

3. Install the 1Password CLI (`op`) and sign in

Now `claude-sandbox` will automatically retrieve the token from 1Password.

### Alternative: Environment Variable

You can also set the token directly:
```bash
export CLAUDE_CODE_OAUTH_TOKEN="sk-ant-oat01-..."
claude-sandbox
```

## Environment Variables

| Variable | Required | Description |
|----------|----------|-------------|
| `GITHUB_TOKEN` | No | For cloning private repositories |
| `SSH_AUTH_SOCK` | No | SSH agent socket (auto-detected) |

## How It Works

1. **Builds a Docker image** with Node.js, Claude Code, and common development tools
2. **Clones or mounts** your repository into the container
3. **Mounts user config** from `~/.claude` (unless `--no-user-config`)
4. **Preserves project assets** like `.claude/` and `CLAUDE.md`
5. **Starts Claude Code** with `--dangerously-skip-permissions`
6. **Isolates all changes** within the container (unless using `-m`)

### Container Contents

The container includes:
- Node.js 22 (Debian Bookworm)
- Claude Code (latest)
- Git, curl, jq
- Python 3
- ripgrep, fd-find
- Build essentials

### Security Model

- **Without `-m`**: Repository is copied into the container. All changes are discarded when the container exits.
- **With `-m`**: Repository is mounted. Changes persist to your host filesystem.
- **User config**: `~/.claude` is mounted read-write by default (your preferences persist)
- **Network**: Container has full network access (required for Claude Code API calls)
- **SSH/Git**: Your SSH keys are mounted read-only for git operations

## Customization

### Adding Tools to the Container

Edit the `Dockerfile` to add additional tools:

```dockerfile
RUN apt-get update && apt-get install -y \
    your-package-here \
    && rm -rf /var/lib/apt/lists/*
```

Then rebuild:

```bash
claude-sandbox --build
```

### Project-Specific CLAUDE.md

Create a `CLAUDE.md` in your repository root to give Claude project-specific instructions:

```markdown
# Project Instructions

This is a Django application with a React frontend.

## Key Conventions
- Use pytest for all tests
- Follow PEP 8 style
- React components go in src/components/

## Commands
- `make test` - Run all tests
- `make dev` - Start development server
```

## Troubleshooting

### "Permission denied" errors

Make sure the script is executable:
```bash
chmod +x claude-sandbox
```

### Docker daemon not running

Start Docker:
```bash
# macOS
open -a Docker

# Linux
sudo systemctl start docker
```

### Private repository access

Set up GitHub token:
```bash
export GITHUB_TOKEN="ghp_your_token_here"
```

Or use SSH (agent will be forwarded):
```bash
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519
claude-sandbox git@github.com:user/private-repo
```

### Claude doesn't see my project settings

Ensure your `.claude/` directory and `CLAUDE.md` are not in `.gitignore`. The sandbox will report which Claude assets it finds:

```
[INFO] Found Claude Code assets: .claude CLAUDE.md
```

## Advanced

### Using an API Key Instead of Subscription

If you prefer to use API billing instead of your subscription, you can explicitly opt in:

```bash
export ANTHROPIC_API_KEY="your-api-key-here"
claude-sandbox --use-api-key
```

The `--use-api-key` flag is required even when `ANTHROPIC_API_KEY` is set. This prevents accidentally burning API credits when you have the variable set for other tools.

| Option | Description |
|--------|-------------|
| `--use-api-key` | Use `ANTHROPIC_API_KEY` instead of subscription |

| Variable | Description |
|----------|-------------|
| `ANTHROPIC_API_KEY` | Your Anthropic API key (requires `--use-api-key` flag) |

## License

MIT - Do whatever you want with it.
