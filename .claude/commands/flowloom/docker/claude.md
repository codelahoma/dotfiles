# Run Claude Code in Isolated Docker Environment

Let input_args = "$ARGUMENTS"

Execute Claude Code with `--dangerously-skip-permissions` in a secure, network-isolated Docker container.

## Argument Interpretation
Process the provided arguments: input_args

Expected patterns:
- (no arguments) - Start interactive Claude session in container
- `--shell` - Start bash shell instead of Claude
- `--status` - Check container status
- `--stop` - Stop the container
- `[claude_args]` - Pass arguments directly to Claude Code

## Docker Environment Execution

**1. Check Container Status**
@bash cd flowloom-docker && echo "🔍 Checking container status..." && docker compose -f docker-compose.yml ps --format "table {{.Name}}\t{{.Status}}"

**2. Start Container if Not Running**
If container is not running, start it:
@bash cd flowloom-docker && echo "🚀 Starting FlowLoom Docker environment..." && docker compose -f docker-compose.yml up -d && echo "⏳ Waiting for container to be ready..." && sleep 3

**3. Execute Claude Code**
Based on input_args:
- If no arguments or not `--shell/--status/--stop`: Run Claude with dangerous permissions
- If `--shell`: Start interactive bash shell
- If `--status`: Show detailed container status
- If `--stop`: Stop the container

For Claude execution:
@bash cd flowloom-docker && docker compose -f docker-compose.yml exec flowloom-claude claude --dangerously-skip-permissions $input_args

For shell access:
@bash cd flowloom-docker && docker compose -f docker-compose.yml exec flowloom-claude /bin/bash

Show the user the command output and indicate that they're now in the isolated environment with:
- ✅ Network isolation (no internet access)
- ✅ Elevated permissions safely contained
- ✅ Project files mounted at `/workspace`
- ✅ Non-root execution for security

## Security Features Active
- Complete network isolation from internet
- File system access controlled via volume mounts
- Non-root user execution (flowloom user)
- Minimal Linux capabilities (only essential ones)
- No privilege escalation allowed

## Usage Examples
- `/flowloom:docker:claude` - Interactive Claude session
- `/flowloom:docker:claude --help` - Claude help in container
- `/flowloom:docker:claude --shell` - Bash shell access
- `/flowloom:docker:claude --status` - Container status
- `/flowloom:docker:claude --stop` - Stop container