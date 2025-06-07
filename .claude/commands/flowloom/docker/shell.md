# Access Docker Environment Shell

Let input_args = "$ARGUMENTS"

Provide direct bash shell access to the FlowLoom Docker isolated environment.

## Argument Interpretation
Process the provided arguments: input_args

Expected patterns:
- (no arguments) - Start interactive bash shell
- `--command [cmd]` - Execute specific command and return
- `--user root` - Access as root user (for debugging only)

## Shell Access Process

**1. Check Container Status**
@bash cd flowloom-docker && docker compose -f docker-compose.yml ps --filter "name=flowloom-claude-isolated"

**2. Start Container if Needed**
If container is not running:
@bash cd flowloom-docker && docker compose -f docker-compose.yml up -d

**3. Provide Shell Access**
Based on input_args:

For interactive shell (default):
@bash cd flowloom-docker && docker compose -f docker-compose.yml exec flowloom-claude /bin/bash

For specific command execution:
@bash cd flowloom-docker && docker compose -f docker-compose.yml exec flowloom-claude bash -c "$input_args"

For root access (if --user root):
@bash cd flowloom-docker && docker compose -f docker-compose.yml exec --user root flowloom-claude /bin/bash

## Environment Context
Inform the user they now have shell access with:
- **Working Directory**: `/workspace` (project files mounted)
- **User Context**: `flowloom` (non-root for security)
- **Network**: Completely isolated (no internet access)
- **Claude Code**: Available via `claude --dangerously-skip-permissions`
- **Permissions**: Elevated permissions safely contained

## Available Commands in Container
- `claude --dangerously-skip-permissions [args]` - Run Claude Code
- `whoami` - Check current user (should be 'flowloom')
- `pwd` - Current directory (should be '/workspace')
- `ls -la` - List project files
- `exit` - Return to host system

## Security Reminders
- ✅ **Network Isolated**: No internet access from container
- ✅ **Non-root Execution**: Running as unprivileged user
- ✅ **Capability Restrictions**: Only essential Linux capabilities
- ✅ **Volume Security**: Only project directory mounted
- ⚠️ **Dangerous Permissions**: Claude Code can modify files freely within container

Type `exit` to return to your host system when done.