# Start Isolated Environment for Dangerous Permissions

Let input_args = "$ARGUMENTS"

Start the FlowLoom Docker isolated environment specifically for running Claude Code with `--dangerously-skip-permissions` safely.

## Argument Interpretation
Process the provided arguments: input_args

All arguments will be passed through to Claude Code after the environment is ready.

## Isolated Environment Startup

**1. Environment Validation**
@bash cd flowloom-docker && echo "🔍 Checking Docker environment..." && docker --version && docker compose version

**2. Container Health Check**
@bash cd flowloom-docker && echo "🏥 Checking container health..." && docker compose -f docker-compose.yml ps || echo "Container not running"

**3. Start Isolated Environment**
@bash cd flowloom-docker && echo "🚀 Starting isolated environment..." && docker compose -f docker-compose.yml up -d

**4. Wait for Health Check**
@bash cd flowloom-docker && echo "⏳ Waiting for environment to be ready..." && sleep 3 && docker compose -f docker-compose.yml ps

**5. Verify Claude Code**
@bash cd flowloom-docker && echo "✅ Verifying Claude Code installation..." && docker compose -f docker-compose.yml exec flowloom-claude claude --version

**6. Start Interactive Session**
@bash cd flowloom-docker && echo "🔒 Starting secure session with dangerous permissions..." && docker compose -f docker-compose.yml exec flowloom-claude claude --dangerously-skip-permissions $input_args

## Security Context Active
When you see the Claude prompt, you are running in a secure environment with:

### ✅ **Security Protections**
- **Network Isolation**: No internet access (cannot reach external APIs without explicit setup)
- **User Isolation**: Running as non-root 'flowloom' user
- **Capability Restrictions**: Only essential Linux capabilities enabled
- **Volume Security**: Only your project directory is accessible

### ⚡ **Enhanced Permissions** 
- **File System**: Can modify any file in the mounted project directory
- **System Operations**: Can perform operations normally restricted by Claude Code
- **Tool Execution**: Can run system commands and tools without permission prompts

### 📁 **Environment Layout**
- **Working Directory**: `/workspace` (your project files)
- **User Home**: `/home/flowloom` (container-specific settings)
- **Claude Config**: Persisted in Docker volume for session continuity

## Usage Guidelines
- All file modifications happen within the isolated container
- Project files are synchronized with your host system via volume mounts
- Use standard Claude Code commands and features
- Type `exit` or use Ctrl+C to return to host environment

## Emergency Controls
- **Stop Environment**: Use `/flowloom:docker:stop` or Ctrl+C
- **Check Status**: Use `/flowloom:docker:status` 
- **Get Shell**: Use `/flowloom:docker:shell` for debugging

The isolated environment is now ready for dangerous operations with complete safety boundaries.