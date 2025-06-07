# Docker Environment Setup Wizard

Let input_args = "$ARGUMENTS"

Interactive setup wizard for the FlowLoom Docker isolated environment.

## Argument Interpretation
Process the provided arguments: input_args

Expected patterns:
- (no arguments) - Run full interactive setup
- `--check` - Check prerequisites only
- `--reset` - Reset configuration to defaults
- `--advanced` - Show advanced configuration options

## Prerequisites Check

**1. Docker Engine Availability**
@bash docker --version 2>/dev/null || echo "❌ Docker not found - please install Docker Desktop"

**2. Docker Compose Version**
@bash docker compose version 2>/dev/null || echo "❌ Docker Compose not found - please install Docker Compose v2"

**3. Docker Daemon Status**
@bash docker info >/dev/null 2>&1 && echo "✅ Docker daemon running" || echo "❌ Docker daemon not running - please start Docker"

## Interactive Setup Process

**1. Welcome Message**
Show the user:
```
🐳 FlowLoom Docker Environment Setup
===================================

This wizard will configure your isolated Docker environment for using
Claude Code with --dangerously-skip-permissions safely.

Features:
- Complete network isolation (no internet access)
- Elevated permissions safely contained
- Project files accessible at /workspace
- Non-root execution for security
```

**2. Configuration Options**
If input_args contains "--advanced", show advanced options:
- Network mode selection (isolated/selective)
- Resource limits (memory, CPU)
- Additional volume mounts
- Custom environment variables

**3. Environment Validation**
@bash cd flowloom-docker && docker compose -f docker-compose.yml config --quiet && echo "✅ Docker Compose configuration valid" || echo "❌ Docker Compose configuration invalid"

**4. Image Build Process**
Show progress during image build:
@bash cd flowloom-docker && echo "🔨 Building FlowLoom Docker image..." && docker compose -f docker-compose.yml build --progress=plain

**5. Network Setup**
@bash cd flowloom-docker && echo "🌐 Creating isolated network..." && docker compose -f docker-compose.yml up --no-start

**6. Test Container**
@bash cd flowloom-docker && echo "🧪 Testing container startup..." && docker compose -f docker-compose.yml up -d && sleep 3 && docker compose -f docker-compose.yml exec flowloom-claude echo "✅ Container ready" && docker compose -f docker-compose.yml down

## Setup Completion

Show the user:
```
🎉 Setup Complete!

Your FlowLoom Docker environment is ready. You can now use:

- /flowloom:docker:claude    - Start Claude with dangerous permissions
- /flowloom:docker:shell     - Access container shell
- /flowloom:docker:status    - Check environment status
- /flowloom:docker:stop      - Stop the environment

Next steps:
1. Try: /flowloom:docker:claude
2. Inside container: claude --dangerously-skip-permissions
3. Your project files are available at /workspace

Security features active:
✅ Network isolation (no internet access)
✅ Non-root execution
✅ Minimal Linux capabilities
✅ No privilege escalation
```

## Error Recovery

If any step fails, provide specific guidance:
- Docker installation links
- Docker daemon startup instructions
- Permission troubleshooting
- Configuration file fixes

Show clear next steps for each type of failure.