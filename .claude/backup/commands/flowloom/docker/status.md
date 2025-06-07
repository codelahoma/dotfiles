# Check Docker Environment Status

Let input_args = "$ARGUMENTS"

Check the status and health of the FlowLoom Docker isolated environment.

## Argument Interpretation
Process the provided arguments: input_args

Expected patterns:
- (no arguments) - Show basic container status
- `--detailed` - Show comprehensive status including resource usage
- `--logs` - Show recent container logs
- `--health` - Show health check status

## Status Check Process

**1. Container Status**
@bash cd flowloom-docker && echo "📊 FlowLoom Docker Environment Status" && echo "================================" && docker compose -f docker-compose.yml ps

**2. Health Check Status** 
@bash cd flowloom-docker && docker inspect flowloom-claude-isolated --format '{{.State.Health.Status}}' 2>/dev/null || echo "Health check not available"

**3. Resource Usage (if --detailed)**
If input_args contains "--detailed":
@bash cd flowloom-docker && docker stats flowloom-claude-isolated --no-stream --format "table {{.Container}}\t{{.CPUPerc}}\t{{.MemUsage}}\t{{.MemPerc}}\t{{.NetIO}}\t{{.BlockIO}}"

**4. Network Status**
@bash cd flowloom-docker && docker network ls | grep flowloom-docker

**5. Volume Status**
@bash cd flowloom-docker && docker volume ls | grep flowloom-docker

**6. Recent Logs (if --logs)**
If input_args contains "--logs":
@bash cd flowloom-docker && docker compose -f docker-compose.yml logs --tail=20 flowloom-claude

## Status Information Provided
Show the user comprehensive status including:
- **Container State**: Running/Stopped/Starting
- **Health Status**: Healthy/Unhealthy/Starting
- **Network Isolation**: Confirmed isolated network
- **Volume Mounts**: Project directory accessibility
- **Security Context**: Non-root user, capabilities dropped
- **Claude Code**: Version and availability status

## Quick Status Indicators
- 🟢 **Running & Healthy**: Ready for dangerous permissions
- 🟡 **Starting**: Container initializing
- 🔴 **Stopped**: Need to start with `/flowloom:docker:claude`
- ⚠️ **Unhealthy**: Container running but Claude Code may have issues

Display clear next steps based on current status.