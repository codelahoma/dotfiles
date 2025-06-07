# Docker Environment Logs

Let input_args = "$ARGUMENTS"

View logs from the FlowLoom Docker isolated environment for troubleshooting and monitoring.

## Argument Interpretation
Process the provided arguments: input_args

Expected patterns:
- (no arguments) - Show recent logs (last 50 lines)
- `--follow` or `-f` - Follow logs in real-time
- `--lines N` - Show last N lines
- `--since TIME` - Show logs since timestamp (e.g., "2h", "2023-01-01T10:00:00")
- `--service SERVICE` - Show logs for specific service only

## Log Display Process

**1. Container Status Check**
@bash cd flowloom-docker && docker compose -f docker-compose.yml ps --format "table {{.Name}}\t{{.Status}}"

**2. Recent Logs (Default)**
If no specific arguments:
@bash cd flowloom-docker && docker compose -f docker-compose.yml logs --tail=50 --timestamps

**3. Follow Logs**
If input_args contains "--follow" or "-f":
@bash cd flowloom-docker && echo "Following logs (Ctrl+C to stop)..." && docker compose -f docker-compose.yml logs --follow --timestamps

**4. Custom Line Count**
If input_args contains "--lines":
Extract line count from input_args and use:
@bash cd flowloom-docker && docker compose -f docker-compose.yml logs --tail=[LINE_COUNT] --timestamps

**5. Time-Based Logs**
If input_args contains "--since":
Extract time parameter and use:
@bash cd flowloom-docker && docker compose -f docker-compose.yml logs --since=[TIME] --timestamps

**6. Service-Specific Logs**
If input_args contains "--service":
@bash cd flowloom-docker && docker compose -f docker-compose.yml logs flowloom-claude --timestamps

## Log Analysis Helpers

**7. Error Pattern Detection**
@bash cd flowloom-docker && docker compose -f docker-compose.yml logs | grep -i "error\|exception\|failed" | tail -10

**8. Security Events**
@bash cd flowloom-docker && docker compose -f docker-compose.yml logs | grep -i "permission\|denied\|security" | tail -10

**9. Performance Metrics**
@bash cd flowloom-docker && docker stats flowloom-claude-isolated --no-stream --format "table {{.Container}}\t{{.CPUPerc}}\t{{.MemUsage}}\t{{.MemPerc}}" 2>/dev/null || echo "Container not running"

## Log Information Provided

Show the user:
- **Timestamps**: All log entries with precise timing
- **Service Context**: Which Docker service generated each log
- **Log Levels**: Error, warning, info levels clearly marked
- **Recent Activity**: Focus on most recent relevant events

## Common Log Patterns

Explain common log entries:
- Container startup/shutdown events
- Claude Code initialization
- Permission changes and security events
- Network isolation confirmations
- Volume mount status

## Troubleshooting Guidance

Based on log content, suggest:
- Configuration adjustments
- Resource allocation changes
- Security setting modifications
- Performance optimizations

Display clear next steps based on log analysis.