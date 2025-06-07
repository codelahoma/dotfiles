# Stop Docker Environment

Let input_args = "$ARGUMENTS"

Safely stop the FlowLoom Docker isolated environment and clean up resources.

## Argument Interpretation
Process the provided arguments: input_args

Expected patterns:
- (no arguments) - Graceful stop with cleanup
- `--force` - Force stop immediately
- `--remove` - Stop and remove container/volumes
- `--keep-volumes` - Stop but preserve volumes

## Stop Process

**1. Check Current Status**
@bash cd flowloom-docker && docker compose -f docker-compose.yml ps

**2. Stop Container**
Based on input_args:

For graceful stop (default):
@bash cd flowloom-docker && docker compose -f docker-compose.yml stop

For force stop:
@bash cd flowloom-docker && docker compose -f docker-compose.yml kill

For stop and remove:
@bash cd flowloom-docker && docker compose -f docker-compose.yml down

For complete cleanup (remove volumes):
@bash cd flowloom-docker && docker compose -f docker-compose.yml down -v

**3. Verify Cleanup**
@bash cd flowloom-docker && docker compose -f docker-compose.yml ps

**4. Show Resource Status**
@bash cd flowloom-docker && echo "Remaining Docker resources:" && docker volume ls | grep flowloom-docker || echo "No FlowLoom volumes remaining"

## Cleanup Information
Show the user what was stopped/cleaned:
- **Container**: Stopped and optionally removed
- **Networks**: Cleaned up if container removed  
- **Volumes**: Preserved by default (use --remove to clean)
- **Images**: Preserved for faster restart

## Security Notes
- ✅ **Isolated Environment Stopped**: No longer consuming resources
- ✅ **Network Isolation Ended**: Container no longer accessible
- ✅ **File Changes**: Any modifications within container are lost unless committed
- ℹ️ **Volume Persistence**: Claude config and workspace changes preserved in volumes (unless --remove used)

## Restart Instructions
To start the environment again:
- Use `/flowloom:docker:claude` to restart with Claude
- Use `/flowloom:docker:shell` to restart with shell access
- Use `/flowloom:docker:status` to check current state

## Resource Cleanup Options
- **Default Stop**: Container stopped, volumes preserved for next session
- **--force**: Immediate termination (may lose unsaved work)
- **--remove**: Complete cleanup (faster restart but lose any container-specific config)
- **--keep-volumes**: Explicitly preserve all data volumes