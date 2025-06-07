# Session Backup Command

Create manual backup of current or specified session.

## Usage
```
/session:backup [session_id] [type] [note]
```

## Parameters
- `session_id` (optional): Session to backup (defaults to current session)
- `type` (optional): Backup type - `full`, `incremental` (default: incremental)
- `note` (optional): Backup description note

## What this command does:

1. **Identify Session**: Find current or specified session
2. **Create Backup**: Generate backup based on type
3. **Validate Backup**: Verify backup integrity
4. **Update Index**: Record backup in session index
5. **Cleanup**: Remove old backups based on retention policy

## Examples
```
/session:backup
/session:backup session_12345_1640995200_abc123 full "Major milestone backup"
/session:backup current incremental "Before refactoring"
```

## Backup Types:

### Full Backup (`full`)
- Complete compressed archive of entire session directory
- Includes all files: metadata, state, artifacts, logs
- Larger size but complete restoration capability
- Recommended for major milestones

### Incremental Backup (`incremental`)
- Backs up only metadata and state files
- Faster and smaller
- Good for frequent checkpoints
- Default backup type

## Implementation

@bash cd "$FLOWLOOM_ROOT" && ./bin/session_manager.sh backup "$ARGUMENTS"

Show the user backup information including:
- Backup ID and timestamp
- Backup type (full/incremental)
- Files included
- Backup size
- Storage location
- Retention information

## Backup Management:

### Automatic Cleanup:
- Retains last 10 full backups per session
- Removes backups older than 30 days
- Maintains at least 1 backup regardless of age

### Backup Verification:
- Archive integrity checks for full backups
- JSON validation for incremental backups
- Corruption detection and reporting

### Storage:
- Backups stored in `.flowloom/backups/[session_id]/`
- Manifest files track backup metadata
- Compressed format for space efficiency

## Restore Process:
Use `/session:recover` to restore from any backup:
```
/session:recover [session_id] [backup_id]
```

## Security Notes
- Backups stored locally only
- No external transmission
- Respects file permissions
- Validates all backup content

## Related Commands
- `/session:recover` - Restore from backup
- `/session:list-backups` - List available backups
- `/session:cleanup` - Manual backup cleanup