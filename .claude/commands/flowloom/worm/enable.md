# Enable WORM Auto-Commit System

Let input_args = "$ARGUMENTS"

Enable the WORM (Write-Once-Read-Many) development environment auto-commit system for complete governance audit trail automation.

## Purpose

Enable automatic governance compliance by committing every file-producing interaction with complete reasoning context preserved in memory snapshots.

## Argument Interpretation

Process enable request: input_args

- Default: Enable with standard governance settings
- With reason: Enable with custom justification (e.g., "starting governance compliance phase")
- `dry-run`: Enable in test mode without actual commits

## WORM Enable Process

1. **Prerequisites Check**
   - Verify git repository is available
   - Check auto-track system availability
   - Validate memory system (memory.json) exists
   - Ensure FlowLoom command system is functional

2. **Enable Auto-Commit**
   ```python
   from src.worm import WORMOrchestrator
   
   worm = WORMOrchestrator()
   result = worm.enable_worm(reason=input_args or "Manual enable via command")
   
   if result['success']:
       print(f"✅ WORM AUTO-COMMIT ENABLED")
       print(f"   Reason: {result.get('reason', 'Manual enable')}")
       print(f"   Governance Status: {result.get('governance_status', 'active')}")
       print()
       print("🔄 All file-producing interactions will now be automatically committed")
       print("📝 Complete reasoning context preserved in commit messages")
       print("🛡️  Immutable governance audit trail activated")
   else:
       print(f"❌ WORM ENABLE FAILED: {result.get('message', 'Unknown error')}")
       
       # Show prerequisites if they failed
       status = worm.get_status()
       if not status['worm_config']['prerequisites_met']:
           print()
           print("Prerequisites not met:")
           for issue in status['worm_config']['issues']:
               print(f"  - {issue}")
   ```

3. **Status Display**
   Show current WORM system status including:
   - Auto-commit state
   - Governance mode
   - Integration status
   - Prerequisites validation

## Implementation

```bash
# Get reason from arguments
REASON="${input_args:-Manual enable via command}"

echo "🔄 ENABLING WORM AUTO-COMMIT SYSTEM..."
echo "   Reason: $REASON"
echo

# Use Python to enable WORM system
python3 -c "
import sys
sys.path.insert(0, 'src')
from worm import WORMOrchestrator

worm = WORMOrchestrator()
result = worm.enable_worm(reason='$REASON')

if result['success']:
    print('✅ WORM AUTO-COMMIT ENABLED')
    print(f'   Governance Status: {result.get(\"governance_status\", \"active\")}')
    print()
    print('🔄 All file-producing interactions will now be automatically committed')
    print('📝 Complete reasoning context preserved in commit messages')
    print('🛡️  Immutable governance audit trail activated')
else:
    print(f'❌ WORM ENABLE FAILED: {result.get(\"message\", \"Unknown error\")}')
    sys.exit(1)
"

# Show status
echo
echo "📊 WORM SYSTEM STATUS:"
python3 -c "
import sys
sys.path.insert(0, 'src')
from worm import WORMOrchestrator

worm = WORMOrchestrator()
status = worm.get_status()

print(f'Auto-commit: {\"ENABLED\" if status[\"worm_config\"][\"auto_commit_enabled\"] else \"DISABLED\"}')
print(f'Governance: {status[\"worm_config\"][\"governance_mode\"].upper()}')
print(f'System Ready: {\"YES\" if status[\"system_ready\"] else \"NO\"}')

if not status['worm_config']['prerequisites_met']:
    print()
    print('Issues:')
    for issue in status['worm_config']['issues']:
        print(f'  - {issue}')
"
```

## Security Note

WORM auto-commit provides immutable audit trails but requires careful consideration:
- Every file change will be automatically committed and pushed
- Sensitive information should be excluded via .gitignore
- System can be disabled but will break governance compliance
- Dry-run mode available for testing

## Usage Examples

- `slashload flowloom/worm/enable` - Enable with default settings
- `slashload flowloom/worm/enable "compliance audit requirement"` - Enable with reason
- `slashload flowloom/worm/enable dry-run` - Enable in test mode