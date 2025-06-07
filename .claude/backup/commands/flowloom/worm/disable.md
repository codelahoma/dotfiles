# Disable WORM Auto-Commit System

Let input_args = "$ARGUMENTS"

Disable the WORM (Write-Once-Read-Many) development environment auto-commit system.

## ⚠️ GOVERNANCE WARNING

Disabling WORM auto-commit **INTERRUPTS GOVERNANCE COMPLIANCE** and breaks the immutable audit trail. This should only be done with explicit justification.

## Purpose

Temporarily or permanently disable automatic commits while maintaining the ability to manually preserve governance records.

## Argument Interpretation

Process disable request: input_args

- **REQUIRED**: Must provide reason for disabling governance
- Examples: "emergency debugging", "sensitive file operations", "manual commit preferred"

## Disable Process

1. **Explicit Reasoning Required**
   - WORM governance requires documented justification for compliance interruption
   - Reason will be recorded in configuration and audit trail

2. **Disable Auto-Commit**
   ```python
   from src.worm import WORMOrchestrator
   
   worm = WORMOrchestrator()
   result = worm.disable_worm(reason=input_args)
   
   if result['success']:
       print(f"⚠️  WORM AUTO-COMMIT DISABLED")
       print(f"   Reason: {input_args}")
       print(f"   Governance Status: {result.get('governance_status', 'interrupted')}")
       print()
       print("🔴 Governance audit trail interrupted")
       print("📝 Manual commits required for compliance")
       print("🛡️  Re-enable WORM to restore automated governance")
   else:
       print(f"❌ WORM DISABLE FAILED: {result.get('message', 'Unknown error')}")
   ```

3. **Manual Commit Guidance**
   Provide instructions for maintaining governance compliance manually

## Implementation

```bash
# Require reason for disabling governance
if [ -z "$input_args" ]; then
    echo "❌ REASON REQUIRED"
    echo
    echo "WORM auto-commit provides governance compliance through immutable audit trails."
    echo "Disabling requires explicit justification for audit purposes."
    echo
    echo "Usage: slashload flowloom/worm/disable \"<reason>\""
    echo "Examples:"
    echo "  slashload flowloom/worm/disable \"emergency debugging session\""
    echo "  slashload flowloom/worm/disable \"sensitive configuration changes\""
    echo "  slashload flowloom/worm/disable \"manual commit preference\""
    exit 1
fi

REASON="$input_args"

echo "⚠️  DISABLING WORM AUTO-COMMIT SYSTEM"
echo "   Reason: $REASON"
echo

# Use Python to disable WORM system
python3 -c "
import sys
sys.path.insert(0, 'src')
from worm import WORMOrchestrator

worm = WORMOrchestrator()
result = worm.disable_worm(reason='$REASON')

if result['success']:
    print('⚠️  WORM AUTO-COMMIT DISABLED')
    print(f'   Governance Status: {result.get(\"governance_status\", \"interrupted\")}')
    print()
    print('🔴 Governance audit trail interrupted')
    print('📝 Manual commits required for compliance')
    print('🛡️  Re-enable WORM to restore automated governance')
    print()
    print('MANUAL COMMIT GUIDANCE:')
    print('1. Stage files: git add <files>')
    print('2. Commit with context: git commit -F <message_file>')
    print('3. Push changes: git push')
    print('4. Document reasoning in commit messages')
else:
    print(f'❌ WORM DISABLE FAILED: {result.get(\"message\", \"Unknown error\")}')
    sys.exit(1)
"

# Show updated status
echo
echo "📊 UPDATED WORM STATUS:"
python3 -c "
import sys
sys.path.insert(0, 'src')
from worm import WORMOrchestrator

worm = WORMOrchestrator()
status = worm.get_status()

print(f'Auto-commit: {\"ENABLED\" if status[\"worm_config\"][\"auto_commit_enabled\"] else \"DISABLED\"}')
print(f'Governance: {status[\"worm_config\"][\"governance_mode\"].upper()}')
print(f'System Ready: {\"YES\" if status[\"system_ready\"] else \"NO\"}')
"

echo
echo "💡 Use 'slashload flowloom/worm/enable' to restore automated governance"
```

## Compliance Notes

1. **Audit Trail Impact**: Disabling WORM creates gaps in the governance audit trail
2. **Manual Responsibility**: User becomes responsible for maintaining commit discipline
3. **Re-enable Recommendation**: Re-enable WORM as soon as governance automation is appropriate
4. **Reason Documentation**: Disable reason is permanently recorded for audit purposes

## Alternative Options

Instead of fully disabling, consider:
- **Dry-run mode**: Test WORM behavior without actual commits
- **File exclusions**: Exclude specific files/patterns from auto-commit
- **Selective enabling**: Enable only for specific development phases

## Usage Examples

- `slashload flowloom/worm/disable "emergency debugging session"` - Disable for debugging
- `slashload flowloom/worm/disable "sensitive configuration updates"` - Disable for sensitive work
- `slashload flowloom/worm/disable "manual commit preference for this feature"` - Disable for manual control