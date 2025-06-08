Footer interaction-aware status indicators implemented.

## Changes Made:
1. Changed time window from 1 hour to 5 minutes for interaction detection
2. Updated status logic to be interaction-specific:
   - ✅ = used within last 5 minutes (current interaction)
   - ⏸️ = healthy but unused in this interaction
   - ❌ = not working correctly

## Status Behavior:
- Memory tools present but not used in interaction → ⏸️ (pause)
- Memory tools used in current interaction → ✅ (checkmark)
- Memory tools not working or missing → ❌ (red x)

## Benefits:
- More accurate representation of memory usage per interaction
- Clear distinction between "available but unused" vs "actively used"
- Encourages memory logging by showing pause when not used
- 5-minute window captures recent activity within current session

## Technical Details:
- Reduced timedelta from hours=1 to minutes=5
- Updated comments to reflect interaction-specific checking
- Maintained same file modification time logic
- Preserves existing file existence and error handling

This makes the footer status indicators more responsive to actual usage patterns during individual interactions.