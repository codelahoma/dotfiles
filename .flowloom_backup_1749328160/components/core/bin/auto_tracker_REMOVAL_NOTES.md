# Auto Tracker Script Removal Notes

## File Removed
- **Original**: `bin/auto_tracker.py`
- **Moved to**: `bin/auto_tracker.py.DEPRECATED`
- **Date**: 2025-06-02 11:07
- **Reason**: Runaway background process creating spurious observations

## Problem Summary
The `auto_tracker.py` script was creating continuous background processes that:
1. **Generated spurious observations** every 2 minutes
2. **Contaminated knowledge craft** with 261+ meaningless entries (Shell_ID 31508)
3. **Caused performance issues** and data quality problems
4. **Re-launched after fixes** due to branch synchronization issues

## Replacement
The auto-track functionality has been replaced with immediate observation capture:
- **New approach**: `./bin/log_observation.py add-observation` for immediate capture
- **Command updated**: `.claude/commands/flowloom/system/auto-track.md`
- **No background processes**: Manual observation capture only
- **WORM governance**: Still maintained through deliberate capture points

## Script Issues Found
1. **Continuous daemon**: Ran every 2 minutes indefinitely
2. **Lock file management**: Created persistent processes
3. **No timeout/limits**: Could run forever once started
4. **Branch persistence**: Survived branch switches and contaminated multiple sessions

## Action Taken
- [x] Script moved to `.DEPRECATED` to prevent execution
- [x] Auto-track command updated to use immediate capture
- [x] Testing completed with new approach
- [ ] Shell_ID 31508 spurious entries still need cleanup from knowledge craft

## Prevention
- **No background auto-tracking**: Only manual/deliberate capture
- **Immediate observation**: Using `log_observation.py` gatekeeper
- **Session isolation**: No cross-session contamination
- **Resource limits**: No persistent processes or timers

This removal prevents the second occurrence of runaway auto-tracking and ensures clean WORM governance through deliberate observation points.