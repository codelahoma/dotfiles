Footer memory status column enhancement completed.

## Changes Made:
1. Added check_memory_status() function to monitor dual memory systems
2. Changed layout from 2x2 to 2x3 table with third column for memory status
3. Added Mem and Doc status indicators:
   - ✅ = used recently (within 1 hour)
   - ⏸️ = healthy but unused in this interaction
   - ❌ = not working correctly

## Status Logic:
- fl-memory.json: checks file modification time vs current time
- basic-memory: checks .flowloom/.knowledge directory for recent file updates
- Considers 1-hour window for 'recent' activity

## Layout:
Row 1: Dir | Mode | Mem
Row 2: Branch | Context | Doc

This provides real-time visibility into dual memory system health and usage.
