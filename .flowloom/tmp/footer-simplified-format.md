Footer simplified - removed ANSI box characters and added bold labels.

## Changes Made:
1. Removed all ANSI box drawing characters (┌┬┐├┼┤└┴┘│)
2. Added bold markdown formatting to all labels (**Label:**)
3. Simplified layout to plain text with fixed-width spacing
4. Maintained 3-row structure and column widths
5. Preserved memory indicators in column 1 positioning

## Layout Structure:
- Row 1: **Mem:** ✅    **Dir:** path    **Mode:** mode
- Row 2: **Doc:** ✅    **Branch:** name    **Stats:** (21M, 44?)  
- Row 3: **Context:** full description

## Benefits:
- Cleaner, more readable appearance
- Better compatibility across different terminals
- Bold labels provide visual hierarchy
- Fixed widths maintain alignment
- Reduced visual clutter while preserving information density

## Technical Details:
- Adjusted padding calculations to account for markdown bold syntax
- Removed complex ANSI box drawing logic
- Simplified table_lines construction
- Maintained column width constants for consistent spacing

This creates a clean, professional footer that's easy to read while preserving all functionality.