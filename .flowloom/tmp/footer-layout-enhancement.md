Footer layout enhancement - moved git stats and added full-width context row.

## Changes Made:
1. Separated git stats from branch name into dedicated Stats column (row 2, col 2)
2. Added third row with context spanning full table width
3. Restructured layout to 3-row format:
   - Row 1: Dir | Mode | Mem  
   - Row 2: Branch | Stats | Doc
   - Row 3: Context (full width)

## Technical Implementation:
- Extract git stats from branch string using rsplit(" (", 1)
- Build first two rows with 3-column layout
- Add separator with bottom-joining characters
- Create third row spanning full width with proper padding
- Maintain ANSI box drawing for clean visual structure

## Layout Benefits:
- Git stats get dedicated space and clear labeling
- Context no longer truncated due to full-width display
- Better visual organization with logical grouping
- Maintains memory status visibility in compact format

This improves readability and information density while preserving the memory monitoring functionality.