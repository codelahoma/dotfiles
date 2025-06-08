Footer memory indicators moved to column 1 for better alignment stability.

## Changes Made:
1. Moved memory indicators (Mem/Doc) from column 3 to column 1
2. Adjusted column widths: left=12 (memory), middle=35, right=35
3. Restructured layout to:
   - Row 1: Mem | Dir | Mode  
   - Row 2: Doc | Branch | Stats
   - Row 3: Context (full width)

## Benefits:
- Memory indicators can't get pushed out of alignment
- Fixed narrow column ensures consistent positioning
- More important info (Dir/Branch) gets wider space
- Visual hierarchy: status first, then content

## Technical Details:
- Left column reduced to 12 chars (perfect for "Mem: ✅")
- Middle and right columns expanded to 35 chars each
- Maintains total width calculation for context row
- Preserves ANSI box drawing structure

This ensures the memory status indicators are always visible and properly aligned regardless of content length in other columns.