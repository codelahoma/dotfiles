# Archive Current Display

## Purpose
Archive the current active display to history and optionally create a new one.

## Arguments
- No arguments: Archive current display
- `new`: Archive current and create new display

## Implementation

@bash
if [ "$ARGUMENTS" = "new" ]; then
    ./.flowloom/bin/display_archive.py --new
else
    ./.flowloom/bin/display_archive.py
fi