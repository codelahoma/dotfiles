#!/bin/bash

# Script to convert Markdown daily logs to Org-mode format
# Author: Claude
# Date: 2025-03-11
# Updated: 2025-03-12 - Added check for newer markdown files

SOURCE_DIR="$HOME/work/atlas-up-ai/.rodk/daily_logs"
TARGET_DIR="$HOME/personal/org-files/atlas_up_daily_logs"

# Create target directory if it doesn't exist
mkdir -p "$TARGET_DIR"

# Check if pandoc is installed
if ! command -v pandoc &> /dev/null; then
    echo "Error: pandoc is not installed. Please install it first."
    exit 1
fi

# Set counter for conversions
converted=0

# Loop through all .md files in the source directory
for md_file in "$SOURCE_DIR"/*.md; do
    # Check if file exists (in case the glob doesn't match any files)
    if [ ! -f "$md_file" ]; then
        echo "No .md files found in $SOURCE_DIR"
        exit 0
    fi
    
    # Get the basename and create target filename
    base_name=$(basename "$md_file" .md)
    org_file="$TARGET_DIR/$base_name.org"
    
    # Check if the org file needs to be updated
    needs_conversion=false
    
    # If org file doesn't exist, we need to convert
    if [ ! -f "$org_file" ]; then
        needs_conversion=true
        conversion_reason="new file"
    else
        # Check if md file is newer than org file
        if [ "$md_file" -nt "$org_file" ]; then
            needs_conversion=true
            conversion_reason="markdown file is newer"
        fi
    fi
    
    # Perform conversion if needed
    if [ "$needs_conversion" = true ]; then
        echo "Converting $base_name.md to org format ($conversion_reason)..."
        
        # Convert using pandoc
        pandoc -f markdown -t org "$md_file" -o "$org_file"
        
        # Check if conversion was successful
        if [ $? -eq 0 ]; then
            echo "Successfully converted to $org_file"
            converted=$((converted + 1))
        else
            echo "Error converting $md_file"
        fi
    else
        echo "Skipping $base_name.md (org file is up to date)"
    fi
done

echo "Conversion complete. $converted files converted."