# Update Installer Resources (Complete)

Sync ALL current project resources to the installer, including UI components, WebSocket bridge, and demo files.

This enhanced command ensures the installer has everything needed for a complete FlowLoom installation with demo capabilities.

## What gets synced:

1. **`.claude/` directory** → All FlowLoom commands
2. **Configuration files** → `.mcp.json`, `CLAUDE.local.md`, `settings.local.json`
3. **UI Components** → All Swift files for FlowLoom UI
4. **WebSocket Bridge** → Python bridge components for UI communication
5. **Scripts** → Launch/stop/toggle scripts, demo scripts
6. **Demo Gallery** → Complete demo-gallery directory
7. **WORM System** → src/worm module
8. **Demo Files** → watch-the-demo-first.html

## Usage

```bash
# Sync all resources
/flowloom:dev:update-installer-resources-complete

# Show what would be synced (dry run)
/flowloom:dev:update-installer-resources-complete --dry-run
```

## Implementation

```bash
#!/bin/bash

set -e

# Configuration
PROJECT_ROOT="/Users/rodk/github/flowloom"
INSTALLER_RESOURCES="$PROJECT_ROOT/packages/flowloom_installer/src/flowloom_installer/resources"
DRY_RUN=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

echo "🔄 FlowLoom Installer Resource Sync (Complete)"
echo "============================================"
echo
echo "Project root: $PROJECT_ROOT"
echo "Installer resources: $INSTALLER_RESOURCES"

if [ "$DRY_RUN" = true ]; then
    echo "Mode: DRY RUN (no changes will be made)"
else
    echo "Mode: LIVE SYNC"
fi

echo

# Function to sync a file/directory
sync_resource() {
    local source="$1"
    local dest="$2"
    local name="$3"
    
    if [ ! -e "$source" ]; then
        echo "⚠️  SKIP: $name (source not found: $source)"
        return
    fi
    
    if [ "$DRY_RUN" = true ]; then
        echo "📋 WOULD SYNC: $name"
        echo "   FROM: $source"
        echo "   TO:   $dest"
        
        if [ -d "$source" ]; then
            local file_count=$(find "$source" -type f | wc -l | tr -d ' ')
            echo "   FILES: $file_count files"
        else
            local size=$(stat -f%z "$source" 2>/dev/null || echo "unknown")
            echo "   SIZE: $size bytes"
        fi
    else
        echo "📂 SYNCING: $name"
        
        # Create destination directory
        mkdir -p "$(dirname "$dest")"
        
        # Remove existing destination if it exists
        if [ -e "$dest" ]; then
            rm -rf "$dest"
        fi
        
        # Copy source to destination
        if [ -d "$source" ]; then
            cp -r "$source" "$dest"
            local file_count=$(find "$dest" -type f | wc -l | tr -d ' ')
            echo "   ✅ Copied directory ($file_count files)"
        else
            cp "$source" "$dest"
            local size=$(stat -f%z "$dest" 2>/dev/null || echo "unknown")
            echo "   ✅ Copied file ($size bytes)"
        fi
    fi
    echo
}

# Create resource directories if needed
if [ "$DRY_RUN" = false ]; then
    mkdir -p "$INSTALLER_RESOURCES/ui"
    mkdir -p "$INSTALLER_RESOURCES/scripts"
    mkdir -p "$INSTALLER_RESOURCES/websocket"
fi

echo "🎯 Resources to sync:"
echo

# 1. Commands directory
sync_resource \
    "$PROJECT_ROOT/.claude" \
    "$INSTALLER_RESOURCES/.claude" \
    ".claude commands directory"

# 2. Configuration files
sync_resource \
    "$PROJECT_ROOT/.mcp.json" \
    "$INSTALLER_RESOURCES/.mcp.json" \
    "MCP server configuration"

sync_resource \
    "$PROJECT_ROOT/CLAUDE.local.md" \
    "$INSTALLER_RESOURCES/CLAUDE.local.md" \
    "Claude configuration template"

sync_resource \
    "$PROJECT_ROOT/settings.local.json" \
    "$INSTALLER_RESOURCES/settings.local.json" \
    "Permission settings template"

# 3. UI Components (Swift files)
echo "🖼️  Syncing UI Components:"
for swift_file in "$PROJECT_ROOT"/*.swift; do
    if [ -f "$swift_file" ]; then
        filename=$(basename "$swift_file")
        sync_resource \
            "$swift_file" \
            "$INSTALLER_RESOURCES/ui/$filename" \
            "UI: $filename"
    fi
done

# 4. WebSocket Bridge Components
echo "🌐 Syncing WebSocket Bridge:"
websocket_files=(
    "flowloom-claude-bridge.py"
    "flowloom-web-claude-patch.py"
    "flowloom-output-capture.py"
    "flowloom-ui-watcher.py"
)

for ws_file in "${websocket_files[@]}"; do
    sync_resource \
        "$PROJECT_ROOT/$ws_file" \
        "$INSTALLER_RESOURCES/websocket/$ws_file" \
        "WebSocket: $ws_file"
done

# 5. Scripts
echo "📜 Syncing Scripts:"
script_files=(
    "launch-flowloom-ui.sh"
    "stop-flowloom-ui.sh"
    "toggle-flowloom-ui.sh"
    "demo-terminal-roundtrip.sh"
    "claude-with-ui.sh"
)

for script in "${script_files[@]}"; do
    sync_resource \
        "$PROJECT_ROOT/$script" \
        "$INSTALLER_RESOURCES/scripts/$script" \
        "Script: $script"
done

# Also sync demo-memory-injection.py
sync_resource \
    "$PROJECT_ROOT/demo-memory-injection.py" \
    "$INSTALLER_RESOURCES/scripts/demo-memory-injection.py" \
    "Demo memory injection script"

# 6. Demo Gallery
sync_resource \
    "$PROJECT_ROOT/demo-gallery" \
    "$INSTALLER_RESOURCES/demo-gallery" \
    "Demo gallery directory"

# 7. WORM System
sync_resource \
    "$PROJECT_ROOT/src/worm" \
    "$INSTALLER_RESOURCES/worm" \
    "WORM governance system"

# 8. Demo Files
sync_resource \
    "$PROJECT_ROOT/watch-the-demo-first.html" \
    "$INSTALLER_RESOURCES/watch-the-demo-first.html" \
    "Demo landing page"

# Summary
echo "📊 Sync Summary"
echo "==============="

if [ "$DRY_RUN" = true ]; then
    echo "This was a dry run. Run without --dry-run to perform the actual sync."
else
    # Count total files synced
    total_files=$(find "$INSTALLER_RESOURCES" -type f | wc -l | tr -d ' ')
    echo "✅ All resources have been synced to the installer bundle."
    echo "📁 Total files in installer resources: $total_files"
    echo
    echo "🔧 Next steps:"
    echo "1. Test the installer with these updated resources"
    echo "2. Update installer to copy WebSocket and new UI components"
    echo "3. Run: python install-flowloom.py ~/test-demo-install"
    echo
    echo "⚠️  IMPORTANT: The installer code may need updates to handle:"
    echo "   - WebSocket bridge installation"
    echo "   - Additional UI components"
    echo "   - Demo mode activation"
fi

echo
echo "🏁 Resource sync complete!"
```