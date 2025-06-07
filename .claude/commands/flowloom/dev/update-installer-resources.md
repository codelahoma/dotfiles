# Update Installer Resources

Sync current project configuration back to the installer's bundled resources.

This command copies the current FlowLoom configuration files from the project to the installer's resource bundle, ensuring that new installations will have the latest commands, settings, and configurations.

## What gets synced:

1. **`.claude/` directory** → installer resources (all FlowLoom commands)
2. **`.mcp.json`** → installer MCP configuration template  
3. **`CLAUDE.local.md`** → installer Claude configuration template
4. **`settings.local.json`** → installer permission settings template

## Usage

```bash
# Sync all resources
/flowloom:dev:update-installer-resources

# Show what would be synced (dry run)
/flowloom:dev:update-installer-resources --dry-run
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

echo "🔄 FlowLoom Installer Resource Sync"
echo "=================================="
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

# Sync resources
echo "🎯 Resources to sync:"
echo

# 1. .claude directory (commands)
sync_resource \
    "$PROJECT_ROOT/.claude" \
    "$INSTALLER_RESOURCES/.claude" \
    ".claude commands directory"

# 2. MCP configuration
sync_resource \
    "$PROJECT_ROOT/.mcp.json" \
    "$INSTALLER_RESOURCES/.mcp.json" \
    "MCP server configuration"

# 3. Claude configuration template
sync_resource \
    "$PROJECT_ROOT/CLAUDE.local.md" \
    "$INSTALLER_RESOURCES/CLAUDE.local.md" \
    "Claude configuration template"

# 4. Settings template
sync_resource \
    "$PROJECT_ROOT/.claude/settings.local.json" \
    "$INSTALLER_RESOURCES/settings.local.json" \
    "Permission settings template"

# Summary
echo "📊 Sync Summary"
echo "==============="

if [ "$DRY_RUN" = true ]; then
    echo "This was a dry run. Run without --dry-run to perform the actual sync."
else
    echo "✅ All resources have been synced to the installer bundle."
    echo
    echo "🔧 Next steps:"
    echo "1. Test the installer with these updated resources"
    echo "2. Consider updating the installer version if significant changes were made"
    echo "3. Run tests to ensure the bundled resources work correctly"
    echo
    echo "💡 You can test the installer by running:"
    echo "   python packages/flowloom_installer/src/flowloom_installer/cli.py install --dry-run"
fi

echo
echo "🏁 Resource sync complete!"
```