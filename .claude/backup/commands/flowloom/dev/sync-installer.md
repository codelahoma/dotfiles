# Sync Installer Bundle

Validate and update the FlowLoom installer bundle with current project files.

## Usage

```bash
# Validate installer bundle
python3 bin/installer_bundler.py --validate

# Show what would be updated (dry run)
python3 bin/installer_bundler.py --update --dry-run

# Update installer bundle
python3 bin/installer_bundler.py --update
```

## What gets synced

- **FlowLoom Commands** (.claude directory)
- **CLAUDE.md Template** (base FlowLoom configuration)  
- **CLAUDE.local.md Template** (local project configuration)
- **MCP Configuration** (.mcp.json)
- **Settings Template** (settings.local.json)
- **WORM Source Code** (src/worm directory)

## Features

- **Validation**: Check if installer resources are in sync
- **Dry run**: Preview changes before applying
- **Detailed reporting**: File counts, sizes, and sync status
- **Error handling**: Clear messages for missing or invalid files

Perfect for ensuring installer templates stay current with development changes!