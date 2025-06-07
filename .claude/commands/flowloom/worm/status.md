# WORM System Status

Display comprehensive status of the WORM (Write-Once-Read-Many) development environment auto-commit system.

## Purpose

Provide detailed visibility into WORM system state, configuration, prerequisites, and current governance compliance status.

## Status Display

```python
from src.worm import WORMOrchestrator

worm = WORMOrchestrator()
status = worm.get_status()

print("🛡️  WORM DEVELOPMENT ENVIRONMENT STATUS")
print("=" * 50)
print()

# Core Status
print("📊 CORE STATUS:")
print(f"   Auto-commit: {'🟢 ENABLED' if status['worm_config']['auto_commit_enabled'] else '🔴 DISABLED'}")
print(f"   Governance: {status['worm_config']['governance_mode'].upper()}")
print(f"   System Ready: {'🟢 YES' if status['system_ready'] else '🔴 NO'}")
print(f"   Dry Run: {'🟡 ENABLED' if status['worm_config']['dry_run_mode'] else '🔴 DISABLED'}")
print()

# Prerequisites
print("🔧 PREREQUISITES:")
if status['worm_config']['prerequisites_met']:
    print("   🟢 All prerequisites met")
else:
    print("   🔴 Prerequisites not met:")
    for issue in status['worm_config']['issues']:
        print(f"      - {issue}")
print()

# Git Status
print("📂 GIT REPOSITORY:")
if status['git_status']['is_git_repo']:
    print("   🟢 Git repository detected")
    if status['git_status']['has_uncommitted_changes']:
        print(f"   🟡 {len(status['git_status']['uncommitted_files'])} uncommitted files")
        for file_info in status['git_status']['uncommitted_files'][:3]:
            print(f"      {file_info['status']} {file_info['file']}")
        if len(status['git_status']['uncommitted_files']) > 3:
            print(f"      ... and {len(status['git_status']['uncommitted_files']) - 3} more")
    else:
        print("   🟢 Working directory clean")
else:
    print("   🔴 Not a git repository")
print()

# Current Interaction
print("🔄 CURRENT INTERACTION:")
if status['current_interaction']:
    interaction = status['current_interaction']
    print(f"   🟢 Active: {interaction['interaction_id']}")
    print(f"   Request: {interaction['user_request'][:60]}...")
    print(f"   Files: {interaction['file_count']} changed")
else:
    print("   🔴 No active interaction")
print()

# Configuration
print("⚙️  CONFIGURATION:")
config = status['worm_config']
print(f"   Config File: {config['config_file']}")
print(f"   File Exclusions: {config['file_exclusions_count']} patterns")
print(f"   Memory Snapshots: {'ENABLED' if config['integration_settings'].get('memory_system_required') else 'DISABLED'}")
print(f"   Auto-track Integration: {'REQUIRED' if config['integration_settings'].get('auto_track_required') else 'OPTIONAL'}")
print(f"   Last Updated: {config['last_updated']}")
```

## Implementation

```bash
echo "🛡️  WORM DEVELOPMENT ENVIRONMENT STATUS"
echo "=" | tr -d '\n'; for i in {1..50}; do echo -n "="; done; echo
echo

# Use Python to get comprehensive status
python3 -c "
import sys
sys.path.insert(0, 'src')
from worm import WORMOrchestrator

worm = WORMOrchestrator()
status = worm.get_status()

# Core Status
print('📊 CORE STATUS:')
enabled = '🟢 ENABLED' if status['worm_config']['auto_commit_enabled'] else '🔴 DISABLED'
print(f'   Auto-commit: {enabled}')
print(f'   Governance: {status[\"worm_config\"][\"governance_mode\"].upper()}')
ready = '🟢 YES' if status['system_ready'] else '🔴 NO'
print(f'   System Ready: {ready}')
dry_run = '🟡 ENABLED' if status['worm_config']['dry_run_mode'] else '🔴 DISABLED'
print(f'   Dry Run: {dry_run}')
print()

# Prerequisites
print('🔧 PREREQUISITES:')
if status['worm_config']['prerequisites_met']:
    print('   🟢 All prerequisites met')
else:
    print('   🔴 Prerequisites not met:')
    for issue in status['worm_config']['issues']:
        print(f'      - {issue}')
print()

# Git Status
print('📂 GIT REPOSITORY:')
if status['git_status']['is_git_repo']:
    print('   🟢 Git repository detected')
    if status['git_status']['has_uncommitted_changes']:
        count = len(status['git_status']['uncommitted_files'])
        print(f'   🟡 {count} uncommitted files')
        for file_info in status['git_status']['uncommitted_files'][:3]:
            print(f'      {file_info[\"status\"]} {file_info[\"file\"]}')
        if count > 3:
            print(f'      ... and {count - 3} more')
    else:
        print('   🟢 Working directory clean')
else:
    print('   🔴 Not a git repository')
print()

# Current Interaction
print('🔄 CURRENT INTERACTION:')
if status['current_interaction']:
    interaction = status['current_interaction']
    print(f'   🟢 Active: {interaction[\"interaction_id\"]}')
    request = interaction['user_request'][:60]
    if len(interaction['user_request']) > 60:
        request += '...'
    print(f'   Request: {request}')
    print(f'   Files: {interaction[\"file_count\"]} changed')
else:
    print('   🔴 No active interaction')
print()

# Configuration
print('⚙️  CONFIGURATION:')
config = status['worm_config']
print(f'   File Exclusions: {config[\"file_exclusions_count\"]} patterns')
memory_enabled = 'ENABLED' if config['integration_settings'].get('memory_system_required') else 'DISABLED'
print(f'   Memory Snapshots: {memory_enabled}')
auto_track = 'REQUIRED' if config['integration_settings'].get('auto_track_required') else 'OPTIONAL'
print(f'   Auto-track Integration: {auto_track}')
print(f'   Last Updated: {config[\"last_updated\"]}')
"

echo
echo "💡 Use 'slashload flowloom/worm/enable' to enable auto-commit"
echo "💡 Use 'slashload flowloom/worm/disable' to disable auto-commit"
```

## Status Indicators

- 🟢 **Green**: System ready/operational
- 🟡 **Yellow**: Warning/attention needed  
- 🔴 **Red**: Issue/not ready
- 🛡️ **Shield**: WORM system components
- 📊 **Chart**: Status information
- 🔧 **Wrench**: Prerequisites/configuration
- 📂 **Folder**: Git repository status
- 🔄 **Loop**: Active interactions
- ⚙️ **Gear**: Configuration settings