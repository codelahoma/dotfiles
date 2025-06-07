#!/usr/bin/env python3
"""Simplified FlowLoom Configuration Access.

This script provides basic configuration access without external dependencies
for integration with shell scripts and workflow commands.
"""

import sys
import json
import argparse
from pathlib import Path
from typing import Optional, Dict, Any


class SimpleConfigurationAccess:
    """Simple configuration access without validation dependencies."""
    
    def __init__(self):
        self.project_root = Path.cwd()
        
    def load_raw_config(self) -> Optional[Dict[str, Any]]:
        """Load raw configuration from flowloom.json."""
        config_file = self.project_root / "flowloom.json"
        
        if not config_file.exists():
            print(f"Error: Configuration file not found: {config_file}", file=sys.stderr)
            return None
        
        try:
            with open(config_file) as f:
                return json.load(f)
        except json.JSONDecodeError as e:
            print(f"Error: Invalid JSON in configuration file: {e}", file=sys.stderr)
            return None
        except Exception as e:
            print(f"Error reading configuration: {e}", file=sys.stderr)
            return None
    
    def get_value(self, key_path: str) -> int:
        """Get a specific configuration value by dot notation path."""
        config = self.load_raw_config()
        if not config:
            return 1
        
        try:
            # Navigate nested dict using dot notation
            keys = key_path.split('.')
            value = config
            
            for key in keys:
                if isinstance(value, dict) and key in value:
                    value = value[key]
                else:
                    print(f"Error: Configuration key '{key_path}' not found", file=sys.stderr)
                    return 1
            
            # Output the value
            if isinstance(value, (str, int, float, bool)):
                print(value)
                return 0
            else:
                print(json.dumps(value))
                return 0
                
        except Exception as e:
            print(f"Error getting configuration value: {e}", file=sys.stderr)
            return 1
    
    def show_all(self) -> int:
        """Show complete configuration as JSON."""
        config = self.load_raw_config()
        if not config:
            return 1
        
        try:
            print(json.dumps(config, indent=2))
            return 0
        except Exception as e:
            print(f"Error formatting configuration: {e}", file=sys.stderr)
            return 1
    
    def show_sync_config(self) -> int:
        """Show sync-specific configuration for shell script integration."""
        config = self.load_raw_config()
        if not config:
            return 1
        
        try:
            sync_config = config.get('workflow', {}).get('sync', {})
            
            # Output shell-compatible format
            print(f"SYNC_STRATEGY='{sync_config.get('strategy', 'comprehensive')}'")
            print(f"AUTO_COMMIT='{str(sync_config.get('autoCommit', True)).lower()}'")
            print(f"GENERATE_COMMIT_MESSAGES='{str(sync_config.get('generateCommitMessages', True)).lower()}'")
            print(f"PUSH_ON_SYNC='{str(sync_config.get('pushOnSync', True)).lower()}'")
            print(f"BACKUP_CONFIGS='{str(sync_config.get('backupConfigs', True)).lower()}'")
            
            return 0
            
        except Exception as e:
            print(f"Error getting sync configuration: {e}", file=sys.stderr)
            return 1
    
    def show_agent_config(self) -> int:
        """Show agent configuration for coordination scripts."""
        config = self.load_raw_config()
        if not config:
            return 1
        
        try:
            agent_config = config.get('agents', {})
            
            # Output shell-compatible format
            print(f"MAX_CONCURRENT_AGENTS='{agent_config.get('maxConcurrent', 3)}'")
            print(f"COORDINATION_STRATEGY='{agent_config.get('coordinationStrategy', 'hierarchical')}'")
            print(f"DEFAULT_MODEL='{agent_config.get('defaultModel', 'claude-sonnet-4')}'")
            print(f"MEMORY_SYSTEM='{agent_config.get('memorySystem', 'dual')}'")
            print(f"AGENT_TIMEOUT='{agent_config.get('agentTimeout', '30m')}'")
            print(f"CROSS_AGENT_SHARING='{str(agent_config.get('crossAgentSharing', True)).lower()}'")
            
            return 0
            
        except Exception as e:
            print(f"Error getting agent configuration: {e}", file=sys.stderr)
            return 1
    
    def show_workflow_config(self) -> int:
        """Show workflow configuration for development scripts."""
        config = self.load_raw_config()
        if not config:
            return 1
        
        try:
            workflow = config.get('workflow', {})
            planning = workflow.get('planning', {})
            development = workflow.get('development', {})
            
            # Output shell-compatible format
            print(f"AUTO_NUMBERING='{str(planning.get('autoNumbering', True)).lower()}'")
            print(f"PLAN_PREFIX='{planning.get('planPrefix', 'FlowLoom')}'")
            print(f"PHASE_STRUCTURE='{planning.get('phaseStructure', 'hierarchical')}'")
            print(f"VERBOSE_LOGGING='{str(development.get('verboseLogging', False)).lower()}'")
            print(f"PROGRESS_REPORTING='{str(development.get('progressReporting', True)).lower()}'")
            print(f"ERROR_RECOVERY='{development.get('errorRecovery', 'prompt')}'")
            
            return 0
            
        except Exception as e:
            print(f"Error getting workflow configuration: {e}", file=sys.stderr)
            return 1


def main():
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        description="FlowLoom Configuration Access (Simple)",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s get workflow.sync.strategy
  %(prog)s show --all
  %(prog)s sync-config
  %(prog)s agent-config
  %(prog)s workflow-config
        """
    )
    
    subparsers = parser.add_subparsers(dest='command', help='Available commands')
    
    # Get command
    get_parser = subparsers.add_parser('get', help='Get configuration value')
    get_parser.add_argument('key', help='Configuration key in dot notation (e.g., workflow.sync.strategy)')
    
    # Show command
    show_parser = subparsers.add_parser('show', help='Show configuration')
    show_parser.add_argument('--all', action='store_true', help='Show complete configuration')
    
    # Sync config command
    subparsers.add_parser('sync-config', help='Show sync configuration for shell scripts')
    
    # Agent config command  
    subparsers.add_parser('agent-config', help='Show agent configuration for coordination')
    
    # Workflow config command
    subparsers.add_parser('workflow-config', help='Show workflow configuration for development')
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        return 1
    
    cli = SimpleConfigurationAccess()
    
    if args.command == 'get':
        return cli.get_value(args.key)
    elif args.command == 'show':
        if args.all:
            return cli.show_all()
        else:
            parser.print_help()
            return 1
    elif args.command == 'sync-config':
        return cli.show_sync_config()
    elif args.command == 'agent-config':
        return cli.show_agent_config()
    elif args.command == 'workflow-config':
        return cli.show_workflow_config()
    else:
        parser.print_help()
        return 1


if __name__ == '__main__':
    sys.exit(main())