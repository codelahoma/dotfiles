#!/usr/bin/env python3
"""FlowLoom Configuration Management CLI.

This script provides command-line access to FlowLoom configuration
for integration with shell scripts and workflow commands.
"""

import sys
import json
import argparse
from pathlib import Path
from typing import Optional, Dict, Any

# Add src to path for configuration imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

try:
    from configuration import load_config, ConfigurationLoader, get_loader
except ImportError as e:
    print(f"Error: Could not import FlowLoom configuration system: {e}", file=sys.stderr)
    print("Make sure you're running from the FlowLoom project root.", file=sys.stderr)
    sys.exit(1)


class ConfigurationCLI:
    """Command-line interface for FlowLoom configuration."""
    
    def __init__(self):
        self.project_root = Path.cwd()
        
    def load_configuration(self) -> Optional[Dict[str, Any]]:
        """Load FlowLoom configuration."""
        try:
            result = load_config(project_root=self.project_root)
            
            if not result.config:
                print("Error: Failed to load configuration", file=sys.stderr)
                if result.errors:
                    for error in result.errors:
                        print(f"  {error}", file=sys.stderr)
                return None
            
            # Show warnings if any
            if result.validation and result.validation.warnings:
                for warning in result.validation.warnings:
                    print(f"Warning: {warning}", file=sys.stderr)
            
            return result.config.dict()
            
        except Exception as e:
            print(f"Error loading configuration: {e}", file=sys.stderr)
            return None
    
    def get_value(self, key_path: str) -> int:
        """Get a specific configuration value by dot notation path."""
        config = self.load_configuration()
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
        config = self.load_configuration()
        if not config:
            return 1
        
        try:
            print(json.dumps(config, indent=2))
            return 0
        except Exception as e:
            print(f"Error formatting configuration: {e}", file=sys.stderr)
            return 1
    
    def validate(self) -> int:
        """Validate configuration and show any issues."""
        try:
            result = load_config(project_root=self.project_root)
            
            if not result.validation:
                print("Error: Configuration validation failed", file=sys.stderr)
                return 1
            
            validation = result.validation
            
            print(f"Configuration validation: {'✅ VALID' if validation.valid else '❌ INVALID'}")
            
            if validation.errors:
                print("\nErrors:")
                for error in validation.errors:
                    print(f"  ❌ {error}")
            
            if validation.warnings:
                print("\nWarnings:")
                for warning in validation.warnings:
                    print(f"  ⚠️  {warning}")
            
            if validation.deprecated_keys:
                print("\nDeprecated keys:")
                for key in validation.deprecated_keys:
                    print(f"  📅 {key}")
            
            return 0 if validation.valid else 1
            
        except Exception as e:
            print(f"Error validating configuration: {e}", file=sys.stderr)
            return 1
    
    def show_sync_config(self) -> int:
        """Show sync-specific configuration for shell script integration."""
        config = self.load_configuration()
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
        config = self.load_configuration()
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


def main():
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        description="FlowLoom Configuration Management CLI",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s get workflow.sync.strategy
  %(prog)s show --all
  %(prog)s validate
  %(prog)s sync-config
  %(prog)s agent-config
        """
    )
    
    subparsers = parser.add_subparsers(dest='command', help='Available commands')
    
    # Get command
    get_parser = subparsers.add_parser('get', help='Get configuration value')
    get_parser.add_argument('key', help='Configuration key in dot notation (e.g., workflow.sync.strategy)')
    
    # Show command
    show_parser = subparsers.add_parser('show', help='Show configuration')
    show_parser.add_argument('--all', action='store_true', help='Show complete configuration')
    
    # Validate command
    subparsers.add_parser('validate', help='Validate configuration')
    
    # Sync config command
    subparsers.add_parser('sync-config', help='Show sync configuration for shell scripts')
    
    # Agent config command  
    subparsers.add_parser('agent-config', help='Show agent configuration for coordination')
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        return 1
    
    cli = ConfigurationCLI()
    
    if args.command == 'get':
        return cli.get_value(args.key)
    elif args.command == 'show':
        if args.all:
            return cli.show_all()
        else:
            parser.print_help()
            return 1
    elif args.command == 'validate':
        return cli.validate()
    elif args.command == 'sync-config':
        return cli.show_sync_config()
    elif args.command == 'agent-config':
        return cli.show_agent_config()
    else:
        parser.print_help()
        return 1


if __name__ == '__main__':
    sys.exit(main())