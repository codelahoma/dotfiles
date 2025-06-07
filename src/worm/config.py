"""
WORM Configuration and Control

Provides user controls for WORM auto-commit behavior while maintaining
governance objectives and providing configuration options.
"""

import json
import os
from pathlib import Path
from typing import Dict, Any, Optional
from datetime import datetime


class WORMConfig:
    """
    Configuration management for WORM development environment.
    
    Controls auto-commit behavior, governance compliance settings,
    and integration with auto-tracking system.
    """
    
    def __init__(self, project_root: str = ".", config_file: str = ".worm_config.json"):
        self.project_root = Path(project_root)
        self.config_file_path = self.project_root / config_file
        self.config = self._load_config()
        
    def _load_config(self) -> Dict[str, Any]:
        """Load WORM configuration from file or create defaults."""
        default_config = {
            'auto_commit_enabled': False,  # Default: disabled for safety
            'require_explicit_enable': True,  # Prevent accidental enabling
            'dry_run_mode': False,
            'commit_message_template': 'default',
            'memory_snapshot_enabled': True,
            'plan_path_detection': True,
            'auto_push_enabled': True,
            'file_exclusions': ['*.log', '*.tmp', 'tmp/*'],
            'governance_mode': 'strict',  # 'strict', 'permissive', 'disabled'
            'integration': {
                'auto_track_required': True,  # Require auto-track for WORM
                'memory_system_required': True,
                'git_hooks_enabled': False
            },
            'last_updated': datetime.now().isoformat(),
            'created_by': 'FlowLoom WORM System'
        }
        
        try:
            if self.config_file_path.exists():
                with open(self.config_file_path, 'r', encoding='utf-8') as f:
                    loaded_config = json.load(f)
                    
                # Merge with defaults to ensure all keys exist
                merged_config = default_config.copy()
                merged_config.update(loaded_config)
                return merged_config
            else:
                # Create default config file
                self._save_config(default_config)
                return default_config
                
        except (json.JSONDecodeError, IOError) as e:
            print(f"WORM Config: Warning - Could not load config: {e}")
            print("WORM Config: Using default configuration")
            return default_config
    
    def _save_config(self, config: Dict[str, Any] = None) -> bool:
        """Save configuration to file."""
        if config is None:
            config = self.config
            
        try:
            config['last_updated'] = datetime.now().isoformat()
            
            with open(self.config_file_path, 'w', encoding='utf-8') as f:
                json.dump(config, f, indent=2, sort_keys=True)
            
            return True
            
        except IOError as e:
            print(f"WORM Config: Error saving config: {e}")
            return False
    
    def is_auto_commit_enabled(self) -> bool:
        """Check if auto-commit is currently enabled."""
        return self.config.get('auto_commit_enabled', False)
    
    def enable_auto_commit(self, reason: str = "Manual enable") -> Dict[str, Any]:
        """
        Enable auto-commit with explicit reasoning.
        
        Args:
            reason: Reason for enabling auto-commit
            
        Returns:
            dict: Result of enable operation
        """
        if self.config.get('require_explicit_enable', True):
            print("⚠️  ENABLING WORM AUTO-COMMIT")
            print(f"Reason: {reason}")
            print("This will automatically commit and push all file-producing interactions.")
            print("WORM governance audit trail will be activated.")
            
            # In a real implementation, might want user confirmation
            # For now, proceed with enable
            
        self.config['auto_commit_enabled'] = True
        self.config['enabled_at'] = datetime.now().isoformat()
        self.config['enabled_reason'] = reason
        
        if self._save_config():
            return {
                'success': True,
                'message': f'WORM auto-commit enabled: {reason}',
                'governance_status': 'active'
            }
        else:
            return {
                'success': False,
                'message': 'Failed to save WORM config',
                'governance_status': 'unknown'
            }
    
    def disable_auto_commit(self, reason: str) -> Dict[str, Any]:
        """
        Disable auto-commit with explicit reasoning.
        
        Args:
            reason: Reason for disabling auto-commit
            
        Returns:
            dict: Result of disable operation
        """
        if self.config.get('require_explicit_disable', True):
            print("⚠️  DISABLING WORM AUTO-COMMIT")
            print(f"Reason: {reason}")
            print("Governance audit trail will be interrupted.")
            print("Manual commits will be required to maintain compliance.")
            
        self.config['auto_commit_enabled'] = False
        self.config['disabled_at'] = datetime.now().isoformat()
        self.config['disabled_reason'] = reason
        
        if self._save_config():
            return {
                'success': True,
                'message': f'WORM auto-commit disabled: {reason}',
                'governance_status': 'interrupted'
            }
        else:
            return {
                'success': False,
                'message': 'Failed to save WORM config',
                'governance_status': 'unknown'
            }
    
    def toggle_auto_commit(self, reason: str = "Manual toggle") -> Dict[str, Any]:
        """Toggle auto-commit state."""
        if self.is_auto_commit_enabled():
            return self.disable_auto_commit(reason)
        else:
            return self.enable_auto_commit(reason)
    
    def set_dry_run_mode(self, enabled: bool) -> Dict[str, Any]:
        """Enable or disable dry-run mode for testing."""
        self.config['dry_run_mode'] = enabled
        
        if self._save_config():
            mode = "enabled" if enabled else "disabled"
            return {
                'success': True,
                'message': f'WORM dry-run mode {mode}',
                'dry_run': enabled
            }
        else:
            return {
                'success': False,
                'message': 'Failed to save dry-run setting'
            }
    
    def get_file_exclusions(self) -> list:
        """Get list of file patterns to exclude from auto-commit."""
        return self.config.get('file_exclusions', [])
    
    def add_file_exclusion(self, pattern: str) -> bool:
        """Add file pattern to exclusion list."""
        exclusions = self.get_file_exclusions()
        if pattern not in exclusions:
            exclusions.append(pattern)
            self.config['file_exclusions'] = exclusions
            return self._save_config()
        return True
    
    def remove_file_exclusion(self, pattern: str) -> bool:
        """Remove file pattern from exclusion list."""
        exclusions = self.get_file_exclusions()
        if pattern in exclusions:
            exclusions.remove(pattern)
            self.config['file_exclusions'] = exclusions
            return self._save_config()
        return True
    
    def should_exclude_file(self, file_path: str) -> bool:
        """Check if file should be excluded from auto-commit."""
        import fnmatch
        
        exclusions = self.get_file_exclusions()
        for pattern in exclusions:
            if fnmatch.fnmatch(file_path, pattern):
                return True
        
        # Always exclude memory.json from auto-commit triggers
        if file_path.endswith('memory.json'):
            return True
            
        # Always exclude WORM config file
        if file_path.endswith('.worm_config.json'):
            return True
            
        return False
    
    def get_governance_mode(self) -> str:
        """Get current governance mode."""
        return self.config.get('governance_mode', 'strict')
    
    def set_governance_mode(self, mode: str) -> Dict[str, Any]:
        """
        Set governance mode.
        
        Args:
            mode: 'strict', 'permissive', or 'disabled'
            
        Returns:
            dict: Result of mode change
        """
        valid_modes = ['strict', 'permissive', 'disabled']
        
        if mode not in valid_modes:
            return {
                'success': False,
                'message': f'Invalid governance mode: {mode}. Must be one of: {valid_modes}'
            }
        
        self.config['governance_mode'] = mode
        
        if self._save_config():
            return {
                'success': True,
                'message': f'WORM governance mode set to: {mode}',
                'governance_mode': mode
            }
        else:
            return {
                'success': False,
                'message': 'Failed to save governance mode setting'
            }
    
    def validate_prerequisites(self) -> Dict[str, Any]:
        """
        Validate that WORM system prerequisites are met.
        
        Returns:
            dict: Validation result with details
        """
        issues = []
        requirements_met = True
        
        # Check if we're in a git repository
        if not (self.project_root / '.git').exists():
            issues.append('Not in a git repository')
            requirements_met = False
        
        # Check if auto-track integration is required and available
        if self.config.get('integration', {}).get('auto_track_required', True):
            # Check for auto-track availability (simplified check)
            auto_track_cmd = self.project_root / '.claude' / 'commands' / 'flowloom' / 'system' / 'auto-track.md'
            if not auto_track_cmd.exists():
                issues.append('Auto-track command not available')
                requirements_met = False
        
        # Check if memory system is required and available
        if self.config.get('integration', {}).get('memory_system_required', True):
            memory_file = self.project_root / 'memory.json'
            if not memory_file.exists():
                issues.append('Memory system (memory.json) not available')
                requirements_met = False
        
        return {
            'prerequisites_met': requirements_met,
            'issues': issues,
            'auto_commit_ready': requirements_met and self.is_auto_commit_enabled(),
            'governance_mode': self.get_governance_mode()
        }
    
    def get_status_report(self) -> Dict[str, Any]:
        """Get comprehensive WORM system status."""
        validation = self.validate_prerequisites()
        
        return {
            'auto_commit_enabled': self.is_auto_commit_enabled(),
            'dry_run_mode': self.config.get('dry_run_mode', False),
            'governance_mode': self.get_governance_mode(),
            'prerequisites_met': validation['prerequisites_met'],
            'issues': validation['issues'],
            'file_exclusions_count': len(self.get_file_exclusions()),
            'config_file': str(self.config_file_path),
            'last_updated': self.config.get('last_updated'),
            'integration_settings': self.config.get('integration', {})
        }
    
    def export_config(self) -> Dict[str, Any]:
        """Export complete configuration for debugging."""
        return self.config.copy()
    
    def reset_to_defaults(self) -> Dict[str, Any]:
        """Reset configuration to defaults."""
        if self.config_file_path.exists():
            # Backup existing config
            backup_path = self.config_file_path.with_suffix('.json.bak')
            self.config_file_path.rename(backup_path)
            
        # Reload defaults
        self.config = self._load_config()
        
        return {
            'success': True,
            'message': 'WORM configuration reset to defaults',
            'backup_created': str(backup_path) if 'backup_path' in locals() else None
        }