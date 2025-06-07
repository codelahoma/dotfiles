"""
WORM Orchestrator

Main coordination system that integrates auto-tracking with WORM auto-commit
functionality, providing seamless governance audit trail automation.
"""

import subprocess
import json
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Any, Optional

from .interaction_tracker import WORMInteractionTracker
from .memory_snapshot import WORMMemorySnapshot
from .commit_engine import WORMCommitEngine
from .error_handling import WORMErrorHandler
from .config import WORMConfig


class WORMOrchestrator:
    """
    Main orchestrator for WORM development environment.
    
    Coordinates between auto-tracking, memory snapshots, and auto-commit
    to provide seamless governance audit trail automation.
    """
    
    def __init__(self, project_root: str = "."):
        self.project_root = Path(project_root)
        self.config = WORMConfig(project_root)
        self.tracker = WORMInteractionTracker()
        self.memory_snapshot = WORMMemorySnapshot(project_root)
        self.commit_engine = WORMCommitEngine(project_root, dry_run=self.config.config.get('dry_run_mode', False))
        self.error_handler = WORMErrorHandler(project_root)
        
    def start_interaction(self, user_request: str) -> str:
        """
        Start tracking a new user interaction.
        
        Args:
            user_request: The user's request or command
            
        Returns:
            str: Interaction ID for tracking
        """
        if not self.config.is_auto_commit_enabled():
            return None
            
        interaction_id = self.tracker.start_interaction(user_request)
        return interaction_id
    
    def track_file_change(self, file_path: str, interaction_id: str = None) -> bool:
        """
        Track a file change during interaction.
        
        Args:
            file_path: Path to changed file
            interaction_id: Current interaction ID (optional)
            
        Returns:
            bool: True if file was tracked
        """
        if not self.config.is_auto_commit_enabled():
            return False
            
        # Check if file should be excluded
        if self.config.should_exclude_file(file_path):
            return False
            
        if interaction_id:
            self.tracker.track_file_change(file_path, interaction_id)
            return True
        elif self.tracker.current_interaction:
            self.tracker.track_file_change(file_path)
            return True
            
        return False
    
    def complete_interaction(self, interaction_id: str = None) -> Dict[str, Any]:
        """
        Complete interaction and trigger auto-commit if needed.
        
        Args:
            interaction_id: Interaction ID to complete (optional)
            
        Returns:
            dict: Result of interaction completion
        """
        if not self.config.is_auto_commit_enabled():
            return {
                'success': True,
                'message': 'WORM auto-commit disabled - interaction tracked only',
                'auto_commit': False
            }
        
        # Complete the interaction tracking
        completion_result = self.tracker.complete_interaction(interaction_id)
        
        if not completion_result.get('should_auto_commit', False):
            return {
                'success': True,
                'message': 'No files changed - no auto-commit needed',
                'auto_commit': False,
                'interaction_summary': completion_result
            }
        
        # Prepare commit data
        commit_data = {
            'interaction_summary': completion_result,
            'files_to_commit': completion_result.get('files_changed', []),
            'reasoning': completion_result.get('user_request', 'No reasoning provided'),
            'memory_snapshot': {}
        }
        
        # Validate prerequisites
        validation = self.config.validate_prerequisites()
        if not validation['prerequisites_met']:
            return {
                'success': False,
                'message': f'WORM prerequisites not met: {validation["issues"]}',
                'auto_commit': False,
                'validation': validation
            }
        
        # Execute auto-commit
        try:
            commit_result = self.commit_engine.execute_auto_commit(commit_data)
            
            if commit_result['success']:
                return {
                    'success': True,
                    'message': 'WORM auto-commit completed successfully',
                    'auto_commit': True,
                    'commit_result': commit_result,
                    'interaction_summary': completion_result
                }
            else:
                # Handle commit failure with recovery options
                recovery_result = self.error_handler.handle_commit_failure(commit_data, 
                    Exception(commit_result.get('message', 'Unknown commit error')))
                
                return {
                    'success': False,
                    'message': 'WORM auto-commit failed - recovery options provided',
                    'auto_commit': False,
                    'commit_result': commit_result,
                    'recovery': recovery_result,
                    'interaction_summary': completion_result
                }
                
        except Exception as e:
            # Handle unexpected errors
            recovery_result = self.error_handler.handle_commit_failure(commit_data, e)
            
            return {
                'success': False,
                'message': f'WORM auto-commit error: {str(e)}',
                'auto_commit': False,
                'error': str(e),
                'recovery': recovery_result,
                'interaction_summary': completion_result
            }
    
    def process_auto_track_entry(self, auto_track_entry: str) -> Dict[str, Any]:
        """
        Process an auto-track entry and trigger WORM if needed.
        
        Args:
            auto_track_entry: Auto-track formatted entry
            
        Returns:
            dict: Processing result
        """
        if not self.config.is_auto_commit_enabled():
            return {
                'success': True,
                'message': 'WORM disabled - auto-track entry recorded only',
                'worm_triggered': False
            }
        
        # Parse auto-track entry to extract information
        parsed_entry = self._parse_auto_track_entry(auto_track_entry)
        
        if not parsed_entry:
            return {
                'success': False,
                'message': 'Could not parse auto-track entry',
                'worm_triggered': False
            }
        
        # Check if this entry indicates file changes
        files_changed = parsed_entry.get('files', [])
        if not files_changed:
            return {
                'success': True,
                'message': 'No files changed - no WORM action needed',
                'worm_triggered': False
            }
        
        # Filter out excluded files
        commit_files = [f for f in files_changed if not self.config.should_exclude_file(f)]
        
        if not commit_files:
            return {
                'success': True,
                'message': 'All files excluded - no WORM action needed',
                'worm_triggered': False,
                'excluded_files': files_changed
            }
        
        # Create interaction data from auto-track entry
        interaction_data = {
            'interaction_id': parsed_entry.get('interaction_id', 'auto_track_' + datetime.now().strftime('%Y%m%d_%H%M%S')),
            'user_request': parsed_entry.get('request', 'Auto-tracked interaction'),
            'files_changed': commit_files,
            'tools_used': parsed_entry.get('tools', []),
            'decisions_made': parsed_entry.get('decisions', ''),
            'next_steps': parsed_entry.get('next', '')
        }
        
        # Prepare commit data
        commit_data = {
            'interaction_summary': interaction_data,
            'files_to_commit': commit_files,
            'reasoning': interaction_data['user_request'],
            'memory_snapshot': {}
        }
        
        # Execute auto-commit
        try:
            commit_result = self.commit_engine.execute_auto_commit(commit_data)
            
            return {
                'success': commit_result['success'],
                'message': 'WORM auto-commit triggered by auto-track entry',
                'worm_triggered': True,
                'commit_result': commit_result,
                'interaction_data': interaction_data
            }
            
        except Exception as e:
            recovery_result = self.error_handler.handle_commit_failure(commit_data, e)
            
            return {
                'success': False,
                'message': f'WORM auto-commit failed: {str(e)}',
                'worm_triggered': True,
                'error': str(e),
                'recovery': recovery_result,
                'interaction_data': interaction_data
            }
    
    def _parse_auto_track_entry(self, entry: str) -> Optional[Dict[str, Any]]:
        """
        Parse auto-track entry format.
        
        Expected format:
        "Shell_ID: [PID] - AUTO: [timestamp] | Request: [request] | Tools: [tools] | Files: [files] | Decisions: [decisions] | Next: [next]"
        
        Args:
            entry: Auto-track formatted string
            
        Returns:
            dict: Parsed components or None if parsing fails
        """
        try:
            # Split on main delimiters
            if ' - AUTO: ' not in entry or ' | ' not in entry:
                return None
            
            # Extract shell info and main content
            shell_part, content = entry.split(' - AUTO: ', 1)
            shell_id = shell_part.replace('Shell_ID: ', '')
            
            # Split content by | delimiter
            parts = content.split(' | ')
            
            parsed = {
                'shell_id': shell_id,
                'files': [],
                'tools': [],
                'request': '',
                'decisions': '',
                'next': ''
            }
            
            for part in parts:
                if part.startswith('Request: '):
                    parsed['request'] = part[9:]
                elif part.startswith('Tools: '):
                    tools_str = part[7:]
                    parsed['tools'] = [t.strip() for t in tools_str.split(',') if t.strip()]
                elif part.startswith('Files: '):
                    files_str = part[7:]
                    parsed['files'] = [f.strip() for f in files_str.split(',') if f.strip()]
                elif part.startswith('Decisions: '):
                    parsed['decisions'] = part[11:]
                elif part.startswith('Next: '):
                    parsed['next'] = part[6:]
            
            return parsed
            
        except Exception:
            return None
    
    def get_status(self) -> Dict[str, Any]:
        """Get comprehensive WORM system status."""
        config_status = self.config.get_status_report()
        
        # Get git repository status
        try:
            git_result = subprocess.run(['git', 'status', '--porcelain'], 
                                      cwd=self.project_root, 
                                      capture_output=True, text=True, check=True)
            
            uncommitted_files = []
            if git_result.stdout.strip():
                for line in git_result.stdout.strip().split('\n'):
                    if line.strip():
                        status = line[:2]
                        filename = line[3:]
                        uncommitted_files.append({'status': status, 'file': filename})
            
            git_status = {
                'is_git_repo': True,
                'uncommitted_files': uncommitted_files,
                'has_uncommitted_changes': len(uncommitted_files) > 0
            }
            
        except subprocess.CalledProcessError:
            git_status = {
                'is_git_repo': False,
                'error': 'Not a git repository or git not available'
            }
        
        return {
            'worm_config': config_status,
            'git_status': git_status,
            'current_interaction': self.tracker.current_interaction,
            'system_ready': (config_status['prerequisites_met'] and 
                           config_status['auto_commit_enabled'] and 
                           git_status.get('is_git_repo', False))
        }
    
    def enable_worm(self, reason: str = "Manual enable") -> Dict[str, Any]:
        """Enable WORM auto-commit system."""
        return self.config.enable_auto_commit(reason)
    
    def disable_worm(self, reason: str = "Manual disable") -> Dict[str, Any]:
        """Disable WORM auto-commit system."""
        return self.config.disable_auto_commit(reason)
    
    def toggle_worm(self, reason: str = "Manual toggle") -> Dict[str, Any]:
        """Toggle WORM auto-commit system."""
        return self.config.toggle_auto_commit(reason)