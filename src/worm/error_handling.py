"""
WORM Error Handling and Recovery

Provides robust error handling for git operations and graceful
degradation when auto-commit fails, maintaining governance continuity.
"""

import subprocess
import tempfile
import os
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple


class WORMErrorHandler:
    """
    Handles errors and provides recovery options for WORM auto-commit system.
    
    Ensures governance continuity even when technical issues prevent
    automatic commits, providing fallback options and user guidance.
    """
    
    def __init__(self, project_root: str = "."):
        self.project_root = Path(project_root)
        self.error_log = []
        
    def handle_commit_failure(self, commit_data: Dict[str, Any], 
                            error: Exception) -> Dict[str, Any]:
        """
        Handle auto-commit failures gracefully with recovery options.
        
        Args:
            commit_data: Original commit data that failed
            error: Exception that caused the failure
            
        Returns:
            dict: Recovery guidance and fallback options
        """
        failure_reason = str(error)
        interaction_summary = commit_data.get('interaction_summary', {})
        files_affected = commit_data.get('files_to_commit', [])
        
        # Log failure details
        failure_record = self._log_auto_commit_failure(
            interaction_summary.get('interaction_id'), 
            failure_reason, 
            files_affected
        )
        
        # Determine failure type and provide specific guidance
        failure_type = self._classify_failure(failure_reason)
        recovery_options = self._generate_recovery_options(failure_type, commit_data)
        
        # Create manual commit template
        manual_commit_file = self._create_manual_commit_template(commit_data)
        
        result = {
            'success': False,
            'operation': 'worm_auto_commit_recovery',
            'failure_type': failure_type,
            'failure_reason': failure_reason,
            'interaction_id': interaction_summary.get('interaction_id'),
            'files_affected': files_affected,
            'recovery_options': recovery_options,
            'manual_commit_template': manual_commit_file,
            'governance_status': 'requires_manual_intervention',
            'timestamp': datetime.now().isoformat()
        }
        
        # Provide user notification
        self._notify_user_of_failure(result)
        
        return result
    
    def _classify_failure(self, error_message: str) -> str:
        """Classify failure type for appropriate recovery guidance."""
        error_lower = error_message.lower()
        
        if 'permission denied' in error_lower or 'access denied' in error_lower:
            return 'permission_error'
        elif 'not a git repository' in error_lower:
            return 'not_git_repo'
        elif 'merge conflict' in error_lower or 'conflict' in error_lower:
            return 'merge_conflict'
        elif 'network' in error_lower or 'connection' in error_lower:
            return 'network_error'
        elif 'push rejected' in error_lower or 'rejected' in error_lower:
            return 'push_rejected'
        elif 'uncommitted changes' in error_lower:
            return 'uncommitted_changes'
        elif 'nothing to commit' in error_lower:
            return 'nothing_to_commit'
        else:
            return 'unknown_error'
    
    def _generate_recovery_options(self, failure_type: str, 
                                 commit_data: Dict[str, Any]) -> List[Dict[str, str]]:
        """Generate specific recovery options based on failure type."""
        files_affected = commit_data.get('files_to_commit', [])
        
        base_options = []
        
        if failure_type == 'permission_error':
            base_options.extend([
                {
                    'option': 'check_git_permissions',
                    'description': 'Check git repository permissions',
                    'command': 'ls -la .git && git config --list'
                },
                {
                    'option': 'manual_commit_with_sudo',
                    'description': 'Try manual commit with proper permissions',
                    'command': f'git add {" ".join(files_affected)} && git commit -F tmp/commit-msg.txt'
                }
            ])
        
        elif failure_type == 'not_git_repo':
            base_options.extend([
                {
                    'option': 'initialize_git_repo',
                    'description': 'Initialize git repository',
                    'command': 'git init && git add . && git commit -F tmp/commit-msg.txt'
                }
            ])
        
        elif failure_type == 'merge_conflict':
            base_options.extend([
                {
                    'option': 'resolve_conflicts',
                    'description': 'Resolve merge conflicts manually',
                    'command': 'git status # Check conflicts, then git add . && git commit -F tmp/commit-msg.txt'
                }
            ])
        
        elif failure_type == 'network_error':
            base_options.extend([
                {
                    'option': 'commit_locally',
                    'description': 'Commit locally, push later',
                    'command': f'git add {" ".join(files_affected)} && git commit -F tmp/commit-msg.txt'
                },
                {
                    'option': 'retry_push',
                    'description': 'Retry push when network available',
                    'command': 'git push'
                }
            ])
        
        elif failure_type == 'push_rejected':
            base_options.extend([
                {
                    'option': 'pull_and_retry',
                    'description': 'Pull remote changes and retry',
                    'command': 'git pull && git push'
                }
            ])
        
        # Universal fallback options
        base_options.extend([
            {
                'option': 'manual_commit',
                'description': 'Manual commit with prepared message',
                'command': f'git add {" ".join(files_affected)} && git commit -F tmp/commit-msg.txt && git push'
            },
            {
                'option': 'skip_governance',
                'description': 'Continue without commit (breaks governance)',
                'command': '# WARNING: This breaks WORM governance compliance'
            }
        ])
        
        return base_options
    
    def _create_manual_commit_template(self, commit_data: Dict[str, Any]) -> str:
        """Create manual commit message template file."""
        interaction_summary = commit_data.get('interaction_summary', {})
        files_to_commit = commit_data.get('files_to_commit', [])
        reasoning = commit_data.get('reasoning', 'no reasoning provided')
        memory_snapshot = commit_data.get('memory_snapshot', {})
        
        # Create commit message content
        files_summary = self._summarize_file_changes(files_to_commit)
        
        # Detect plan context for traceability
        plan_path = self._detect_plan_context(files_to_commit, reasoning)
        plan_footer = f"\nPlan: {plan_path}" if plan_path else ""
        
        commit_content = f"""feat: {files_summary}

User Request: {reasoning}

Files Changed:
{self._format_files_list(files_to_commit)}

WORM Memory Context:
Interaction: {interaction_summary.get('interaction_id', 'unknown')}
Summary: {memory_snapshot.get('summary', 'no memory context available')}

RECOVERY NOTE: This commit was created manually due to auto-commit failure.
WORM governance continuity maintained through manual intervention.

🤖 Generated with FlowLoom WORM Development Environment
Auto-tracked governance record with complete reasoning preservation{plan_footer}

Co-Authored-By: FlowLoom <flowloom@rodk.dev>"""
        
        # Write to temporary commit file
        commit_file_path = self.project_root / 'tmp' / 'commit-msg.txt'
        commit_file_path.parent.mkdir(exist_ok=True)
        
        with open(commit_file_path, 'w', encoding='utf-8') as f:
            f.write(commit_content)
        
        return str(commit_file_path)
    
    def _summarize_file_changes(self, files: List[str]) -> str:
        """Create concise summary of file changes."""
        if not files:
            return "update project files"
        
        if len(files) == 1:
            file_path = files[0]
            if 'implementation' in file_path.lower():
                return f"implement {Path(file_path).stem}"
            elif 'test' in file_path.lower():
                return f"add tests for {Path(file_path).stem}"
            elif file_path.endswith('.md'):
                return f"update {Path(file_path).stem} documentation"
            else:
                return f"update {Path(file_path).name}"
        
        # Multiple files
        return f"update {len(files)} files"
    
    def _format_files_list(self, files: List[str]) -> str:
        """Format files list for commit message."""
        if not files:
            return "  (no files)"
        
        formatted_files = []
        for file_path in files:
            formatted_files.append(f"  - {file_path}")
        
        return "\n".join(formatted_files)
    
    def _detect_plan_context(self, files_changed: List[str], reasoning: str) -> Optional[str]:
        """
        Detect the current plan context for commit traceability.
        
        Args:
            files_changed: List of files being committed
            reasoning: User request/reasoning text
            
        Returns:
            str: Relative path to plan file if detected, None otherwise
        """
        # Check if any changed files are in plans directory
        for file_path in files_changed:
            if file_path.startswith('plans/') and file_path.endswith('.md'):
                return file_path
        
        # Check if reasoning mentions a specific plan
        reasoning_lower = reasoning.lower()
        
        # Look for plan references in reasoning (prioritize specific matches)
        if 'worm' in reasoning_lower:
            return 'plans/FlowLoom/worm-development/410_worm-development_implementation_interaction-level-auto-commit.md'
        
        # Look for other plan patterns
        if 'multi-claude' in reasoning_lower or 'coordination' in reasoning_lower:
            return 'plans/FlowLoom/multi-claude-coordination/'
        
        if 'installer' in reasoning_lower:
            return 'plans/FlowLoom/installer-system/'
        
        if 'session' in reasoning_lower and 'management' in reasoning_lower:
            return 'plans/FlowLoom/session-management/'
        
        if 'memory' in reasoning_lower and ('monitor' in reasoning_lower or 'system' in reasoning_lower):
            return 'plans/FlowLoom/memory-monitor/'
        
        # Check if any plan files exist that might be relevant (more specific patterns first)
        plan_patterns = [
            ('docker', 'plans/FlowLoom/docker-environment/'),
            ('test', 'plans/FlowLoom/test-sandbox/'),
            ('config', 'plans/FlowLoom/configuration-system/'),
            ('extension', 'plans/FlowLoom/extension-system/'),
            ('website', 'plans/FlowLoom/website-management/'),
            ('plan management', 'plans/FlowLoom/plan-management-system/'),  # More specific
        ]
        
        for keyword, plan_path in plan_patterns:
            if keyword in reasoning_lower:
                return plan_path
        
        return None
    
    def _log_auto_commit_failure(self, interaction_id: str, 
                               failure_reason: str, 
                               files_affected: List[str]) -> Dict[str, Any]:
        """Log auto-commit failure for debugging and audit trail."""
        failure_record = {
            'timestamp': datetime.now().isoformat(),
            'interaction_id': interaction_id,
            'failure_reason': failure_reason,
            'files_affected': files_affected,
            'event_type': 'worm_auto_commit_failure'
        }
        
        self.error_log.append(failure_record)
        
        # Could also write to a persistent error log file
        error_log_path = self.project_root / 'tmp' / 'worm_errors.log'
        error_log_path.parent.mkdir(exist_ok=True)
        
        with open(error_log_path, 'a', encoding='utf-8') as f:
            f.write(f"{failure_record['timestamp']}: {failure_reason}\n")
        
        return failure_record
    
    def _notify_user_of_failure(self, failure_result: Dict[str, Any]):
        """Provide user notification of auto-commit failure."""
        print(f"⚠️  WORM AUTO-COMMIT FAILED: {failure_result['failure_type']}")
        print(f"   Reason: {failure_result['failure_reason']}")
        print(f"   Interaction: {failure_result['interaction_id']}")
        print(f"   Files affected: {len(failure_result['files_affected'])} files")
        print()
        print("GOVERNANCE CONTINUITY OPTIONS:")
        
        for i, option in enumerate(failure_result['recovery_options'][:3], 1):
            print(f"  {i}. {option['description']}")
            print(f"     Command: {option['command']}")
            print()
        
        print(f"Manual commit template created: {failure_result['manual_commit_template']}")
        print("Use: git commit -F tmp/commit-msg.txt")
    
    def attempt_recovery(self, failure_result: Dict[str, Any], 
                        recovery_option: str) -> Dict[str, Any]:
        """
        Attempt automatic recovery for certain failure types.
        
        Args:
            failure_result: Previous failure result
            recovery_option: Selected recovery option
            
        Returns:
            dict: Recovery attempt result
        """
        if recovery_option == 'commit_locally':
            return self._attempt_local_commit(failure_result)
        elif recovery_option == 'pull_and_retry':
            return self._attempt_pull_and_retry(failure_result)
        else:
            return {
                'success': False,
                'message': f'Recovery option {recovery_option} requires manual intervention',
                'requires_manual': True
            }
    
    def _attempt_local_commit(self, failure_result: Dict[str, Any]) -> Dict[str, Any]:
        """Attempt local commit without push."""
        try:
            files_affected = failure_result['files_affected']
            commit_template = failure_result['manual_commit_template']
            
            # Stage files
            for file_path in files_affected:
                subprocess.run(['git', 'add', file_path], 
                             cwd=self.project_root, check=True)
            
            # Commit with template file using -F option
            subprocess.run(['git', 'commit', '-F', commit_template],
                         cwd=self.project_root, check=True)
            
            return {
                'success': True,
                'message': 'Local commit successful - push separately when ready',
                'governance_status': 'locally_preserved',
                'requires_push': True
            }
            
        except subprocess.CalledProcessError as e:
            return {
                'success': False,
                'message': f'Local commit failed: {e}',
                'requires_manual': True
            }
    
    def _attempt_pull_and_retry(self, failure_result: Dict[str, Any]) -> Dict[str, Any]:
        """Attempt pull then retry full commit."""
        try:
            # Pull first
            subprocess.run(['git', 'pull'], cwd=self.project_root, check=True)
            
            # Then attempt local commit
            local_result = self._attempt_local_commit(failure_result)
            
            if local_result['success']:
                # Try push
                subprocess.run(['git', 'push'], cwd=self.project_root, check=True)
                
                return {
                    'success': True,
                    'message': 'Pull and retry successful - full governance preserved',
                    'governance_status': 'fully_preserved'
                }
            else:
                return local_result
                
        except subprocess.CalledProcessError as e:
            return {
                'success': False,
                'message': f'Pull and retry failed: {e}',
                'requires_manual': True
            }


class WORMRetryManager:
    """
    Manages retry logic for temporary failures in WORM auto-commit system.
    """
    
    def __init__(self, max_retries: int = 3, retry_delay: float = 1.0):
        self.max_retries = max_retries
        self.retry_delay = retry_delay
        
    def with_retry(self, operation_func, *args, **kwargs) -> Dict[str, Any]:
        """
        Execute operation with retry logic for temporary failures.
        
        Args:
            operation_func: Function to execute
            *args: Arguments for operation function
            **kwargs: Keyword arguments for operation function
            
        Returns:
            dict: Operation result with retry information
        """
        last_error = None
        
        for attempt in range(self.max_retries):
            try:
                result = operation_func(*args, **kwargs)
                
                if result.get('success'):
                    if attempt > 0:
                        result['retry_info'] = {
                            'attempts': attempt + 1,
                            'succeeded_on_retry': True
                        }
                    return result
                else:
                    # Operation failed, but may be retryable
                    if self._is_retryable_error(result):
                        last_error = result
                        if attempt < self.max_retries - 1:
                            time.sleep(self.retry_delay)
                            continue
                    else:
                        # Non-retryable error
                        return result
                        
            except Exception as e:
                last_error = {'success': False, 'error': str(e)}
                if attempt < self.max_retries - 1:
                    time.sleep(self.retry_delay)
                    continue
        
        # All retries exhausted
        if last_error:
            last_error['retry_info'] = {
                'attempts': self.max_retries,
                'all_retries_failed': True
            }
            return last_error
        
        return {'success': False, 'error': 'Unknown error after retries'}
    
    def _is_retryable_error(self, result: Dict[str, Any]) -> bool:
        """Determine if error is worth retrying."""
        error_message = result.get('message', '').lower()
        
        # Retryable errors (usually temporary)
        retryable_patterns = [
            'network',
            'connection',
            'timeout',
            'temporary',
            'busy',
            'lock'
        ]
        
        for pattern in retryable_patterns:
            if pattern in error_message:
                return True
        
        return False