"""
WORM Commit Engine

Executes git commit and push operations with proper error handling
and FlowLoom identity integration for WORM governance compliance.
"""

import subprocess
import json
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple

from .memory_snapshot import WORMMemorySnapshot


class WORMCommitEngine:
    """
    Executes git operations for WORM auto-commit system.
    
    Implements the actual commit and push operations with proper error
    handling, FlowLoom identity integration, and governance compliance.
    """
    
    def __init__(self, project_root: str = ".", dry_run: bool = False):
        self.project_root = Path(project_root)
        self.dry_run = dry_run
        self.memory_snapshot = WORMMemorySnapshot(project_root)
        
    def execute_auto_commit(self, commit_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Execute git commit with memory context and FlowLoom identity.
        
        Args:
            commit_data: Data from interaction tracker including files and reasoning
            
        Returns:
            dict: Result of commit operation with status and details
        """
        try:
            # Capture memory snapshot for commit context
            memory_context = self._capture_memory_context(commit_data)
            
            # Stage changed files
            staging_result = self._stage_files(commit_data['files_to_commit'])
            if not staging_result['success']:
                return staging_result
            
            # Generate commit message with memory context
            commit_message = self._generate_worm_commit_message(commit_data, memory_context)
            
            # Execute commit with FlowLoom identity
            commit_result = self._execute_git_commit(commit_message)
            if not commit_result['success']:
                return commit_result
            
            # Auto-push for governance preservation
            push_result = self._execute_git_push()
            if not push_result['success']:
                return push_result
            
            # Log successful auto-commit
            success_result = self._create_success_result(commit_data, commit_result, push_result)
            self._log_auto_commit_success(success_result)
            
            return success_result
            
        except Exception as e:
            error_result = self._create_error_result(commit_data, str(e))
            self._log_auto_commit_failure(error_result)
            return error_result
    
    def _capture_memory_context(self, commit_data: Dict[str, Any]) -> Dict[str, Any]:
        """Capture memory snapshot for commit context."""
        interaction_summary = commit_data.get('interaction_summary', {})
        interaction_id = interaction_summary.get('interaction_id', 'unknown')
        user_request = commit_data.get('reasoning', 'no request recorded')
        
        return self.memory_snapshot.capture_interaction_snapshot(
            interaction_id, user_request
        )
    
    def _stage_files(self, files_to_commit: List[str]) -> Dict[str, Any]:
        """
        Stage files for commit.
        
        Args:
            files_to_commit: List of file paths to stage
            
        Returns:
            dict: Result of staging operation
        """
        if self.dry_run:
            return {
                'success': True,
                'operation': 'stage_files',
                'message': f'DRY RUN: Would stage {len(files_to_commit)} files',
                'files': files_to_commit
            }
        
        try:
            # Check if files exist before staging
            existing_files = []
            missing_files = []
            
            for file_path in files_to_commit:
                full_path = self.project_root / file_path
                if full_path.exists():
                    existing_files.append(file_path)
                else:
                    missing_files.append(file_path)
            
            if missing_files:
                return {
                    'success': False,
                    'operation': 'stage_files',
                    'message': f'Files not found: {missing_files}',
                    'missing_files': missing_files
                }
            
            # Stage existing files
            for file_path in existing_files:
                result = subprocess.run(
                    ['git', 'add', file_path],
                    cwd=self.project_root,
                    capture_output=True,
                    text=True,
                    check=True
                )
            
            return {
                'success': True,
                'operation': 'stage_files',
                'message': f'Staged {len(existing_files)} files',
                'files': existing_files
            }
            
        except subprocess.CalledProcessError as e:
            return {
                'success': False,
                'operation': 'stage_files',
                'message': f'Git add failed: {e.stderr}',
                'error': str(e),
                'files': files_to_commit
            }
    
    def _generate_worm_commit_message(self, commit_data: Dict[str, Any], 
                                    memory_context: Dict[str, Any]) -> str:
        """
        Generate WORM-compliant commit message with reasoning context.
        
        Args:
            commit_data: Interaction data
            memory_context: Memory snapshot data
            
        Returns:
            str: Formatted commit message
        """
        interaction_summary = commit_data.get('interaction_summary', {})
        files_changed = commit_data.get('files_to_commit', [])
        reasoning = commit_data.get('reasoning', 'no reasoning provided')
        
        # Create concise file summary
        files_summary = self._summarize_file_changes(files_changed)
        
        # Format memory context for commit
        memory_text = self.memory_snapshot.format_for_commit_message(memory_context)
        
        # Detect current plan context for traceability
        plan_path = self._detect_plan_context(files_changed, reasoning)
        plan_footer = f"\nPlan: {plan_path}" if plan_path else ""
        
        # Generate commit message
        commit_message = f"""feat: {files_summary}

User Request: {reasoning}

Files Changed:
{self._format_files_list(files_changed)}

{memory_text}

🤖 Generated with FlowLoom WORM Development Environment
Auto-tracked governance record with complete reasoning preservation{plan_footer}

Co-Authored-By: FlowLoom <flowloom@rodk.dev>"""
        
        return commit_message
    
    def _summarize_file_changes(self, files: List[str]) -> str:
        """Create concise summary of file changes for commit subject."""
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
        file_types = set()
        for file_path in files:
            if file_path.endswith('.py'):
                file_types.add('Python code')
            elif file_path.endswith('.md'):
                file_types.add('documentation')
            elif file_path.endswith(('.json', '.yaml', '.yml')):
                file_types.add('configuration')
            else:
                file_types.add('project files')
        
        if len(file_types) == 1:
            return f"update {list(file_types)[0]} ({len(files)} files)"
        else:
            return f"update {len(files)} files across {len(file_types)} categories"
    
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
    
    def _execute_git_commit(self, commit_message: str) -> Dict[str, Any]:
        """
        Execute git commit with FlowLoom identity using -F option.
        
        Args:
            commit_message: Formatted commit message
            
        Returns:
            dict: Result of commit operation
        """
        if self.dry_run:
            return {
                'success': True,
                'operation': 'git_commit',
                'message': 'DRY RUN: Would execute git commit',
                'commit_message_preview': commit_message[:200] + '...'
            }
        
        try:
            # Create temporary commit message file
            commit_file_path = self.project_root / 'tmp' / 'commit-msg.txt'
            commit_file_path.parent.mkdir(exist_ok=True)
            
            with open(commit_file_path, 'w', encoding='utf-8') as f:
                f.write(commit_message)
            
            # Use -F option to read commit message from file
            result = subprocess.run([
                'git', 'commit',
                '-F', str(commit_file_path),
                '--author', 'FlowLoom <flowloom@rodk.dev>'
            ], 
            cwd=self.project_root,
            capture_output=True,
            text=True,
            check=True)
            
            # Extract commit hash from output
            commit_hash = self._extract_commit_hash(result.stdout)
            
            return {
                'success': True,
                'operation': 'git_commit',
                'message': 'Commit successful',
                'commit_hash': commit_hash,
                'git_output': result.stdout
            }
            
        except subprocess.CalledProcessError as e:
            return {
                'success': False,
                'operation': 'git_commit',
                'message': f'Git commit failed: {e.stderr}',
                'error': str(e),
                'git_output': e.stdout
            }
    
    def _execute_git_push(self) -> Dict[str, Any]:
        """
        Execute git push for governance preservation.
        
        Returns:
            dict: Result of push operation
        """
        if self.dry_run:
            return {
                'success': True,
                'operation': 'git_push',
                'message': 'DRY RUN: Would execute git push'
            }
        
        try:
            result = subprocess.run([
                'git', 'push'
            ],
            cwd=self.project_root,
            capture_output=True,
            text=True,
            check=True)
            
            return {
                'success': True,
                'operation': 'git_push',
                'message': 'Push successful - governance record preserved',
                'git_output': result.stdout
            }
            
        except subprocess.CalledProcessError as e:
            return {
                'success': False,
                'operation': 'git_push',
                'message': f'Git push failed: {e.stderr}',
                'error': str(e),
                'git_output': e.stdout,
                'recovery_hint': 'Commit created but not pushed - governance record exists locally'
            }
    
    def _extract_commit_hash(self, git_output: str) -> Optional[str]:
        """Extract commit hash from git commit output."""
        try:
            # Git commit output typically starts with commit hash
            lines = git_output.strip().split('\n')
            for line in lines:
                if line.startswith('[') and ']' in line:
                    # Extract hash from "[branch hash] commit message"
                    hash_part = line.split(']')[0].split(' ')[-1]
                    if len(hash_part) >= 7:  # Minimum git short hash length
                        return hash_part
            return None
        except Exception:
            return None
    
    def _create_success_result(self, commit_data: Dict[str, Any], 
                             commit_result: Dict[str, Any],
                             push_result: Dict[str, Any]) -> Dict[str, Any]:
        """Create success result summary."""
        interaction_summary = commit_data.get('interaction_summary', {})
        
        return {
            'success': True,
            'operation': 'worm_auto_commit',
            'message': 'WORM auto-commit completed successfully',
            'interaction_id': interaction_summary.get('interaction_id'),
            'files_committed': commit_data.get('files_to_commit', []),
            'commit_hash': commit_result.get('commit_hash'),
            'governance_status': 'preserved',
            'timestamp': datetime.now().isoformat()
        }
    
    def _create_error_result(self, commit_data: Dict[str, Any], error: str) -> Dict[str, Any]:
        """Create error result summary."""
        interaction_summary = commit_data.get('interaction_summary', {})
        
        return {
            'success': False,
            'operation': 'worm_auto_commit',
            'message': f'WORM auto-commit failed: {error}',
            'interaction_id': interaction_summary.get('interaction_id'),
            'files_affected': commit_data.get('files_to_commit', []),
            'error': error,
            'governance_status': 'interrupted',
            'timestamp': datetime.now().isoformat()
        }
    
    def _log_auto_commit_success(self, result: Dict[str, Any]):
        """Log successful auto-commit for debugging."""
        print(f"✅ WORM AUTO-COMMIT SUCCESS: {result.get('commit_hash', 'unknown hash')}")
        print(f"   Files: {len(result.get('files_committed', []))} committed")
        print(f"   Interaction: {result.get('interaction_id', 'unknown')}")
    
    def _log_auto_commit_failure(self, result: Dict[str, Any]):
        """Log auto-commit failure for debugging."""
        print(f"❌ WORM AUTO-COMMIT FAILED: {result.get('error', 'unknown error')}")
        print(f"   Files affected: {result.get('files_affected', [])}")
        print(f"   Interaction: {result.get('interaction_id', 'unknown')}")
    
    def get_repository_status(self) -> Dict[str, Any]:
        """
        Get current git repository status for WORM system.
        
        Returns:
            dict: Repository status information
        """
        try:
            # Check if we're in a git repository
            result = subprocess.run([
                'git', 'status', '--porcelain'
            ],
            cwd=self.project_root,
            capture_output=True,
            text=True,
            check=True)
            
            # Check for uncommitted changes
            uncommitted_files = []
            if result.stdout.strip():
                for line in result.stdout.strip().split('\n'):
                    if line.strip():
                        status = line[:2]
                        filename = line[3:]
                        uncommitted_files.append({'status': status, 'file': filename})
            
            # Get current branch
            branch_result = subprocess.run([
                'git', 'branch', '--show-current'
            ],
            cwd=self.project_root,
            capture_output=True,
            text=True,
            check=True)
            
            current_branch = branch_result.stdout.strip()
            
            return {
                'success': True,
                'is_git_repo': True,
                'current_branch': current_branch,
                'uncommitted_files': uncommitted_files,
                'has_uncommitted_changes': len(uncommitted_files) > 0,
                'worm_ready': len(uncommitted_files) == 0
            }
            
        except subprocess.CalledProcessError as e:
            return {
                'success': False,
                'is_git_repo': False,
                'error': str(e),
                'worm_ready': False
            }


class WORMCommitValidator:
    """
    Validates git repository state before attempting WORM auto-commit.
    """
    
    def __init__(self, project_root: str = "."):
        self.project_root = Path(project_root)
    
    def validate_commit_readiness(self, files_to_commit: List[str]) -> Dict[str, Any]:
        """
        Validate that repository is ready for WORM auto-commit.
        
        Args:
            files_to_commit: List of files that would be committed
            
        Returns:
            dict: Validation result with readiness status
        """
        engine = WORMCommitEngine(self.project_root)
        repo_status = engine.get_repository_status()
        
        if not repo_status['success']:
            return {
                'ready': False,
                'issues': ['Not a git repository or git not available'],
                'repo_status': repo_status
            }
        
        issues = []
        
        # Check for existing uncommitted changes that aren't our target files
        if repo_status['has_uncommitted_changes']:
            uncommitted = [f['file'] for f in repo_status['uncommitted_files']]
            unexpected_changes = [f for f in uncommitted if f not in files_to_commit]
            
            if unexpected_changes:
                issues.append(f'Unexpected uncommitted changes: {unexpected_changes}')
        
        # Check if target files exist
        missing_files = []
        for file_path in files_to_commit:
            full_path = self.project_root / file_path
            if not full_path.exists():
                missing_files.append(file_path)
        
        if missing_files:
            issues.append(f'Target files do not exist: {missing_files}')
        
        return {
            'ready': len(issues) == 0,
            'issues': issues,
            'repo_status': repo_status
        }