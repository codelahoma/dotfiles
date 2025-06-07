#!/usr/bin/env python3
"""
FlowLoom Production Coordinator

Unified Multi-Claude coordination system with optimized performance,
robust error handling, and seamless integration.

This script replaces individual coordination demos with a production-ready
implementation that automatically selects the best coordination strategy.

Usage:
    python3 bin/flowloom_coordinator.py controller init
    python3 bin/flowloom_coordinator.py worker register
    python3 bin/flowloom_coordinator.py status
"""

import os
import sys
import argparse
import asyncio
import datetime
from pathlib import Path
from typing import Optional, List, Dict, Any

# Add packages to Python path
PROJECT_ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(PROJECT_ROOT / "packages"))

try:
    from flowloom_session import EnhancedSessionManager, quick_session_status
    from flowloom_memory_monitor import (
        BaseCoordinator, CoordinatorMixin,
        create_standardized_worker_entity,
        create_standardized_task_entity, 
        create_standardized_session_entity,
        search_entities, wait_for_worker_task, wait_for_task_completion
    )
except ImportError as e:
    print(f"❌ Import Error: {e}")
    print("   Make sure FlowLoom packages are properly installed")
    print("   Run: uv install or pip install -e packages/flowloom_*")
    sys.exit(1)


class FlowLoomCoordinator(BaseCoordinator, CoordinatorMixin):
    """
    Production-ready Multi-Claude coordinator with automatic optimization.
    
    Features:
    - Automatic branch-aware or basic coordination mode selection
    - Robust error handling and graceful degradation
    - Performance optimizations and caching
    - Seamless integration with FlowLoom session management
    - Production monitoring and diagnostics
    """
    
    def __init__(self):
        super().__init__()
        self.session_manager = EnhancedSessionManager()
        self.coordination_mode = self._detect_coordination_mode()
        self._worker_cache = {}
        self._task_cache = {}
        
    def get_coordinator_type(self) -> str:
        """Return the type of coordinator."""
        return self.coordination_mode
    
    def get_worker_entity_type(self) -> str:
        """Return the entity type used for workers."""
        return "BranchAwareWorker" if self.coordination_mode == "branch-aware" else "Worker"
    
    def get_task_entity_type(self) -> str:
        """Return the entity type used for tasks."""
        return "BranchAwareTask" if self.coordination_mode == "branch-aware" else "WorkItem"
    
    def get_session_entity_type(self) -> str:
        """Return the entity type used for sessions."""
        return "BranchAwareSession" if self.coordination_mode == "branch-aware" else "CoordinationSession"
    
    def _detect_coordination_mode(self) -> str:
        """Automatically detect the best coordination mode for current environment."""
        try:
            current_branch = self._get_current_branch()
            
            # Use branch-aware mode if:
            # - We're on a non-main branch
            # - We're in a git repository with multiple branches
            # - Environment suggests cross-branch work
            if current_branch not in ["main", "master"]:
                return "branch-aware"
            
            # Check if we have multiple branches
            try:
                import subprocess
                result = subprocess.run(
                    ["git", "branch", "-r"],
                    capture_output=True,
                    text=True,
                    timeout=3,
                    cwd=PROJECT_ROOT
                )
                if result.returncode == 0 and len(result.stdout.strip().split('\n')) > 2:
                    return "branch-aware"
            except:
                pass
            
            return "basic"
            
        except Exception as e:
            print(f"Warning: Mode detection failed: {e}, using basic mode")
            return "basic"
    
    def _determine_session_type(self, branch: Optional[str] = None) -> str:
        """Determine session type based on branch."""
        if not branch:
            branch = self._get_current_branch()
            
        if branch == "gh-pages":
            return "web"
        elif branch in ["main", "master", "develop"]:
            return "dev"
        elif branch.startswith("feature"):
            return "feature"
        else:
            return "general"
    
    # Controller Methods
    
    def controller_init(self, session_name: Optional[str] = None, 
                       objective: str = "", force_mode: Optional[str] = None) -> str:
        """Initialize coordination session with automatic optimization."""
        
        # Override mode if specified
        if force_mode and force_mode in ["basic", "branch-aware"]:
            self.coordination_mode = force_mode
            
        # Generate session name if not provided
        if not session_name:
            current_branch = self._get_current_branch()
            session_type = self._determine_session_type(current_branch)
            date_str = datetime.datetime.now().strftime("%Y%m%d-%H%M")
            session_name = f"FlowLoom-{session_type}-{date_str}"
        
        controller_id = f"FlowLoomController-{self.shell_pid}"
        
        print("🎯 FlowLoom Multi-Claude Coordination")
        print("=" * 38)
        print(f"Session Name: {session_name}")
        print(f"Controller ID: {controller_id}")
        print(f"Coordination Mode: {self.coordination_mode}")
        print(f"Objective: {objective}")
        print()
        
        try:
            # Create session entity
            session_entity = create_standardized_session_entity(self, session_name, objective)
            
            # Add mode-specific information
            if self.coordination_mode == "branch-aware":
                current_branch = self._get_current_branch()
                session_type = self._determine_session_type(current_branch)
                session_entity["observations"].extend([
                    f"controller_branch: {current_branch}",
                    f"session_type: {session_type}",
                    "cross_branch_coordination: enabled"
                ])
            
            # Update memory
            self._safe_memory_update(session_name, [
                f"Coordination session initialized in {self.coordination_mode} mode",
                f"Controller: {controller_id}",
                f"Objective: {objective}"
            ])
            
            print("✅ Coordination session initialized successfully")
            print()
            print("📋 Session Details:")
            print(f"   • Mode: {self.coordination_mode}")
            print(f"   • Session: {session_name}")
            print(f"   • Controller: {controller_id}")
            print(f"   • Status: Active")
            
            if self.coordination_mode == "branch-aware":
                current_branch = self._get_current_branch()
                session_type = self._determine_session_type(current_branch)
                print(f"   • Branch: {current_branch} ({session_type})")
                print(f"   • Cross-branch: Enabled")
            
            print()
            print("💡 Next Steps:")
            print("   python3 bin/flowloom_coordinator.py status")
            print("   python3 bin/flowloom_coordinator.py controller workers")
            print("   python3 bin/flowloom_coordinator.py controller dispatch 'task description'")
            print()
            print("🔗 Worker Setup (in separate terminals):")
            print("   python3 bin/flowloom_coordinator.py worker register")
            print("   python3 bin/flowloom_coordinator.py worker wait")
            print()
            print(f"🆔 Session ID: {session_name}")
            
            return session_name
            
        except Exception as e:
            self._handle_coordination_error("session initialization", e)
            return ""
    
    def controller_workers(self) -> None:
        """List available workers with enhanced display."""
        print("👥 Worker Status Report")
        print("=" * 23)
        print()
        
        try:
            workers = self.find_workers_by_status()  # All workers
            
            if not workers:
                print("🚫 No workers found")
                print()
                print("💡 Register workers:")
                print("   python3 bin/flowloom_coordinator.py worker register")
                if self.coordination_mode == "branch-aware":
                    print("   # With specific branch:")
                    print("   python3 bin/flowloom_coordinator.py worker register --branch gh-pages --capabilities web,content")
                return
            
            # Group and display workers
            if self.coordination_mode == "branch-aware":
                self._display_branch_aware_workers(workers)
            else:
                self._display_basic_workers(workers)
                
        except Exception as e:
            self._handle_coordination_error("worker listing", e)
    
    def _display_basic_workers(self, workers: List[Dict[str, Any]]) -> None:
        """Display workers in basic mode."""
        available_count = 0
        busy_count = 0
        
        for worker in workers:
            parsed_obs = self._parse_observations(worker.get('observations', []))
            status = parsed_obs.get('status', 'unknown')
            capabilities = parsed_obs.get('capabilities', 'unknown')
            shell_pid = parsed_obs.get('shell_pid', 'unknown')
            
            status_icon = "✅" if status == "available" else "🔄" if status == "busy" else "❌"
            
            print(f"{status_icon} {worker.get('name', 'Unknown')}")
            print(f"   Status: {status}")
            print(f"   Shell PID: {shell_pid}")
            print(f"   Capabilities: {capabilities}")
            print()
            
            if status == "available":
                available_count += 1
            elif status == "busy":
                busy_count += 1
        
        print("📊 Summary:")
        print(f"   Available: {available_count}")
        print(f"   Busy: {busy_count}")
        print(f"   Total: {len(workers)}")
    
    def _display_branch_aware_workers(self, workers: List[Dict[str, Any]]) -> None:
        """Display workers in branch-aware mode."""
        branch_groups = {}
        
        for worker in workers:
            parsed_obs = self._parse_observations(worker.get('observations', []))
            current_branch = parsed_obs.get('current_branch', 'unknown')
            
            if current_branch not in branch_groups:
                branch_groups[current_branch] = []
            branch_groups[current_branch].append((worker, parsed_obs))
        
        total_available = 0
        total_busy = 0
        
        for branch, branch_workers in branch_groups.items():
            session_type = self._determine_session_type(branch)
            print(f"🌲 {branch} branch ({session_type} workers):")
            
            for worker, parsed_obs in branch_workers:
                status = parsed_obs.get('status', 'unknown')
                status_icon = "✅" if status == "available" else "🔄" if status == "busy" else "❌"
                
                print(f"   {status_icon} {worker.get('name', 'Unknown')}")
                print(f"      Status: {status}")
                
                if status == "available":
                    total_available += 1
                elif status == "busy":
                    total_busy += 1
            print()
        
        print("📊 Summary:")
        print(f"   Available: {total_available}")
        print(f"   Busy: {total_busy}")
        print(f"   Branches: {len(branch_groups)}")
    
    def controller_dispatch(self, description: str, task_type: str = "general", 
                           priority: str = "medium", requires_branch: Optional[str] = None,
                           requires_skills: Optional[str] = None) -> Optional[str]:
        """Create and dispatch task with intelligent worker matching."""
        print("📤 Task Dispatch")
        print("=" * 16)
        print()
        
        try:
            # Find available workers
            available_workers = self.find_workers_by_status("available")
            
            if not available_workers:
                print("❌ No available workers found")
                print("   Register workers first:")
                print("   python3 bin/flowloom_coordinator.py worker register")
                return None
            
            # Create task
            task_entity = create_standardized_task_entity(self, task_type, description, priority)
            task_id = task_entity["name"]
            
            # Add mode-specific requirements
            if self.coordination_mode == "branch-aware" and requires_branch:
                task_entity["observations"].append(f"required_branch: {requires_branch}")
            
            if requires_skills:
                task_entity["observations"].append(f"required_skills: {requires_skills}")
            
            # Assign to best worker
            best_worker = self._find_best_worker(available_workers, requires_branch, requires_skills)
            
            if not best_worker:
                print("❌ No suitable worker found for task requirements")
                return None
            
            worker_id = best_worker.get("name", "Unknown")
            
            # Update task assignment
            task_entity["observations"] = [
                obs.replace("assigned_to: unassigned", f"assigned_to: {worker_id}")
                for obs in task_entity["observations"]
            ]
            
            print(f"Task ID: {task_id}")
            print(f"Description: {description}")
            print(f"Type: {task_type}")
            print(f"Priority: {priority}")
            print(f"Assigned to: {worker_id}")
            
            if requires_branch:
                print(f"Required Branch: {requires_branch}")
            if requires_skills:
                print(f"Required Skills: {requires_skills}")
            
            # Update memory
            self._safe_memory_update(task_id, [
                f"Task created and assigned to {worker_id}",
                f"Type: {task_type}, Priority: {priority}",
                f"Requirements: branch={requires_branch}, skills={requires_skills}"
            ])
            
            self._safe_memory_update(worker_id, [
                f"Task assigned: {task_id}",
                "status: busy",
                f"current_task: {task_id}"
            ])
            
            print()
            print("✅ Task dispatched successfully")
            print()
            print("💡 Monitor progress:")
            print("   python3 bin/flowloom_coordinator.py status")
            print(f"   python3 bin/flowloom_coordinator.py controller wait-for {task_id}")
            
            return task_id
            
        except Exception as e:
            self._handle_coordination_error("task dispatch", e)
            return None
    
    def _find_best_worker(self, workers: List[Dict[str, Any]], 
                         requires_branch: Optional[str] = None,
                         requires_skills: Optional[str] = None) -> Optional[Dict[str, Any]]:
        """Find the best worker for task requirements."""
        scored_workers = []
        
        for worker in workers:
            score = self._score_worker(worker, requires_branch, requires_skills)
            if score > 0:
                scored_workers.append((worker, score))
        
        if not scored_workers:
            return None
        
        # Sort by score (highest first) and return best worker
        scored_workers.sort(key=lambda x: x[1], reverse=True)
        return scored_workers[0][0]
    
    def _score_worker(self, worker: Dict[str, Any], 
                     requires_branch: Optional[str] = None,
                     requires_skills: Optional[str] = None) -> float:
        """Score worker suitability for task."""
        parsed_obs = self._parse_observations(worker.get('observations', []))
        score = 50.0  # Base score
        
        # Branch matching (only in branch-aware mode)
        if self.coordination_mode == "branch-aware" and requires_branch:
            current_branch = parsed_obs.get('current_branch', 'main')
            if current_branch == requires_branch:
                score += 30
            else:
                # Check cross-branch capabilities
                branch_caps = parsed_obs.get('branch_capabilities', '')
                if requires_branch in branch_caps:
                    score += 15
                else:
                    score -= 20
        
        # Skill matching
        if requires_skills:
            worker_caps = set(parsed_obs.get('capabilities', '').split(','))
            required_caps = set(requires_skills.split(','))
            
            overlap = worker_caps.intersection(required_caps)
            if overlap:
                skill_score = (len(overlap) / len(required_caps)) * 20
                score += skill_score
            else:
                score -= 10
        
        return max(0, score)
    
    def status(self) -> None:
        """Show comprehensive coordination status."""
        summary = self.get_coordination_summary()
        
        status_data = {
            "System Status": {
                "Mode": self.coordination_mode,
                "Shell PID": self.shell_pid,
                "Session Manager": f"{summary['session_stats']['active_sessions']} active sessions"
            },
            "Workers": summary.get('worker_stats', {}),
            "Tasks": summary.get('task_stats', {}),
            "Sessions": summary.get('session_stats', {})
        }
        
        if self.coordination_mode == "branch-aware":
            current_branch = self._get_current_branch()
            session_type = self._determine_session_type(current_branch)
            status_data["Branch Context"] = {
                "Current Branch": f"{current_branch} ({session_type})",
                "Cross-branch": "Enabled"
            }
        
        self._format_status_display("FlowLoom Coordination Status", status_data)
        
        # Show recent activity
        try:
            session_status = quick_session_status()
            print("💡 Tips:")
            if summary['worker_stats']['available'] > 0:
                print("   • Workers available for task dispatch")
                print("   • Use: python3 bin/flowloom_coordinator.py controller dispatch 'task description'")
            else:
                print("   • No available workers - register workers to enable task dispatch")
                print("   • Use: python3 bin/flowloom_coordinator.py worker register")
            
            if summary['task_stats']['pending'] > 0:
                print(f"   • {summary['task_stats']['pending']} tasks waiting for assignment")
            
            print()
        except Exception as e:
            print(f"Warning: Could not get session status: {e}")
    
    async def controller_wait_for(self, task_id: str, timeout: int = 300) -> None:
        """Wait for task completion with enhanced monitoring."""
        print(f"⏳ Waiting for task completion: {task_id}")
        print(f"   Timeout: {timeout} seconds")
        print("   (Press Ctrl+C to stop)")
        print()
        
        try:
            result = await wait_for_task_completion(task_id, timeout)
            
            if result:
                print("🎉 Task completed successfully!")
                print(f"Task ID: {result.get('name')}")
                
                # Show completion details
                parsed_obs = self._parse_observations(result.get('observations', []))
                print(f"Status: {parsed_obs.get('status', 'unknown')}")
                if 'completed_at' in parsed_obs:
                    print(f"Completed: {parsed_obs['completed_at']}")
            else:
                print("⏰ Task did not complete within timeout period")
                print()
                print("💡 Check task status:")
                print("   python3 bin/flowloom_coordinator.py status")
                
        except KeyboardInterrupt:
            print("\n🛑 Waiting cancelled by user")
        except Exception as e:
            self._handle_coordination_error("task waiting", e)
    
    # Worker Methods
    
    def worker_register(self, capabilities: str = "general,research,coding", 
                       branch: Optional[str] = None) -> Optional[str]:
        """Register as worker with automatic capability detection."""
        
        if self.coordination_mode == "branch-aware" and not branch:
            branch = self._get_current_branch()
        
        worker_id = f"{self.get_coordinator_type().title()}Worker-{self.shell_pid}"
        
        print("🤖 Worker Registration")
        print("=" * 22)
        print(f"Worker ID: {worker_id}")
        print(f"Capabilities: {capabilities}")
        print(f"Mode: {self.coordination_mode}")
        
        if self.coordination_mode == "branch-aware":
            session_type = self._determine_session_type(branch)
            print(f"Branch: {branch} ({session_type})")
        
        print()
        
        try:
            # Create worker entity
            worker_entity = create_standardized_worker_entity(self, capabilities)
            
            # Add mode-specific information
            if self.coordination_mode == "branch-aware":
                session_type = self._determine_session_type(branch)
                worker_entity["observations"].extend([
                    f"current_branch: {branch}",
                    f"session_type: {session_type}",
                    "cross_branch_capable: true"
                ])
            
            # Update memory
            self._safe_memory_update(worker_id, [
                f"Worker registered in {self.coordination_mode} mode",
                f"Capabilities: {capabilities}",
                f"Branch context: {branch}" if branch else "No branch context"
            ])
            
            print("✅ Worker registered successfully")
            print()
            
            # Get session context
            session_status = quick_session_status()
            session_count = len(session_status.get('sessions', {}))
            
            print("🔧 Integration:")
            print(f"   Session Manager: {session_count} active sessions")
            print(f"   Memory Coordination: Enabled")
            print(f"   Mode: {self.coordination_mode}")
            print()
            
            print("💡 Next Steps:")
            print("   python3 bin/flowloom_coordinator.py worker wait")
            print("   python3 bin/flowloom_coordinator.py worker status")
            print()
            print("🔄 Worker is now available for task assignments")
            
            return worker_id
            
        except Exception as e:
            self._handle_coordination_error("worker registration", e)
            return None
    
    async def worker_wait(self, timeout: int = 300) -> None:
        """Wait for task assignments with enhanced monitoring."""
        worker_id = f"{self.get_coordinator_type().title()}Worker-{self.shell_pid}"
        
        print("⏳ Worker Task Monitor")
        print("=" * 21)
        print(f"Worker ID: {worker_id}")
        print(f"Mode: {self.coordination_mode}")
        print(f"Timeout: {timeout} seconds")
        
        if self.coordination_mode == "branch-aware":
            current_branch = self._get_current_branch()
            print(f"Current Branch: {current_branch}")
        
        print("🔍 Waiting for task assignments...")
        print("   (Press Ctrl+C to stop)")
        print()
        
        try:
            task = await wait_for_worker_task(worker_id, timeout)
            
            if task:
                print("🎯 Task Assignment Received!")
                print("=" * 30)
                
                parsed_obs = self._parse_observations(task.get('observations', []))
                task_id = task.get('name', 'Unknown')
                description = parsed_obs.get('description', 'No description')
                task_type = parsed_obs.get('type', 'Unknown')
                priority = parsed_obs.get('priority', 'Unknown')
                
                print(f"Task ID: {task_id}")
                print(f"Type: {task_type}")
                print(f"Priority: {priority}")
                print(f"Description: {description}")
                
                if self.coordination_mode == "branch-aware":
                    required_branch = parsed_obs.get('required_branch')
                    if required_branch:
                        current_branch = self._get_current_branch()
                        print(f"Required Branch: {required_branch}")
                        if current_branch != required_branch:
                            print(f"⚠️  Branch Switch Needed: {current_branch} → {required_branch}")
                        else:
                            print("✅ Already on correct branch")
                
                print()
                print("💡 Next Steps:")
                print("   1. Complete your work")
                print("   2. Report completion (manual process)")
                print("   3. Check status: python3 bin/flowloom_coordinator.py worker status")
                print()
                
                return task
            else:
                print("⏰ No tasks received within timeout period")
                print()
                print("💡 Options:")
                print("   python3 bin/flowloom_coordinator.py worker wait")
                print("   python3 bin/flowloom_coordinator.py status")
                
        except KeyboardInterrupt:
            print("\n🛑 Task waiting cancelled by user")
            print("   Worker remains registered and available")
        except Exception as e:
            self._handle_coordination_error("worker task waiting", e)
    
    def worker_status(self) -> None:
        """Show worker status with enhanced information."""
        worker_id = f"{self.get_coordinator_type().title()}Worker-{self.shell_pid}"
        
        print("🔍 Worker Status Check")
        print("=" * 22)
        print(f"Worker ID: {worker_id}")
        print(f"Mode: {self.coordination_mode}")
        
        if self.coordination_mode == "branch-aware":
            current_branch = self._get_current_branch()
            session_type = self._determine_session_type(current_branch)
            print(f"Current Branch: {current_branch} ({session_type})")
        
        print()
        
        try:
            workers = self._safe_search_entities(
                self.get_worker_entity_type(), 
                f"shell_pid: {self.shell_pid}"
            )
            
            if not workers:
                print("❌ Worker not registered")
                print("   Run: python3 bin/flowloom_coordinator.py worker register")
                return
            
            worker = workers[0]
            parsed_obs = self._parse_observations(worker.get('observations', []))
            
            status = parsed_obs.get('status', 'unknown')
            capabilities = parsed_obs.get('capabilities', 'unknown')
            current_task = parsed_obs.get('current_task', 'none')
            tasks_completed = parsed_obs.get('tasks_completed', '0')
            
            print(f"✅ Status: {status}")
            print(f"   Capabilities: {capabilities}")
            if current_task != 'none':
                print(f"   Current Task: {current_task}")
            print(f"   Tasks Completed: {tasks_completed}")
            
            if self.coordination_mode == "branch-aware":
                registered_branch = parsed_obs.get('current_branch', 'unknown')
                current_branch = self._get_current_branch()
                print(f"   Registered Branch: {registered_branch}")
                if registered_branch != current_branch:
                    print(f"   ⚠️  Current Branch: {current_branch} (different from registered)")
            
            print()
            
            # Check for assigned tasks
            my_tasks = self._safe_search_entities(
                self.get_task_entity_type(),
                f"assigned_to: {worker_id}"
            )
            
            if my_tasks:
                print(f"📋 Assigned Tasks: {len(my_tasks)}")
                for task in my_tasks[:3]:
                    task_obs = self._parse_observations(task.get('observations', []))
                    task_status = task_obs.get('status', 'unknown')
                    print(f"   • {task.get('name')}: {task_status}")
            else:
                print("📋 No assigned tasks")
                
        except Exception as e:
            self._handle_coordination_error("worker status check", e)


def main():
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        description="FlowLoom Production Multi-Claude Coordinator",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Initialize coordination session
  python3 bin/flowloom_coordinator.py controller init
  
  # Register worker
  python3 bin/flowloom_coordinator.py worker register --capabilities research,coding
  
  # Dispatch task
  python3 bin/flowloom_coordinator.py controller dispatch "Analyze code performance"
  
  # Check status
  python3 bin/flowloom_coordinator.py status
        """
    )
    
    subparsers = parser.add_subparsers(dest='role', help='Role to run')
    
    # Controller commands
    controller_parser = subparsers.add_parser('controller', help='Controller commands')
    controller_sub = controller_parser.add_subparsers(dest='controller_cmd')
    
    # Controller init
    init_parser = controller_sub.add_parser('init', help='Initialize coordination session')
    init_parser.add_argument('--session', help='Session name')
    init_parser.add_argument('--objective', default='', help='Session objective')
    init_parser.add_argument('--mode', choices=['basic', 'branch-aware'], help='Force coordination mode')
    
    # Controller workers
    controller_sub.add_parser('workers', help='List available workers')
    
    # Controller dispatch
    dispatch_parser = controller_sub.add_parser('dispatch', help='Create and dispatch task')
    dispatch_parser.add_argument('description', help='Task description')
    dispatch_parser.add_argument('--type', default='general', help='Task type')
    dispatch_parser.add_argument('--priority', default='medium', choices=['low', 'medium', 'high', 'critical'], help='Task priority')
    dispatch_parser.add_argument('--requires-branch', help='Required branch for task')
    dispatch_parser.add_argument('--requires-skills', help='Required skills (comma-separated)')
    
    # Controller wait-for
    wait_parser = controller_sub.add_parser('wait-for', help='Wait for task completion')
    wait_parser.add_argument('task_id', help='Task ID to wait for')
    wait_parser.add_argument('--timeout', type=int, default=300, help='Timeout in seconds')
    
    # Worker commands
    worker_parser = subparsers.add_parser('worker', help='Worker commands')
    worker_sub = worker_parser.add_subparsers(dest='worker_cmd')
    
    # Worker register
    register_parser = worker_sub.add_parser('register', help='Register as worker')
    register_parser.add_argument('--capabilities', default='general,research,coding', help='Worker capabilities')
    register_parser.add_argument('--branch', help='Primary branch (auto-detected if not specified)')
    
    # Worker wait
    wait_worker_parser = worker_sub.add_parser('wait', help='Wait for task assignments')
    wait_worker_parser.add_argument('--timeout', type=int, default=300, help='Timeout in seconds')
    
    # Worker status
    worker_sub.add_parser('status', help='Show worker status')
    
    # Global status command
    parser.add_argument('--status', action='store_true', help='Show coordination status')
    
    args = parser.parse_args()
    
    # Handle global status
    if args.status or (hasattr(args, 'role') and args.role == 'status'):
        coordinator = FlowLoomCoordinator()
        coordinator.status()
        return
    
    # Handle case where no command provided
    if not hasattr(args, 'role') or not args.role:
        parser.print_help()
        return
    
    coordinator = FlowLoomCoordinator()
    
    try:
        if args.role == 'controller':
            if args.controller_cmd == 'init':
                coordinator.controller_init(args.session, args.objective, args.mode)
            elif args.controller_cmd == 'workers':
                coordinator.controller_workers()
            elif args.controller_cmd == 'dispatch':
                coordinator.controller_dispatch(
                    args.description,
                    args.type,
                    args.priority,
                    args.requires_branch,
                    args.requires_skills
                )
            elif args.controller_cmd == 'wait-for':
                asyncio.run(coordinator.controller_wait_for(args.task_id, args.timeout))
            else:
                controller_parser.print_help()
        
        elif args.role == 'worker':
            if args.worker_cmd == 'register':
                coordinator.worker_register(args.capabilities, args.branch)
            elif args.worker_cmd == 'wait':
                asyncio.run(coordinator.worker_wait(args.timeout))
            elif args.worker_cmd == 'status':
                coordinator.worker_status()
            else:
                worker_parser.print_help()
        
        elif args.role == 'status':
            coordinator.status()
        
        else:
            parser.print_help()
            
    except KeyboardInterrupt:
        print("\n🛑 Operation cancelled by user")
    except Exception as e:
        print(f"❌ Unexpected error: {e}")
        print("   Please check the system status and try again")
        sys.exit(1)


if __name__ == "__main__":
    main()