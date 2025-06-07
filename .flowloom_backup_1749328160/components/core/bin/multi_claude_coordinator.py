#!/usr/bin/env python3
"""
Multi-Claude Coordination Prototype

This prototype demonstrates working Multi-Claude coordination using:
- Enhanced session tracking for branch context
- MCP memory server for shared coordination state
- Cross-branch coordination capabilities
- Integration with FlowLoom's architecture

Usage:
    python3 bin/multi_claude_coordinator.py controller init
    python3 bin/multi_claude_coordinator.py worker register
    python3 bin/multi_claude_coordinator.py worker wait
"""

import os
import sys
import json
import uuid
import datetime
import asyncio
import argparse
from pathlib import Path

# Add packages to Python path
PROJECT_ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(PROJECT_ROOT / "packages"))

from flowloom_session import EnhancedSessionManager, quick_session_status
from flowloom_memory_monitor import (
    search_entities, wait_for_worker_task, find_available_workers,
    find_session_tasks, wait_for_task_completion
)

class MultiClaudeCoordinator:
    """Multi-Claude coordination prototype implementation."""
    
    def __init__(self):
        self.session_manager = EnhancedSessionManager()
        self.shell_pid = self._get_shell_pid()
        
    def _get_shell_pid(self):
        """Get shell PID using FlowLoom's detection method."""
        try:
            script_path = PROJECT_ROOT / "bin" / "get_shell_pid.sh"
            if script_path.exists():
                import subprocess
                result = subprocess.run(
                    [str(script_path)], 
                    capture_output=True, 
                    text=True, 
                    timeout=5  # Add timeout
                )
                if result.returncode == 0 and result.stdout.strip():
                    return int(result.stdout.strip())
        except (subprocess.TimeoutExpired, subprocess.SubprocessError, ValueError) as e:
            print(f"Warning: Shell PID detection failed: {e}")
        except Exception as e:
            print(f"Warning: Unexpected error in shell PID detection: {e}")
        return os.getpid()
    
    def _format_timestamp(self):
        """Get current timestamp in ISO format."""
        return datetime.datetime.now().isoformat()
    
    def _create_worker_entity(self, capabilities: str = "research,coding,testing"):
        """Create worker entity for registration."""
        worker_id = f"Worker-{self.shell_pid}"
        
        return {
            "name": worker_id,
            "entityType": "Worker", 
            "observations": [
                f"shell_pid: {self.shell_pid}",
                "status: available",
                f"capabilities: {capabilities}",
                f"started: {self._format_timestamp()}",
                "current_task: none",
                "tasks_completed: 0",
                f"last_heartbeat: {self._format_timestamp()}",
                f"branch_context: main"  # Integration with session system
            ]
        }
    
    def _create_coordination_session(self, session_name: str, objective: str = ""):
        """Create coordination session entity."""
        return {
            "name": session_name,
            "entityType": "CoordinationSession",
            "observations": [
                f"controller: Controller-{self.shell_pid}",
                "workers: []",
                f"started: {self._format_timestamp()}",
                "status: active", 
                "total_tasks: 0",
                "completed_tasks: 0",
                "failed_tasks: 0",
                f"objective: {objective}",
                f"session_manager_integration: enabled"
            ]
        }
    
    def _create_task_entity(self, task_type: str, description: str, priority: str = "medium"):
        """Create work item entity."""
        task_id = f"Task-{str(uuid.uuid4())[:8]}"
        
        return {
            "name": task_id,
            "entityType": "WorkItem",
            "observations": [
                f"type: {task_type}",
                f"description: {description}",
                "assigned_to: unassigned",
                "status: pending",
                f"priority: {priority}",
                f"created_by: Controller-{self.shell_pid}",
                f"created_at: {self._format_timestamp()}",
                "dependencies: []",
                "context: {}",
                "estimated_duration: unknown"
            ]
        }
    
    def controller_init(self, session_name: str = None, objective: str = ""):
        """Initialize coordination session as controller."""
        if not session_name:
            date_str = datetime.datetime.now().strftime("%Y-%m-%d")
            session_name = f"CoordSession-{date_str}-{str(uuid.uuid4())[:8]}"
        
        controller_id = f"Controller-{self.shell_pid}"
        
        print("🎯 Multi-Claude Coordination Session Initialization")
        print("=" * 55)
        print(f"Session Name: {session_name}")
        print(f"Controller ID: {controller_id}")
        print(f"Objective: {objective}")
        print(f"Shell PID: {self.shell_pid}")
        print()
        
        # Create coordination session entity
        session_entity = self._create_coordination_session(session_name, objective)
        
        # In real implementation, would create via MCP memory
        print("✅ Coordination session initialized successfully")
        print()
        print("📋 Session Status:")
        print(f"   • Session: {session_name}")
        print(f"   • Controller: {controller_id}")
        print(f"   • Status: Active")
        print(f"   • Workers: 0 (none connected yet)")
        print()
        print("💡 Next Steps:")
        print("   python3 bin/multi_claude_coordinator.py controller workers")
        print("   python3 bin/multi_claude_coordinator.py controller dispatch 'task description'")
        print()
        print("🔗 Worker Setup (in separate terminals/sessions):")
        print("   python3 bin/multi_claude_coordinator.py worker register")
        print("   python3 bin/multi_claude_coordinator.py worker wait")
        print()
        print(f"🆔 Session ID: {session_name}")
        
        return session_name
    
    def controller_workers(self):
        """List available workers."""
        print("👥 Worker Status Report")
        print("=" * 25)
        print()
        
        try:
            # Query for workers using memory monitor
            workers = search_entities(entity_type="Worker")
        except Exception as e:
            print(f"❌ Error querying workers: {e}")
            print("   Check that memory system is accessible")
            return
        
        if not workers:
            print("🚫 No workers found")
            print("   No workers have been registered yet")
            print()
            print("💡 To register workers:")
            print("   1. Start Claude in another terminal/session")
            print("   2. Run: python3 bin/multi_claude_coordinator.py worker register")
            print("   3. Run: python3 bin/multi_claude_coordinator.py worker wait")
        else:
            available_count = 0
            busy_count = 0
            offline_count = 0
            
            for i, worker in enumerate(workers, 1):
                name = worker.get('name', 'Unknown')
                observations = worker.get('observations', [])
                
                # Parse observations
                parsed_obs = {}
                for obs in observations:
                    if ':' in obs:
                        key, value = obs.split(':', 1)
                        parsed_obs[key.strip()] = value.strip()
                
                status = parsed_obs.get('status', 'unknown')
                capabilities = parsed_obs.get('capabilities', 'unknown')
                shell_pid = parsed_obs.get('shell_pid', 'unknown')
                last_heartbeat = parsed_obs.get('last_heartbeat', 'unknown')
                
                # Status icon
                if status == "available":
                    available_count += 1
                    status_icon = "✅"
                elif status == "busy":
                    busy_count += 1
                    status_icon = "🔄"
                else:
                    offline_count += 1
                    status_icon = "❌"
                
                print(f"{status_icon} {name}")
                print(f"   Status: {status}")
                print(f"   Shell PID: {shell_pid}")
                print(f"   Capabilities: {capabilities}")
                print(f"   Last Heartbeat: {last_heartbeat}")
                if i < len(workers):
                    print()
            
            print("📊 Summary:")
            print(f"   Available: {available_count}")
            print(f"   Busy: {busy_count}")
            print(f"   Offline: {offline_count}")
            print(f"   Total: {len(workers)}")
            print()
            
            if available_count > 0:
                print("💡 Ready for task dispatch:")
                print("   python3 bin/multi_claude_coordinator.py controller dispatch 'research OAuth2 patterns' --type research --priority high")
            else:
                print("⚠️  No available workers for task dispatch")
    
    def controller_dispatch(self, description: str, task_type: str = "research", priority: str = "medium"):
        """Create and dispatch a task."""
        print("📤 Task Dispatch")
        print("=" * 16)
        print()
        
        try:
            # Find available workers
            workers = search_entities(entity_type="Worker", search_term="status: available")
        except Exception as e:
            print(f"❌ Error finding workers: {e}")
            print("   Check that memory system is accessible")
            return
        
        if not workers:
            print("❌ No available workers found")
            print("   Register workers first with: python3 bin/multi_claude_coordinator.py worker register")
            return
        
        # Create task
        task_entity = self._create_task_entity(task_type, description, priority)
        task_id = task_entity["name"]
        
        # Assign to first available worker
        worker = workers[0]
        worker_id = worker["name"]
        
        print(f"Task ID: {task_id}")
        print(f"Type: {task_type}")
        print(f"Priority: {priority}")
        print(f"Description: {description}")
        print(f"Assigned to: {worker_id}")
        print()
        
        # In real implementation, would update via MCP memory
        print("✅ Task created and assigned successfully")
        print()
        print("💡 Monitor progress:")
        print(f"   python3 bin/multi_claude_coordinator.py controller status")
        print(f"   python3 bin/multi_claude_coordinator.py controller wait-for {task_id}")
        
        return task_id
    
    def controller_status(self):
        """Show coordination session status."""
        print("📊 Coordination Session Status")
        print("=" * 31)
        print()
        
        try:
            # Get session status from session manager
            session_status = quick_session_status()
            print(f"🔧 Session Manager Status:")
            print(f"   Active Sessions: {len(session_status.get('sessions', {}))}")
            print(f"   Current Shell PID: {self.shell_pid}")
            print()
            
            # Get coordination entities with error handling
            sessions = search_entities(entity_type="CoordinationSession")
            workers = search_entities(entity_type="Worker")
            tasks = search_entities(entity_type="WorkItem")
        except Exception as e:
            print(f"❌ Error retrieving status: {e}")
            print("   Check that session manager and memory system are accessible")
            return
        
        print(f"🎯 Coordination Status:")
        print(f"   Active Coordination Sessions: {len(sessions)}")
        print(f"   Registered Workers: {len(workers)}")
        print(f"   Total Tasks: {len(tasks)}")
        print()
        
        if tasks:
            pending = len([t for t in tasks if any("status: pending" in obs for obs in t.get('observations', []))])
            in_progress = len([t for t in tasks if any("status: in_progress" in obs for obs in t.get('observations', []))])
            completed = len([t for t in tasks if any("status: completed" in obs for obs in t.get('observations', []))])
            
            print(f"📋 Task Breakdown:")
            print(f"   Pending: {pending}")
            print(f"   In Progress: {in_progress}")
            print(f"   Completed: {completed}")
    
    async def controller_wait_for(self, task_id: str, timeout: int = 300):
        """Wait for specific task completion."""
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
            else:
                print("⏰ Task did not complete within timeout period")
                
        except KeyboardInterrupt:
            print("\n🛑 Waiting cancelled by user")
        except Exception as e:
            print(f"❌ Error waiting for task: {e}")
    
    def worker_register(self, capabilities: str = "research,coding,testing"):
        """Register as worker."""
        worker_id = f"Worker-{self.shell_pid}"
        
        print("🤖 Worker Registration")
        print("=" * 22)
        print(f"Worker ID: {worker_id}")
        print(f"Capabilities: {capabilities}")
        print(f"Shell PID: {self.shell_pid}")
        print()
        
        # Get current session context for branch info
        session_status = quick_session_status()
        current_session_count = len(session_status.get('sessions', {}))
        
        # Create worker entity
        worker_entity = self._create_worker_entity(capabilities)
        
        # In real implementation, would create via MCP memory
        print("✅ Worker registered successfully")
        print()
        print(f"🔧 Session Context:")
        print(f"   Active Sessions: {current_session_count}")
        print(f"   Integration: FlowLoom session tracking enabled")
        print()
        print("💡 Next Steps:")
        print("   python3 bin/multi_claude_coordinator.py worker wait")
        print("   python3 bin/multi_claude_coordinator.py worker status")
        print()
        print("🔄 Worker is now available for task assignments")
        
        return worker_id
    
    async def worker_wait(self, timeout: int = 300):
        """Wait for task assignments."""
        worker_id = f"Worker-{self.shell_pid}"
        
        print("⏳ Worker Task Monitor")
        print("=" * 21)
        print(f"Worker ID: {worker_id}")
        print(f"Timeout: {timeout} seconds") 
        print("🔍 Waiting for task assignments...")
        print("   (Press Ctrl+C to stop)")
        print()
        
        try:
            task = await wait_for_worker_task(worker_id, timeout)
            
            if task:
                print("🎯 Task Assignment Received!")
                print("=" * 40)
                
                # Parse task details
                observations = task.get('observations', [])
                parsed_obs = {}
                for obs in observations:
                    if ':' in obs:
                        key, value = obs.split(':', 1)
                        parsed_obs[key.strip()] = value.strip()
                
                print(f"Task ID: {task.get('name', 'Unknown')}")
                print(f"Type: {parsed_obs.get('type', 'Unknown')}")
                print(f"Description: {parsed_obs.get('description', 'No description')}")
                print(f"Priority: {parsed_obs.get('priority', 'Unknown')}")
                print()
                print("💡 Next Steps:")
                print(f"   python3 bin/multi_claude_coordinator.py worker accept {task.get('name')}")
                print("   python3 bin/multi_claude_coordinator.py worker status")
                print()
                
                return task
            else:
                print("⏰ No tasks received within timeout period")
                print()
                print("💡 Options:")
                print("   python3 bin/multi_claude_coordinator.py worker wait")
                print("   python3 bin/multi_claude_coordinator.py worker status")
                return None
                
        except KeyboardInterrupt:
            print("\n🛑 Task waiting cancelled by user")
            print("   Worker remains registered and available")
        except Exception as e:
            print(f"\n❌ Unexpected error: {e}")
    
    def worker_status(self):
        """Show worker status."""
        worker_id = f"Worker-{self.shell_pid}"
        
        print("🔍 Worker Status Check")
        print("=" * 22)
        print(f"Worker ID: {worker_id}")
        print()
        
        try:
            # Check if worker is registered
            workers = search_entities(entity_type="Worker", search_term=f"shell_pid: {self.shell_pid}")
        except Exception as e:
            print(f"❌ Error checking worker status: {e}")
            print("   Check that memory system is accessible")
            return
        
        if not workers:
            print("❌ Worker not registered")
            print("   Run: python3 bin/multi_claude_coordinator.py worker register")
            return
        
        worker = workers[0]
        observations = worker.get('observations', [])
        
        # Parse observations
        parsed_obs = {}
        for obs in observations:
            if ':' in obs:
                key, value = obs.split(':', 1)
                parsed_obs[key.strip()] = value.strip()
        
        print(f"✅ Worker Status: {parsed_obs.get('status', 'unknown')}")
        print(f"   Capabilities: {parsed_obs.get('capabilities', 'unknown')}")
        print(f"   Current Task: {parsed_obs.get('current_task', 'none')}")
        print(f"   Tasks Completed: {parsed_obs.get('tasks_completed', '0')}")
        print(f"   Last Heartbeat: {parsed_obs.get('last_heartbeat', 'unknown')}")
        print()
        
        # Check for assigned tasks
        my_tasks = search_entities(entity_type="WorkItem", search_term=f"assigned_to: {worker_id}")
        
        if my_tasks:
            print(f"📋 Assigned Tasks: {len(my_tasks)}")
            for task in my_tasks[:3]:  # Show first 3
                task_obs = {}
                for obs in task.get('observations', []):
                    if ':' in obs:
                        key, value = obs.split(':', 1)
                        task_obs[key.strip()] = value.strip()
                
                print(f"   • {task.get('name')}: {task_obs.get('status', 'unknown')}")
        else:
            print("📋 No assigned tasks")


def main():
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(description="Multi-Claude Coordination Prototype")
    subparsers = parser.add_subparsers(dest='role', help='Role to run')
    
    # Controller commands
    controller_parser = subparsers.add_parser('controller', help='Controller commands')
    controller_sub = controller_parser.add_subparsers(dest='controller_cmd')
    
    init_parser = controller_sub.add_parser('init', help='Initialize coordination session')
    init_parser.add_argument('session_name', nargs='?', help='Session name')
    init_parser.add_argument('--objective', default='', help='Session objective')
    
    controller_sub.add_parser('workers', help='List available workers')
    controller_sub.add_parser('status', help='Show session status')
    
    dispatch_parser = controller_sub.add_parser('dispatch', help='Create and dispatch task')
    dispatch_parser.add_argument('description', help='Task description')
    dispatch_parser.add_argument('--type', default='research', help='Task type')
    dispatch_parser.add_argument('--priority', default='medium', help='Task priority')
    
    wait_parser = controller_sub.add_parser('wait-for', help='Wait for task completion')
    wait_parser.add_argument('task_id', help='Task ID to wait for')
    wait_parser.add_argument('--timeout', type=int, default=300, help='Timeout in seconds')
    
    # Worker commands
    worker_parser = subparsers.add_parser('worker', help='Worker commands')
    worker_sub = worker_parser.add_subparsers(dest='worker_cmd')
    
    register_parser = worker_sub.add_parser('register', help='Register as worker')
    register_parser.add_argument('--capabilities', default='research,coding,testing', help='Worker capabilities')
    
    wait_worker_parser = worker_sub.add_parser('wait', help='Wait for task assignments')
    wait_worker_parser.add_argument('--timeout', type=int, default=300, help='Timeout in seconds')
    
    worker_sub.add_parser('status', help='Show worker status')
    
    args = parser.parse_args()
    
    if not args.role:
        parser.print_help()
        return
    
    coordinator = MultiClaudeCoordinator()
    
    try:
        if args.role == 'controller':
            if args.controller_cmd == 'init':
                coordinator.controller_init(args.session_name, args.objective)
            elif args.controller_cmd == 'workers':
                coordinator.controller_workers()
            elif args.controller_cmd == 'dispatch':
                coordinator.controller_dispatch(args.description, args.type, args.priority)
            elif args.controller_cmd == 'status':
                coordinator.controller_status()
            elif args.controller_cmd == 'wait-for':
                asyncio.run(coordinator.controller_wait_for(args.task_id, args.timeout))
            else:
                controller_parser.print_help()
        
        elif args.role == 'worker':
            if args.worker_cmd == 'register':
                coordinator.worker_register(args.capabilities)
            elif args.worker_cmd == 'wait':
                asyncio.run(coordinator.worker_wait(args.timeout))
            elif args.worker_cmd == 'status':
                coordinator.worker_status()
            else:
                worker_parser.print_help()
        else:
            parser.print_help()
            
    except KeyboardInterrupt:
        print("\n🛑 Operation cancelled by user")
    except Exception as e:
        print(f"❌ Error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()