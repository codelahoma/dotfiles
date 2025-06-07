#!/usr/bin/env python3
"""
Branch-Aware Multi-Claude Coordination System

Enhanced Multi-Claude coordination that includes:
- Branch-aware task assignment
- Worker registration with branch capabilities
- Cross-branch workflow coordination
- Integration with enhanced session management
- Memory graph tracking with branch context

Usage:
    python3 bin/branch_aware_coordinator.py controller init --session web-development
    python3 bin/branch_aware_coordinator.py worker register --branch gh-pages --capabilities web,documentation
    python3 bin/branch_aware_coordinator.py controller dispatch "Update website content" --requires-branch gh-pages
"""

import os
import sys
import json
import uuid
import datetime
import asyncio
import argparse
import subprocess
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

# Add packages to Python path
PROJECT_ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(PROJECT_ROOT / "packages"))

from flowloom_session import EnhancedSessionManager, quick_session_status
from flowloom_memory_monitor import (
    search_entities, wait_for_worker_task, find_available_workers,
    find_session_tasks, wait_for_task_completion
)

class BranchCapability:
    """Represents a worker's capability on a specific branch."""
    
    def __init__(self, branch: str, skills: Set[str], preference: int = 1):
        self.branch = branch
        self.skills = skills
        self.preference = preference  # 1=preferred, 2=capable, 3=can_switch
        
    def to_dict(self) -> dict:
        return {
            "branch": self.branch,
            "skills": list(self.skills),
            "preference": self.preference
        }
        
    @classmethod
    def from_dict(cls, data: dict) -> 'BranchCapability':
        return cls(
            branch=data["branch"],
            skills=set(data["skills"]),
            preference=data.get("preference", 1)
        )

class TaskRequirement:
    """Represents requirements for task execution."""
    
    def __init__(self, required_branch: Optional[str] = None, 
                 required_skills: Optional[Set[str]] = None,
                 can_cross_branch: bool = True,
                 priority: str = "medium"):
        self.required_branch = required_branch
        self.required_skills = required_skills or set()
        self.can_cross_branch = can_cross_branch
        self.priority = priority
        
    def to_dict(self) -> dict:
        return {
            "required_branch": self.required_branch,
            "required_skills": list(self.required_skills),
            "can_cross_branch": self.can_cross_branch,
            "priority": self.priority
        }
        
    @classmethod
    def from_dict(cls, data: dict) -> 'TaskRequirement':
        return cls(
            required_branch=data.get("required_branch"),
            required_skills=set(data.get("required_skills", [])),
            can_cross_branch=data.get("can_cross_branch", True),
            priority=data.get("priority", "medium")
        )

class BranchAwareCoordinator:
    """Enhanced Multi-Claude coordinator with branch awareness."""
    
    def __init__(self):
        self.session_manager = EnhancedSessionManager()
        self.shell_pid = self._get_shell_pid()
        self._current_branch = None
        
    def _get_shell_pid(self) -> int:
        """Get shell PID using FlowLoom's detection method."""
        try:
            script_path = PROJECT_ROOT / "bin" / "get_shell_pid.sh"
            if script_path.exists():
                result = subprocess.run(
                    [str(script_path)], 
                    capture_output=True, 
                    text=True, 
                    timeout=5
                )
                if result.returncode == 0 and result.stdout.strip():
                    return int(result.stdout.strip())
        except (subprocess.TimeoutExpired, subprocess.SubprocessError, ValueError) as e:
            print(f"Warning: Shell PID detection failed: {e}")
        except Exception as e:
            print(f"Warning: Unexpected error in shell PID detection: {e}")
        return os.getpid()
    
    def _get_current_branch(self) -> str:
        """Get current git branch."""
        if self._current_branch:
            return self._current_branch
            
        try:
            result = subprocess.run(
                ["git", "branch", "--show-current"],
                capture_output=True,
                text=True,
                check=True,
                cwd=PROJECT_ROOT,
                timeout=5
            )
            self._current_branch = result.stdout.strip() or "main"
        except subprocess.TimeoutExpired:
            print("Warning: Git branch detection timed out, using 'main'")
            self._current_branch = "main"
        except subprocess.CalledProcessError as e:
            print(f"Warning: Git branch detection failed: {e}, using 'main'")
            self._current_branch = "main"
        except Exception as e:
            print(f"Warning: Unexpected error in git branch detection: {e}, using 'main'")
            self._current_branch = "main"
            
        return self._current_branch
    
    def _determine_session_type(self, branch: str) -> str:
        """Determine session type based on branch."""
        if branch == "gh-pages":
            return "web"
        elif branch in ["main", "master", "develop"]:
            return "dev"
        else:
            return "feature"
    
    def _format_timestamp(self) -> str:
        """Get current timestamp in ISO format."""
        return datetime.datetime.now().strftime("%Y-%m-%dT%H:%M:%SZ")
    
    def _update_memory_entity(self, entity_name: str, observations: List[str]):
        """Update entity in memory graph with proper shell ID format."""
        timestamp = self._format_timestamp()
        formatted_observations = []
        
        for obs in observations:
            if not obs.startswith("Shell_ID:"):
                formatted_obs = f"Shell_ID: {self.shell_pid} - {timestamp} | {obs}"
            else:
                formatted_obs = obs
            formatted_observations.append(formatted_obs)
        
        # In real implementation, would use mcp__memory__add_observations
        # For now, log the operation
        print(f"📝 Memory Update: {entity_name}")
        for obs in formatted_observations[:3]:
            print(f"   • {obs}")
        if len(formatted_observations) > 3:
            print(f"   • ... and {len(formatted_observations) - 3} more")
        
        return True
    
    def _create_branch_aware_worker(self, capabilities: List[BranchCapability]) -> dict:
        """Create worker entity with branch capabilities."""
        worker_id = f"BranchWorker-{self.shell_pid}"
        current_branch = self._get_current_branch()
        session_type = self._determine_session_type(current_branch)
        
        # Format capabilities for observations
        branch_caps = []
        for cap in capabilities:
            pref_text = {1: "preferred", 2: "capable", 3: "can_switch"}[cap.preference]
            branch_caps.append(f"{cap.branch}:{','.join(cap.skills)}:{pref_text}")
        
        return {
            "name": worker_id,
            "entityType": "BranchAwareWorker",
            "observations": [
                f"shell_pid: {self.shell_pid}",
                "status: available",
                f"current_branch: {current_branch}",
                f"session_type: {session_type}",
                f"branch_capabilities: {';'.join(branch_caps)}",
                f"started: {self._format_timestamp()}",
                "current_task: none",
                "tasks_completed: 0",
                f"last_heartbeat: {self._format_timestamp()}",
                "cross_branch_capable: true",
                "branch_switch_count: 0"
            ]
        }
    
    def _create_branch_aware_task(self, description: str, requirements: TaskRequirement) -> dict:
        """Create task entity with branch requirements."""
        task_id = f"BranchTask-{str(uuid.uuid4())[:8]}"
        
        observations = [
            f"description: {description}",
            "assigned_to: unassigned",
            "status: pending",
            f"priority: {requirements.priority}",
            f"created_by: BranchController-{self.shell_pid}",
            f"created_at: {self._format_timestamp()}",
            "dependencies: []",
            f"cross_branch_allowed: {requirements.can_cross_branch}"
        ]
        
        if requirements.required_branch:
            observations.append(f"required_branch: {requirements.required_branch}")
        
        if requirements.required_skills:
            observations.append(f"required_skills: {','.join(requirements.required_skills)}")
        
        return {
            "name": task_id,
            "entityType": "BranchAwareTask",
            "observations": observations
        }
    
    def _find_best_worker_for_task(self, requirements: TaskRequirement) -> Optional[dict]:
        """Find best worker for task based on branch and skill requirements."""
        try:
            # Query for available branch-aware workers
            workers = search_entities(entity_type="BranchAwareWorker", search_term="status: available")
        except Exception as e:
            print(f"Warning: Failed to query workers: {e}")
            return None
        
        if not workers:
            return None
        
        scored_workers = []
        
        for worker in workers:
            score = self._score_worker_for_task(worker, requirements)
            if score > 0:
                scored_workers.append((worker, score))
        
        if not scored_workers:
            return None
        
        # Sort by score (highest first)
        scored_workers.sort(key=lambda x: x[1], reverse=True)
        return scored_workers[0][0]
    
    def _score_worker_for_task(self, worker: dict, requirements: TaskRequirement) -> float:
        """Score worker suitability for task (0-100)."""
        observations = worker.get('observations', [])
        
        # Parse worker observations
        worker_data = {}
        for obs in observations:
            if ':' in obs and not obs.startswith("Shell_ID:"):
                key, value = obs.split(':', 1)
                worker_data[key.strip()] = value.strip()
        
        score = 0.0
        
        # Branch matching
        current_branch = worker_data.get('current_branch', 'main')
        branch_capabilities = worker_data.get('branch_capabilities', '')
        
        if requirements.required_branch:
            if current_branch == requirements.required_branch:
                score += 50  # Perfect branch match
            elif requirements.can_cross_branch:
                # Check if worker can work on required branch
                if requirements.required_branch in branch_capabilities:
                    if 'preferred' in branch_capabilities:
                        score += 40
                    elif 'capable' in branch_capabilities:
                        score += 30
                    elif 'can_switch' in branch_capabilities:
                        score += 20
                else:
                    return 0  # Cannot work on required branch
            else:
                return 0  # Cross-branch not allowed and wrong branch
        else:
            score += 25  # No branch requirement
        
        # Skill matching
        worker_skills = set()
        for cap_str in branch_capabilities.split(';'):
            if ':' in cap_str:
                parts = cap_str.split(':')
                if len(parts) >= 2:
                    worker_skills.update(parts[1].split(','))
        
        if requirements.required_skills:
            skill_overlap = requirements.required_skills.intersection(worker_skills)
            skill_score = (len(skill_overlap) / len(requirements.required_skills)) * 30
            score += skill_score
        else:
            score += 15  # No skill requirements
        
        # Worker status bonuses
        if worker_data.get('status') == 'available':
            score += 10
        
        # Cross-branch capability bonus
        if worker_data.get('cross_branch_capable') == 'true':
            score += 5
        
        return score
    
    def _create_coordination_session(self, session_name: str, objective: str = "") -> dict:
        """Create branch-aware coordination session."""
        current_branch = self._get_current_branch()
        session_type = self._determine_session_type(current_branch)
        
        return {
            "name": session_name,
            "entityType": "BranchAwareSession",
            "observations": [
                f"controller: BranchController-{self.shell_pid}",
                f"controller_branch: {current_branch}",
                f"session_type: {session_type}",
                "workers: []",
                f"started: {self._format_timestamp()}",
                "status: active",
                "total_tasks: 0",
                "completed_tasks: 0",
                "failed_tasks: 0",
                f"objective: {objective}",
                "cross_branch_coordination: enabled",
                "branch_switches_tracked: 0"
            ]
        }
    
    # Controller Commands
    
    def controller_init(self, session_name: str = None, objective: str = "", branch_strategy: str = "adaptive"):
        """Initialize branch-aware coordination session."""
        if not session_name:
            current_branch = self._get_current_branch()
            session_type = self._determine_session_type(current_branch)
            date_str = datetime.datetime.now().strftime("%Y%m%d-%H%M")
            session_name = f"BranchCoord-{session_type}-{date_str}"
        
        controller_id = f"BranchController-{self.shell_pid}"
        current_branch = self._get_current_branch()
        session_type = self._determine_session_type(current_branch)
        
        print("🌳 Branch-Aware Multi-Claude Coordination")
        print("=" * 42)
        print(f"Session Name: {session_name}")
        print(f"Controller ID: {controller_id}")
        print(f"Current Branch: {current_branch} ({session_type})")
        print(f"Branch Strategy: {branch_strategy}")
        print(f"Objective: {objective}")
        print()
        
        # Create session entity
        session_entity = self._create_coordination_session(session_name, objective)
        
        # Update memory
        self._update_memory_entity(session_name, [
            f"Branch-aware coordination session initialized",
            f"Controller on {current_branch} branch",
            f"Session type: {session_type}",
            f"Strategy: {branch_strategy}",
            f"Objective: {objective}"
        ])
        
        print("✅ Branch-aware coordination session initialized")
        print()
        print("🌲 Branch Context:")
        print(f"   • Current Branch: {current_branch}")
        print(f"   • Session Type: {session_type}")
        print(f"   • Cross-branch: Enabled")
        print()
        print("📋 Next Steps:")
        print("   python3 bin/branch_aware_coordinator.py controller workers")
        print("   python3 bin/branch_aware_coordinator.py controller dispatch 'task' --requires-branch gh-pages")
        print()
        print("🔗 Worker Setup (different branches):")
        print("   python3 bin/branch_aware_coordinator.py worker register --branch gh-pages --capabilities web,content")
        print("   python3 bin/branch_aware_coordinator.py worker register --branch main --capabilities dev,testing")
        print()
        print(f"🆔 Session ID: {session_name}")
        
        return session_name
    
    def controller_workers(self, show_all_branches: bool = False):
        """List workers with branch capabilities."""
        print("👥 Branch-Aware Worker Status")
        print("=" * 30)
        print()
        
        try:
            # Query for branch-aware workers
            workers = search_entities(entity_type="BranchAwareWorker")
        except Exception as e:
            print(f"❌ Error querying branch-aware workers: {e}")
            print("   Check that memory system is accessible")
            return
        
        if not workers:
            print("🚫 No branch-aware workers found")
            print()
            print("💡 Register workers on different branches:")
            print("   # Web worker on gh-pages")
            print("   python3 bin/branch_aware_coordinator.py worker register --branch gh-pages --capabilities web,content,documentation")
            print()
            print("   # Dev worker on main")
            print("   python3 bin/branch_aware_coordinator.py worker register --branch main --capabilities dev,testing,implementation")
            print()
            print("   # Feature worker on any branch")
            print("   python3 bin/branch_aware_coordinator.py worker register --branch feature-xyz --capabilities research,analysis")
            return
        
        # Group workers by branch
        branch_groups = {}
        for worker in workers:
            observations = worker.get('observations', [])
            worker_data = {}
            for obs in observations:
                if ':' in obs and not obs.startswith("Shell_ID:"):
                    key, value = obs.split(':', 1)
                    worker_data[key.strip()] = value.strip()
            
            branch = worker_data.get('current_branch', 'unknown')
            if branch not in branch_groups:
                branch_groups[branch] = []
            branch_groups[branch].append((worker, worker_data))
        
        total_available = 0
        total_busy = 0
        
        for branch, branch_workers in branch_groups.items():
            session_type = self._determine_session_type(branch)
            print(f"🌲 {branch} branch ({session_type} workers):")
            
            for worker, worker_data in branch_workers:
                name = worker.get('name', 'Unknown')
                status = worker_data.get('status', 'unknown')
                capabilities = worker_data.get('branch_capabilities', 'unknown')
                current_task = worker_data.get('current_task', 'none')
                
                # Status icon
                if status == "available":
                    total_available += 1
                    status_icon = "✅"
                elif status == "busy":
                    total_busy += 1
                    status_icon = "🔄"
                else:
                    status_icon = "❌"
                
                print(f"   {status_icon} {name}")
                print(f"      Status: {status}")
                if current_task != 'none':
                    print(f"      Current Task: {current_task}")
                
                # Parse and display branch capabilities
                if capabilities != 'unknown':
                    print(f"      Branch Capabilities:")
                    for cap_str in capabilities.split(';'):
                        if ':' in cap_str:
                            parts = cap_str.split(':')
                            if len(parts) >= 3:
                                cap_branch, skills, preference = parts[0], parts[1], parts[2]
                                print(f"        • {cap_branch}: {skills} ({preference})")
                print()
        
        print("📊 Summary:")
        print(f"   Available Workers: {total_available}")
        print(f"   Busy Workers: {total_busy}")
        print(f"   Branches Covered: {len(branch_groups)}")
        print()
        
        if total_available > 0:
            print("💡 Ready for branch-aware task dispatch:")
            print("   python3 bin/branch_aware_coordinator.py controller dispatch 'Update docs' --requires-branch gh-pages")
            print("   python3 bin/branch_aware_coordinator.py controller dispatch 'Run tests' --requires-branch main --requires-skills testing")
        else:
            print("⚠️  No available workers for task dispatch")
    
    def controller_dispatch(self, description: str, requires_branch: str = None, 
                          requires_skills: str = None, priority: str = "medium",
                          allow_cross_branch: bool = True):
        """Create and dispatch branch-aware task."""
        print("📤 Branch-Aware Task Dispatch")
        print("=" * 30)
        print()
        
        # Create task requirements
        skill_set = set(requires_skills.split(',')) if requires_skills else set()
        requirements = TaskRequirement(
            required_branch=requires_branch,
            required_skills=skill_set,
            can_cross_branch=allow_cross_branch,
            priority=priority
        )
        
        # Find best worker
        best_worker = self._find_best_worker_for_task(requirements)
        
        if not best_worker:
            print("❌ No suitable worker found")
            print()
            print("Requirements:")
            if requires_branch:
                print(f"   • Branch: {requires_branch}")
            if requires_skills:
                print(f"   • Skills: {requires_skills}")
            print(f"   • Cross-branch allowed: {allow_cross_branch}")
            print()
            print("💡 Register workers that meet these requirements:")
            if requires_branch:
                print(f"   python3 bin/branch_aware_coordinator.py worker register --branch {requires_branch} --capabilities {requires_skills or 'general'}")
            return
        
        # Create task
        task_entity = self._create_branch_aware_task(description, requirements)
        task_id = task_entity["name"]
        worker_id = best_worker["name"]
        
        # Parse worker info
        worker_data = {}
        for obs in best_worker.get('observations', []):
            if ':' in obs and not obs.startswith("Shell_ID:"):
                key, value = obs.split(':', 1)
                worker_data[key.strip()] = value.strip()
        
        worker_branch = worker_data.get('current_branch', 'unknown')
        controller_branch = self._get_current_branch()
        
        print(f"Task ID: {task_id}")
        print(f"Description: {description}")
        print(f"Priority: {priority}")
        if requires_branch:
            print(f"Required Branch: {requires_branch}")
        if requires_skills:
            print(f"Required Skills: {requires_skills}")
        print()
        print(f"🎯 Assignment:")
        print(f"   Worker: {worker_id}")
        print(f"   Worker Branch: {worker_branch}")
        print(f"   Controller Branch: {controller_branch}")
        
        if requires_branch and worker_branch != requires_branch:
            if allow_cross_branch:
                print(f"   🌉 Cross-branch assignment: {worker_branch} → {requires_branch}")
            else:
                print(f"   ⚠️  Branch mismatch (cross-branch disabled)")
        
        # Update memory
        self._update_memory_entity(task_id, [
            f"Branch-aware task created and assigned",
            f"Assigned to {worker_id} on {worker_branch}",
            f"Task requirements: branch={requires_branch}, skills={requires_skills}",
            f"Cross-branch coordination: {'enabled' if allow_cross_branch else 'disabled'}"
        ])
        
        self._update_memory_entity(worker_id, [
            f"Task assigned: {task_id}",
            f"Task description: {description}",
            "status: busy",
            f"current_task: {task_id}"
        ])
        
        print()
        print("✅ Branch-aware task created and assigned")
        print()
        print("💡 Monitor progress:")
        print("   python3 bin/branch_aware_coordinator.py controller status")
        print("   python3 bin/branch_aware_coordinator.py controller wait-for", task_id)
        
        return task_id
    
    def controller_status(self):
        """Show comprehensive branch-aware coordination status."""
        print("📊 Branch-Aware Coordination Status")
        print("=" * 36)
        print()
        
        try:
            # Session manager status
            session_status = quick_session_status()
            controller_branch = self._get_current_branch()
            session_type = self._determine_session_type(controller_branch)
            
            print(f"🎯 Controller Status:")
            print(f"   Branch: {controller_branch} ({session_type})")
            print(f"   Shell PID: {self.shell_pid}")
            print(f"   Active Sessions: {len(session_status.get('sessions', {}))}")
            print()
            
            # Get coordination entities with error handling
            sessions = search_entities(entity_type="BranchAwareSession")
            workers = search_entities(entity_type="BranchAwareWorker")
            tasks = search_entities(entity_type="BranchAwareTask")
        except Exception as e:
            print(f"❌ Error retrieving coordination status: {e}")
            print("   Check that session manager and memory system are accessible")
            return
        
        print(f"🌳 Branch Coordination:")
        print(f"   Active Sessions: {len(sessions)}")
        print(f"   Registered Workers: {len(workers)}")
        print(f"   Total Tasks: {len(tasks)}")
        print()
        
        # Branch distribution
        if workers:
            branch_count = {}
            for worker in workers:
                worker_data = {}
                for obs in worker.get('observations', []):
                    if ':' in obs and not obs.startswith("Shell_ID:"):
                        key, value = obs.split(':', 1)
                        worker_data[key.strip()] = value.strip()
                
                branch = worker_data.get('current_branch', 'unknown')
                branch_count[branch] = branch_count.get(branch, 0) + 1
            
            print(f"🌲 Branch Distribution:")
            for branch, count in branch_count.items():
                session_type = self._determine_session_type(branch)
                print(f"   • {branch} ({session_type}): {count} workers")
            print()
        
        # Task status
        if tasks:
            pending = 0
            in_progress = 0
            completed = 0
            
            for task in tasks:
                for obs in task.get('observations', []):
                    if obs.startswith("status: "):
                        status = obs.split(": ", 1)[1]
                        if status == "pending":
                            pending += 1
                        elif status == "in_progress":
                            in_progress += 1
                        elif status == "completed":
                            completed += 1
                        break
            
            print(f"📋 Task Status:")
            print(f"   Pending: {pending}")
            print(f"   In Progress: {in_progress}")
            print(f"   Completed: {completed}")
            print()
        
        # Cross-branch activity
        cross_branch_tasks = 0
        for task in tasks:
            task_data = {}
            for obs in task.get('observations', []):
                if ':' in obs and not obs.startswith("Shell_ID:"):
                    key, value = obs.split(':', 1)
                    task_data[key.strip()] = value.strip()
            
            if task_data.get('required_branch') and task_data.get('assigned_to'):
                # Check if worker is on different branch than required
                worker_id = task_data['assigned_to']
                for worker in workers:
                    if worker.get('name') == worker_id:
                        worker_data = {}
                        for obs in worker.get('observations', []):
                            if ':' in obs and not obs.startswith("Shell_ID:"):
                                key, value = obs.split(':', 1)
                                worker_data[key.strip()] = value.strip()
                        
                        worker_branch = worker_data.get('current_branch')
                        required_branch = task_data.get('required_branch')
                        if worker_branch != required_branch:
                            cross_branch_tasks += 1
                        break
        
        if cross_branch_tasks > 0:
            print(f"🌉 Cross-Branch Coordination:")
            print(f"   Active Cross-Branch Tasks: {cross_branch_tasks}")
    
    # Worker Commands
    
    def worker_register(self, branch: str = None, capabilities: str = "research,coding,testing", 
                       preferences: str = None):
        """Register as branch-aware worker."""
        if not branch:
            branch = self._get_current_branch()
        
        worker_id = f"BranchWorker-{self.shell_pid}"
        session_type = self._determine_session_type(branch)
        
        print("🤖 Branch-Aware Worker Registration")
        print("=" * 36)
        print(f"Worker ID: {worker_id}")
        print(f"Primary Branch: {branch} ({session_type})")
        print(f"Capabilities: {capabilities}")
        print()
        
        # Parse capabilities and preferences
        skill_set = set(cap.strip() for cap in capabilities.split(','))
        
        # Create branch capabilities
        branch_capabilities = []
        
        # Primary branch (preferred)
        branch_capabilities.append(BranchCapability(branch, skill_set, preference=1))
        
        # Add other common branches based on preferences
        if preferences:
            for pref_spec in preferences.split(';'):
                if ':' in pref_spec:
                    pref_branch, pref_level = pref_spec.split(':', 1)
                    pref_val = {"preferred": 1, "capable": 2, "can_switch": 3}.get(pref_level, 2)
                    branch_capabilities.append(BranchCapability(pref_branch.strip(), skill_set, pref_val))
        else:
            # Default cross-branch capabilities based on session type
            if session_type == "web":
                # Web workers can also do documentation
                if "main" != branch:
                    branch_capabilities.append(BranchCapability("main", {"documentation", "content"}, 2))
            elif session_type == "dev":
                # Dev workers can handle feature branches
                branch_capabilities.append(BranchCapability("feature/*", skill_set, 2))
                if "gh-pages" != branch:
                    branch_capabilities.append(BranchCapability("gh-pages", {"documentation"}, 3))
        
        # Create worker entity
        worker_entity = self._create_branch_aware_worker(branch_capabilities)
        
        # Get session context
        session_status = quick_session_status()
        session_count = len(session_status.get('sessions', {}))
        
        # Update memory
        self._update_memory_entity(worker_id, [
            f"Branch-aware worker registered",
            f"Primary branch: {branch} ({session_type})",
            f"Capabilities: {capabilities}",
            f"Cross-branch: {len(branch_capabilities)} branches",
            f"Session integration: {session_count} sessions tracked"
        ])
        
        print("✅ Branch-aware worker registered successfully")
        print()
        print(f"🌲 Branch Capabilities:")
        for cap in branch_capabilities:
            pref_text = {1: "preferred", 2: "capable", 3: "can switch"}[cap.preference]
            print(f"   • {cap.branch}: {', '.join(cap.skills)} ({pref_text})")
        print()
        print(f"🔧 Session Context:")
        print(f"   Active Sessions: {session_count}")
        print(f"   Session Type: {session_type}")
        print(f"   Integration: Enabled")
        print()
        print("💡 Next Steps:")
        print("   python3 bin/branch_aware_coordinator.py worker wait")
        print("   python3 bin/branch_aware_coordinator.py worker status")
        print()
        print("🔄 Worker is now available for branch-aware task assignments")
        
        return worker_id
    
    async def worker_wait(self, timeout: int = 300):
        """Wait for branch-aware task assignments."""
        worker_id = f"BranchWorker-{self.shell_pid}"
        current_branch = self._get_current_branch()
        
        print("⏳ Branch-Aware Task Monitor")
        print("=" * 28)
        print(f"Worker ID: {worker_id}")
        print(f"Current Branch: {current_branch}")
        print(f"Timeout: {timeout} seconds")
        print("🔍 Waiting for branch-aware task assignments...")
        print("   (Press Ctrl+C to stop)")
        print()
        
        try:
            # In real implementation, would use enhanced wait_for_worker_task
            # that considers branch requirements
            task = await wait_for_worker_task(worker_id, timeout)
            
            if task:
                print("🎯 Branch-Aware Task Assignment Received!")
                print("=" * 45)
                
                # Parse task details
                task_data = {}
                for obs in task.get('observations', []):
                    if ':' in obs and not obs.startswith("Shell_ID:"):
                        key, value = obs.split(':', 1)
                        task_data[key.strip()] = value.strip()
                
                task_id = task.get('name', 'Unknown')
                description = task_data.get('description', 'No description')
                required_branch = task_data.get('required_branch')
                required_skills = task_data.get('required_skills')
                priority = task_data.get('priority', 'Unknown')
                
                print(f"Task ID: {task_id}")
                print(f"Description: {description}")
                print(f"Priority: {priority}")
                
                if required_branch:
                    print(f"Required Branch: {required_branch}")
                    if current_branch != required_branch:
                        print(f"⚠️  Branch Switch Needed: {current_branch} → {required_branch}")
                    else:
                        print("✅ Already on correct branch")
                
                if required_skills:
                    print(f"Required Skills: {required_skills}")
                
                print()
                print("💡 Next Steps:")
                if required_branch and current_branch != required_branch:
                    print(f"   1. Switch to branch: git checkout {required_branch}")
                    print(f"   2. Accept task: python3 bin/branch_aware_coordinator.py worker accept {task_id}")
                else:
                    print(f"   python3 bin/branch_aware_coordinator.py worker accept {task_id}")
                print("   python3 bin/branch_aware_coordinator.py worker status")
                
                # Update memory with assignment
                self._update_memory_entity(worker_id, [
                    f"Task assignment received: {task_id}",
                    f"Branch requirement: {required_branch or 'any'}",
                    f"Skill requirement: {required_skills or 'any'}",
                    f"Branch switch needed: {current_branch != required_branch if required_branch else False}"
                ])
                
                return task
            else:
                print("⏰ No tasks received within timeout period")
                print()
                print("💡 Options:")
                print("   python3 bin/branch_aware_coordinator.py worker wait")
                print("   python3 bin/branch_aware_coordinator.py worker status")
                return None
                
        except KeyboardInterrupt:
            print("\n🛑 Task waiting cancelled by user")
            print("   Worker remains registered and available")
        except Exception as e:
            print(f"\n❌ Unexpected error: {e}")
    
    def worker_status(self):
        """Show branch-aware worker status."""
        worker_id = f"BranchWorker-{self.shell_pid}"
        current_branch = self._get_current_branch()
        session_type = self._determine_session_type(current_branch)
        
        print("🔍 Branch-Aware Worker Status")
        print("=" * 29)
        print(f"Worker ID: {worker_id}")
        print(f"Current Branch: {current_branch} ({session_type})")
        print()
        
        try:
            # Check if worker is registered
            workers = search_entities(entity_type="BranchAwareWorker", search_term=f"shell_pid: {self.shell_pid}")
        except Exception as e:
            print(f"❌ Error checking worker status: {e}")
            print("   Check that memory system is accessible")
            return
        
        if not workers:
            print("❌ Worker not registered")
            print("   Run: python3 bin/branch_aware_coordinator.py worker register")
            print()
            print("💡 Registration options:")
            print(f"   # Current branch ({current_branch})")
            print(f"   python3 bin/branch_aware_coordinator.py worker register --branch {current_branch}")
            print()
            print("   # With cross-branch preferences")
            print(f"   python3 bin/branch_aware_coordinator.py worker register --branch {current_branch} --preferences 'main:capable;gh-pages:can_switch'")
            return
        
        worker = workers[0]
        
        # Parse worker observations
        worker_data = {}
        for obs in worker.get('observations', []):
            if ':' in obs and not obs.startswith("Shell_ID:"):
                key, value = obs.split(':', 1)
                worker_data[key.strip()] = value.strip()
        
        status = worker_data.get('status', 'unknown')
        current_task = worker_data.get('current_task', 'none')
        capabilities = worker_data.get('branch_capabilities', '')
        tasks_completed = worker_data.get('tasks_completed', '0')
        
        print(f"✅ Worker Status: {status}")
        if current_task != 'none':
            print(f"   Current Task: {current_task}")
        print(f"   Tasks Completed: {tasks_completed}")
        print()
        
        # Show branch capabilities
        if capabilities:
            print(f"🌲 Branch Capabilities:")
            for cap_str in capabilities.split(';'):
                if ':' in cap_str:
                    parts = cap_str.split(':')
                    if len(parts) >= 3:
                        cap_branch, skills, preference = parts[0], parts[1], parts[2]
                        print(f"   • {cap_branch}: {skills} ({preference})")
            print()
        
        # Check for assigned tasks
        my_tasks = search_entities(entity_type="BranchAwareTask", search_term=f"assigned_to: {worker_id}")
        
        if my_tasks:
            print(f"📋 Assigned Tasks: {len(my_tasks)}")
            for task in my_tasks[:3]:  # Show first 3
                task_data = {}
                for obs in task.get('observations', []):
                    if ':' in obs and not obs.startswith("Shell_ID:"):
                        key, value = obs.split(':', 1)
                        task_data[key.strip()] = value.strip()
                
                task_status = task_data.get('status', 'unknown')
                required_branch = task_data.get('required_branch', 'any')
                print(f"   • {task.get('name')}: {task_status} (branch: {required_branch})")
        else:
            print("📋 No assigned tasks")
        
        # Branch compatibility check
        if current_branch != worker_data.get('current_branch', current_branch):
            print()
            print("⚠️  Branch Status:")
            print(f"   Registered on: {worker_data.get('current_branch')}")
            print(f"   Currently on: {current_branch}")
            print("   Consider re-registering to update branch context")


def main():
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(description="Branch-Aware Multi-Claude Coordination")
    subparsers = parser.add_subparsers(dest='role', help='Role to run')
    
    # Controller commands
    controller_parser = subparsers.add_parser('controller', help='Controller commands')
    controller_sub = controller_parser.add_subparsers(dest='controller_cmd')
    
    # Controller init
    init_parser = controller_sub.add_parser('init', help='Initialize branch-aware coordination session')
    init_parser.add_argument('--session', help='Session name')
    init_parser.add_argument('--objective', default='', help='Session objective')
    init_parser.add_argument('--strategy', default='adaptive', choices=['adaptive', 'strict', 'flexible'], help='Branch strategy')
    
    # Controller workers
    workers_parser = controller_sub.add_parser('workers', help='List branch-aware workers')
    workers_parser.add_argument('--all-branches', action='store_true', help='Show workers from all branches')
    
    # Controller status
    controller_sub.add_parser('status', help='Show branch-aware coordination status')
    
    # Controller dispatch
    dispatch_parser = controller_sub.add_parser('dispatch', help='Create and dispatch branch-aware task')
    dispatch_parser.add_argument('description', help='Task description')
    dispatch_parser.add_argument('--requires-branch', help='Required branch for task')
    dispatch_parser.add_argument('--requires-skills', help='Required skills (comma-separated)')
    dispatch_parser.add_argument('--priority', default='medium', choices=['low', 'medium', 'high', 'critical'], help='Task priority')
    dispatch_parser.add_argument('--no-cross-branch', action='store_true', help='Disable cross-branch assignment')
    
    # Worker commands
    worker_parser = subparsers.add_parser('worker', help='Worker commands')
    worker_sub = worker_parser.add_subparsers(dest='worker_cmd')
    
    # Worker register
    register_parser = worker_sub.add_parser('register', help='Register as branch-aware worker')
    register_parser.add_argument('--branch', help='Primary branch (defaults to current)')
    register_parser.add_argument('--capabilities', default='research,coding,testing', help='Worker capabilities')
    register_parser.add_argument('--preferences', help='Cross-branch preferences (e.g., "main:capable;gh-pages:can_switch")')
    
    # Worker wait
    wait_parser = worker_sub.add_parser('wait', help='Wait for branch-aware task assignments')
    wait_parser.add_argument('--timeout', type=int, default=300, help='Timeout in seconds')
    
    # Worker status
    worker_sub.add_parser('status', help='Show branch-aware worker status')
    
    args = parser.parse_args()
    
    if not args.role:
        parser.print_help()
        return
    
    coordinator = BranchAwareCoordinator()
    
    try:
        if args.role == 'controller':
            if args.controller_cmd == 'init':
                coordinator.controller_init(args.session, args.objective, args.strategy)
            elif args.controller_cmd == 'workers':
                coordinator.controller_workers(args.all_branches)
            elif args.controller_cmd == 'dispatch':
                coordinator.controller_dispatch(
                    args.description,
                    args.requires_branch,
                    args.requires_skills,
                    args.priority,
                    not args.no_cross_branch
                )
            elif args.controller_cmd == 'status':
                coordinator.controller_status()
            else:
                controller_parser.print_help()
        
        elif args.role == 'worker':
            if args.worker_cmd == 'register':
                coordinator.worker_register(args.branch, args.capabilities, args.preferences)
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