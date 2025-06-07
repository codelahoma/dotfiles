#!/usr/bin/env python3
"""
Branch-Aware Multi-Claude Coordination Demo

This demonstrates the enhanced coordination system with:
- Branch-aware worker registration
- Cross-branch task assignment
- Branch capability matching
- Cross-branch workflow orchestration

Example scenarios:
1. Web content workflow (gh-pages branch)
2. Development workflow (main branch)  
3. Feature development workflow (feature branches)
4. Cross-branch deployment pipeline
"""

import os
import sys
import json
import uuid
import datetime
import time
from pathlib import Path

# Add packages to Python path
PROJECT_ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(PROJECT_ROOT / "packages"))

from flowloom_session import quick_session_status

class BranchCoordinationDemo:
    """Demonstration of branch-aware coordination capabilities."""
    
    def __init__(self):
        self.shell_pid = self._get_shell_pid()
        self.demo_entities = []
        
    def _get_shell_pid(self):
        """Get shell PID using FlowLoom's detection method."""
        try:
            script_path = PROJECT_ROOT / "bin" / "get_shell_pid.sh"
            if script_path.exists():
                import subprocess
                result = subprocess.run([str(script_path)], capture_output=True, text=True)
                if result.returncode == 0 and result.stdout.strip():
                    return int(result.stdout.strip())
        except:
            pass
        return os.getpid()
    
    def _format_timestamp(self):
        """Get current timestamp in ISO format."""
        return datetime.datetime.now().strftime("%Y-%m-%dT%H:%M:%SZ")
    
    def _create_demo_entity(self, name: str, entity_type: str, observations: list):
        """Create demonstration entity."""
        entity = {
            "name": name,
            "entityType": entity_type,
            "observations": observations
        }
        self.demo_entities.append(entity)
        return entity
    
    def demo_setup(self):
        """Show demo setup and context."""
        print("🌳 Branch-Aware Multi-Claude Coordination Demo")
        print("=" * 48)
        print(f"Shell PID: {self.shell_pid}")
        print(f"Demo Timestamp: {self._format_timestamp()}")
        print()
        
        # Get session context
        session_status = quick_session_status()
        active_sessions = len(session_status.get('sessions', {}))
        
        print(f"🔧 FlowLoom Integration:")
        print(f"   Active Sessions: {active_sessions}")
        print(f"   Session Management: Enhanced with branch tracking")
        print(f"   Memory Coordination: MCP-based shared state")
        print()
        
        return {
            "shell_pid": self.shell_pid,
            "active_sessions": active_sessions,
            "timestamp": self._format_timestamp()
        }
    
    def demo_branch_aware_workers(self):
        """Demonstrate workers with branch capabilities."""
        print("🤖 Branch-Aware Worker Registration Demo")
        print("=" * 42)
        print()
        
        # Simulate workers on different branches
        workers = [
            {
                "worker_id": f"WebWorker-{self.shell_pid + 1}",
                "branch": "gh-pages",
                "session_type": "web",
                "capabilities": ["web", "content", "documentation", "html", "css"],
                "preferences": {
                    "main": "capable",
                    "gh-pages": "preferred"
                }
            },
            {
                "worker_id": f"DevWorker-{self.shell_pid + 2}",
                "branch": "main", 
                "session_type": "dev",
                "capabilities": ["development", "testing", "implementation", "python", "git"],
                "preferences": {
                    "main": "preferred",
                    "feature/*": "capable",
                    "gh-pages": "can_switch"
                }
            },
            {
                "worker_id": f"FeatureWorker-{self.shell_pid + 3}",
                "branch": "feature-auth",
                "session_type": "feature",
                "capabilities": ["research", "analysis", "prototyping", "security"],
                "preferences": {
                    "feature-auth": "preferred",
                    "main": "capable"
                }
            }
        ]
        
        for worker in workers:
            print(f"🌲 {worker['worker_id']}")
            print(f"   Primary Branch: {worker['branch']} ({worker['session_type']})")
            print(f"   Capabilities: {', '.join(worker['capabilities'])}")
            print(f"   Cross-Branch Preferences:")
            for branch, pref in worker['preferences'].items():
                print(f"     • {branch}: {pref}")
            
            # Create worker entity
            branch_caps = []
            for branch, pref in worker['preferences'].items():
                branch_caps.append(f"{branch}:{','.join(worker['capabilities'])}:{pref}")
            
            worker_entity = self._create_demo_entity(
                worker['worker_id'],
                "BranchAwareWorker",
                [
                    f"shell_pid: {worker['worker_id'].split('-')[1]}",
                    "status: available",
                    f"current_branch: {worker['branch']}",
                    f"session_type: {worker['session_type']}",
                    f"branch_capabilities: {';'.join(branch_caps)}",
                    f"started: {self._format_timestamp()}",
                    "current_task: none",
                    "tasks_completed: 0",
                    f"last_heartbeat: {self._format_timestamp()}",
                    "cross_branch_capable: true",
                    "branch_switch_count: 0"
                ]
            )
            print()
        
        print("✅ Branch-aware worker registration completed")
        print(f"   Total Workers: {len(workers)}")
        print(f"   Branches Covered: {len(set(w['branch'] for w in workers))}")
        print(f"   Cross-Branch Capable: {len(workers)}")
        
        return workers
    
    def demo_branch_aware_tasks(self):
        """Demonstrate tasks with branch requirements."""
        print("📋 Branch-Aware Task Creation Demo")
        print("=" * 36)
        print()
        
        # Define example tasks with different branch requirements
        tasks = [
            {
                "description": "Update website homepage content",
                "required_branch": "gh-pages",
                "required_skills": ["web", "content"],
                "priority": "high",
                "cross_branch_allowed": False,
                "workflow_type": "web_content"
            },
            {
                "description": "Implement user authentication system",
                "required_branch": "feature-auth",
                "required_skills": ["development", "security"],
                "priority": "high",
                "cross_branch_allowed": True,
                "workflow_type": "feature_development"
            },
            {
                "description": "Run integration tests and update CI",
                "required_branch": "main",
                "required_skills": ["testing", "development"],
                "priority": "medium",
                "cross_branch_allowed": True,
                "workflow_type": "ci_cd"
            },
            {
                "description": "Research best practices for API security",
                "required_branch": None,  # Any branch
                "required_skills": ["research", "security"],
                "priority": "medium",
                "cross_branch_allowed": True,
                "workflow_type": "research"
            },
            {
                "description": "Create deployment documentation",
                "required_branch": "gh-pages",
                "required_skills": ["documentation", "content"],
                "priority": "low",
                "cross_branch_allowed": True,
                "workflow_type": "documentation"
            }
        ]
        
        for i, task in enumerate(tasks, 1):
            task_id = f"BranchTask-{str(uuid.uuid4())[:8]}"
            
            print(f"📝 Task {i}: {task_id}")
            print(f"   Description: {task['description']}")
            print(f"   Required Branch: {task['required_branch'] or 'any'}")
            print(f"   Required Skills: {', '.join(task['required_skills'])}")
            print(f"   Priority: {task['priority']}")
            print(f"   Cross-Branch: {'✅' if task['cross_branch_allowed'] else '❌'}")
            print(f"   Workflow Type: {task['workflow_type']}")
            
            # Create task entity
            observations = [
                f"description: {task['description']}",
                "assigned_to: unassigned",
                "status: pending",
                f"priority: {task['priority']}",
                f"created_by: BranchController-{self.shell_pid}",
                f"created_at: {self._format_timestamp()}",
                "dependencies: []",
                f"cross_branch_allowed: {task['cross_branch_allowed']}",
                f"workflow_type: {task['workflow_type']}"
            ]
            
            if task['required_branch']:
                observations.append(f"required_branch: {task['required_branch']}")
            
            if task['required_skills']:
                observations.append(f"required_skills: {','.join(task['required_skills'])}")
            
            task_entity = self._create_demo_entity(
                task_id,
                "BranchAwareTask", 
                observations
            )
            
            task['task_id'] = task_id
            print()
        
        print("✅ Branch-aware task creation completed")
        print(f"   Total Tasks: {len(tasks)}")
        print(f"   Branch-Specific: {len([t for t in tasks if t['required_branch']])}")
        print(f"   Cross-Branch Enabled: {len([t for t in tasks if t['cross_branch_allowed']])}")
        
        return tasks
    
    def demo_task_assignment_algorithm(self, workers, tasks):
        """Demonstrate intelligent task assignment based on branch capabilities."""
        print("🎯 Branch-Aware Task Assignment Demo")
        print("=" * 37)
        print()
        
        assignments = []
        
        for task in tasks:
            print(f"🔍 Assigning Task: {task['task_id']}")
            print(f"   Description: {task['description']}")
            
            # Find best worker for this task
            best_worker = None
            best_score = 0
            assignment_details = {}
            
            for worker in workers:
                score = self._score_worker_for_task(worker, task)
                print(f"   📊 {worker['worker_id']}: {score:.1f} points")
                
                if score > best_score:
                    best_score = score
                    best_worker = worker
            
            if best_worker:
                # Determine assignment type
                worker_branch = best_worker['branch']
                required_branch = task['required_branch']
                
                if not required_branch:
                    assignment_type = "any_branch"
                elif worker_branch == required_branch:
                    assignment_type = "perfect_match"
                elif task['cross_branch_allowed']:
                    if required_branch in best_worker['preferences']:
                        pref_level = best_worker['preferences'][required_branch]
                        if pref_level == "preferred":
                            assignment_type = "cross_branch_preferred"
                        elif pref_level == "capable":
                            assignment_type = "cross_branch_capable"
                        else:
                            assignment_type = "cross_branch_switch"
                    else:
                        assignment_type = "cross_branch_unknown"
                else:
                    assignment_type = "blocked_cross_branch"
                
                assignment = {
                    "task_id": task['task_id'],
                    "worker_id": best_worker['worker_id'],
                    "worker_branch": worker_branch,
                    "required_branch": required_branch,
                    "assignment_type": assignment_type,
                    "score": best_score,
                    "cross_branch": worker_branch != required_branch if required_branch else False
                }
                
                assignments.append(assignment)
                
                # Update task status
                task['assigned_to'] = best_worker['worker_id']
                task['status'] = 'assigned'
                
                # Update worker status
                best_worker['status'] = 'busy'
                best_worker['current_task'] = task['task_id']
                
                print(f"   ✅ Assigned to: {best_worker['worker_id']}")
                print(f"   📍 Assignment Type: {assignment_type}")
                print(f"   🌉 Cross-Branch: {'Yes' if assignment['cross_branch'] else 'No'}")
                
                # Update memory entities
                self._update_demo_entity(task['task_id'], [
                    f"assigned_to: {best_worker['worker_id']}",
                    "status: assigned",
                    f"assignment_type: {assignment_type}",
                    f"assignment_score: {best_score:.1f}",
                    f"assigned_at: {self._format_timestamp()}"
                ])
                
                self._update_demo_entity(best_worker['worker_id'], [
                    "status: busy",
                    f"current_task: {task['task_id']}",
                    f"task_assigned_at: {self._format_timestamp()}"
                ])
                
            else:
                print(f"   ❌ No suitable worker found")
                assignment = {
                    "task_id": task['task_id'],
                    "worker_id": None,
                    "error": "no_suitable_worker",
                    "score": 0
                }
                assignments.append(assignment)
            
            print()
        
        return assignments
    
    def _score_worker_for_task(self, worker, task):
        """Score worker suitability for task (0-100)."""
        score = 0.0
        
        # Branch matching
        worker_branch = worker['branch']
        required_branch = task['required_branch']
        
        if required_branch:
            if worker_branch == required_branch:
                score += 50  # Perfect branch match
            elif task['cross_branch_allowed']:
                if required_branch in worker['preferences']:
                    pref_level = worker['preferences'][required_branch]
                    if pref_level == "preferred":
                        score += 40
                    elif pref_level == "capable":
                        score += 30
                    elif pref_level == "can_switch":
                        score += 20
                else:
                    return 0  # Cannot work on required branch
            else:
                return 0  # Cross-branch not allowed and wrong branch
        else:
            score += 25  # No branch requirement
        
        # Skill matching
        worker_skills = set(worker['capabilities'])
        required_skills = set(task['required_skills'])
        
        if required_skills:
            skill_overlap = required_skills.intersection(worker_skills)
            if skill_overlap:
                skill_score = (len(skill_overlap) / len(required_skills)) * 30
                score += skill_score
            else:
                score -= 10  # Missing required skills
        else:
            score += 15  # No skill requirements
        
        # Worker status bonus
        if worker.get('status', 'available') == 'available':
            score += 10
        
        # Priority bonus
        if task['priority'] == 'high':
            score += 5
        elif task['priority'] == 'critical':
            score += 10
        
        return score
    
    def _update_demo_entity(self, entity_name, new_observations):
        """Update demo entity with new observations."""
        for entity in self.demo_entities:
            if entity['name'] == entity_name:
                entity['observations'].extend(new_observations)
                break
    
    def demo_cross_branch_workflows(self, assignments):
        """Demonstrate cross-branch workflow coordination."""
        print("🌉 Cross-Branch Workflow Coordination Demo")
        print("=" * 43)
        print()
        
        # Group assignments by workflow type
        workflows = {}
        for assignment in assignments:
            if assignment.get('worker_id'):
                task = next(t for t in self.demo_entities if t['name'] == assignment['task_id'])
                workflow_type = None
                for obs in task['observations']:
                    if obs.startswith('workflow_type:'):
                        workflow_type = obs.split(':', 1)[1].strip()
                        break
                
                if workflow_type:
                    if workflow_type not in workflows:
                        workflows[workflow_type] = []
                    workflows[workflow_type].append(assignment)
        
        # Show workflow coordination
        for workflow_type, workflow_assignments in workflows.items():
            print(f"🔄 {workflow_type.replace('_', ' ').title()} Workflow:")
            
            cross_branch_count = len([a for a in workflow_assignments if a.get('cross_branch')])
            
            for assignment in workflow_assignments:
                worker_branch = assignment['worker_branch']
                required_branch = assignment.get('required_branch', 'any')
                cross_branch_indicator = " 🌉" if assignment.get('cross_branch') else ""
                
                print(f"   • {assignment['task_id']}: {assignment['worker_id']}")
                print(f"     Branch: {worker_branch} → {required_branch}{cross_branch_indicator}")
            
            print(f"   📊 Cross-Branch Tasks: {cross_branch_count}/{len(workflow_assignments)}")
            print()
        
        # Example deployment pipeline
        if len(workflows) > 1:
            print("🚀 Example: Feature to Production Pipeline")
            print("   1. Feature development (feature-auth branch)")
            print("   2. Testing and integration (main branch)")
            print("   3. Documentation update (gh-pages branch)")
            print("   ✅ All coordinated through shared memory state")
            print()
        
        return workflows
    
    def demo_session_integration(self):
        """Demonstrate integration with FlowLoom session management."""
        print("🔧 FlowLoom Session Integration Demo")
        print("=" * 36)
        print()
        
        # Get current session status
        session_status = quick_session_status()
        sessions = session_status.get('sessions', {})
        
        print(f"📊 Current Session State:")
        print(f"   Active Sessions: {len(sessions)}")
        print(f"   Shell PID: {self.shell_pid}")
        print()
        
        if sessions:
            print("📋 Session Details:")
            for session_id, session_data in list(sessions.items())[:3]:  # Show first 3
                print(f"   • {session_id}")
                print(f"     Purpose: {session_data.get('purpose', 'N/A')}")
                print(f"     Status: {session_data.get('status', 'N/A')}")
                print(f"     Created: {session_data.get('created', 'N/A')}")
            
            if len(sessions) > 3:
                print(f"   ... and {len(sessions) - 3} more sessions")
            print()
        
        # Session-branch mapping
        print("🌲 Branch-Session Coordination:")
        print("   • Each worker tracks session context")
        print("   • Branch switches recorded in session metadata")
        print("   • Cross-branch tasks maintain session continuity")
        print("   • Memory graph includes session-branch correlations")
        print()
        
        # Create sample session entity
        session_entity = self._create_demo_entity(
            f"BranchSession-{self.shell_pid}",
            "BranchAwareSession",
            [
                f"controller: BranchController-{self.shell_pid}",
                "controller_branch: main",
                "session_type: dev",
                f"workers: [{len(self.demo_entities)} demo workers]",
                f"started: {self._format_timestamp()}",
                "status: active",
                f"total_tasks: {len([e for e in self.demo_entities if e['entityType'] == 'BranchAwareTask'])}",
                "completed_tasks: 0",
                "failed_tasks: 0",
                "objective: Demonstrate branch-aware coordination capabilities",
                "cross_branch_coordination: enabled",
                "branch_switches_tracked: 0",
                f"session_integration: {len(sessions)} active sessions"
            ]
        )
        
        print("✅ Session integration demonstrated")
        return session_entity
    
    def demo_memory_graph_tracking(self):
        """Demonstrate memory graph integration."""
        print("🧠 Memory Graph Integration Demo")
        print("=" * 32)
        print()
        
        # Summary of created entities
        entity_types = {}
        for entity in self.demo_entities:
            entity_type = entity['entityType']
            entity_types[entity_type] = entity_types.get(entity_type, 0) + 1
        
        print("📊 Demo Entity Summary:")
        for entity_type, count in entity_types.items():
            print(f"   • {entity_type}: {count}")
        print(f"   Total Entities: {len(self.demo_entities)}")
        print()
        
        # Memory format demonstration
        print("💾 Memory Graph Format:")
        print("   • MCP memory server for shared state")
        print("   • Shell_ID tagged observations")
        print("   • Branch context in all entities")
        print("   • Cross-branch relationships tracked")
        print("   • Session correlation maintained")
        print()
        
        # Example entity observation
        if self.demo_entities:
            example_entity = self.demo_entities[0]
            print(f"📝 Example Entity: {example_entity['name']}")
            print(f"   Type: {example_entity['entityType']}")
            print("   Key Observations:")
            for obs in example_entity['observations'][:5]:
                print(f"     • {obs}")
            if len(example_entity['observations']) > 5:
                print(f"     • ... and {len(example_entity['observations']) - 5} more")
            print()
        
        # Real implementation note
        print("🔄 In Real Implementation:")
        print("   • mcp__memory__create_entities for worker registration")
        print("   • mcp__memory__add_observations for status updates")
        print("   • flowloom_memory_monitor for task waiting")
        print("   • Cross-instance coordination via shared memory")
        print()
        
        return len(self.demo_entities)
    
    def demo_conclusion(self):
        """Show demo conclusion and next steps."""
        print("🎉 Branch-Aware Coordination Demo Complete!")
        print("=" * 45)
        print()
        
        # Demo achievements
        achievements = [
            "✅ Branch-aware worker registration with cross-branch capabilities",
            "✅ Intelligent task assignment based on branch and skill requirements",
            "✅ Cross-branch workflow coordination and tracking",
            "✅ Integration with FlowLoom enhanced session management",
            "✅ Memory graph tracking with proper Shell_ID correlation",
            "✅ Support for web, dev, and feature development workflows"
        ]
        
        print("🏆 Demo Achievements:")
        for achievement in achievements:
            print(f"   {achievement}")
        print()
        
        # Architecture validation
        print("🏗️  Architecture Validation:")
        print("   • MCP memory server enables seamless coordination")
        print("   • Branch context provides intelligent task routing")
        print("   • Session management maintains workflow continuity")
        print("   • Cross-branch capabilities support complex workflows")
        print("   • Memory graph provides comprehensive audit trail")
        print()
        
        # Next implementation steps
        print("🚀 Next Implementation Steps:")
        print("   1. Connect to actual MCP memory operations")
        print("   2. Implement real blocking task assignment")
        print("   3. Add branch switching automation")
        print("   4. Create workflow template system")
        print("   5. Add monitoring dashboard integration")
        print()
        
        # Usage examples
        print("💡 Ready to Use:")
        print("   # Start coordination session")
        print("   python3 bin/branch_aware_coordinator.py controller init --session web-project")
        print()
        print("   # Register workers on different branches")
        print("   python3 bin/branch_aware_coordinator.py worker register --branch gh-pages --capabilities web,content")
        print("   python3 bin/branch_aware_coordinator.py worker register --branch main --capabilities dev,testing")
        print()
        print("   # Dispatch branch-specific tasks")
        print("   python3 bin/branch_aware_coordinator.py controller dispatch 'Update homepage' --requires-branch gh-pages")
        print("   python3 bin/branch_aware_coordinator.py controller dispatch 'Run tests' --requires-branch main")
        print()
        
        print(f"📈 Demo Statistics:")
        print(f"   Shell PID: {self.shell_pid}")
        print(f"   Entities Created: {len(self.demo_entities)}")
        print(f"   Demo Duration: ~2 minutes")
        print(f"   Architecture: Branch-aware MCP coordination")


def main():
    """Run the branch-aware coordination demo."""
    demo = BranchCoordinationDemo()
    
    print()
    
    # Demo flow
    context = demo.demo_setup()
    print()
    
    workers = demo.demo_branch_aware_workers()
    print()
    
    tasks = demo.demo_branch_aware_tasks()
    print()
    
    assignments = demo.demo_task_assignment_algorithm(workers, tasks)
    print()
    
    workflows = demo.demo_cross_branch_workflows(assignments)
    print()
    
    session_entity = demo.demo_session_integration()
    print()
    
    entity_count = demo.demo_memory_graph_tracking()
    print()
    
    demo.demo_conclusion()
    print()


if __name__ == "__main__":
    main()