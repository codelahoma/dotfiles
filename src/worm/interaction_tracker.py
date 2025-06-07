"""
WORM Interaction Tracker

Tracks user interactions and file changes to determine when auto-commit
should be triggered, implementing the core WORM governance workflow.
"""

import json
import time
import uuid
from datetime import datetime
from pathlib import Path
from typing import List, Optional, Dict, Any


class WORMInteractionTracker:
    """
    Tracks user interactions and file changes for WORM auto-commit system.
    
    Core WORM principle: Every code change must be preceded by documented
    conversation and immediately committed with complete reasoning context.
    """
    
    def __init__(self, project_root: str = "."):
        self.project_root = Path(project_root)
        self.interaction_id: Optional[str] = None
        self.files_changed: List[str] = []
        self.interaction_reasoning: str = ""
        self.start_time: Optional[datetime] = None
        self.memory_snapshot: Optional[Dict[str, Any]] = None
        self.is_active: bool = False
    
    @property
    def current_interaction(self) -> Optional[Dict[str, Any]]:
        """Get current interaction data if active."""
        if self.is_active:
            return self.get_interaction_summary()
        return None
        
    def start_interaction(self, user_request: str) -> str:
        """
        Begin tracking a new user interaction.
        
        Args:
            user_request: The user's request that initiated this interaction
            
        Returns:
            interaction_id: Unique identifier for this interaction
        """
        self.interaction_id = str(uuid.uuid4())[:8]
        self.files_changed = []
        self.interaction_reasoning = user_request
        self.start_time = datetime.now()
        self.is_active = True
        self.memory_snapshot = None
        
        # Log interaction start
        self._log_interaction_event("INTERACTION_START", {
            "interaction_id": self.interaction_id,
            "user_request": user_request,
            "timestamp": self.start_time.isoformat()
        })
        
        return self.interaction_id
    
    def track_file_change(self, file_path: str) -> bool:
        """
        Track file changes during interaction.
        
        Args:
            file_path: Path to file that was changed
            
        Returns:
            bool: True if file change was tracked, False if excluded
        """
        if not self.is_active:
            return False
            
        # Convert to relative path for consistency
        try:
            relative_path = str(Path(file_path).relative_to(self.project_root))
        except ValueError:
            # File outside project root
            relative_path = file_path
        
        # Exclude memory.json changes to avoid recursive auto-commits
        if relative_path.endswith('memory.json'):
            return False
        
        # Exclude certain file patterns that shouldn't trigger commits
        excluded_patterns = [
            '.git/',
            '__pycache__/',
            '.pytest_cache/',
            'node_modules/',
            '.DS_Store'
        ]
        
        for pattern in excluded_patterns:
            if pattern in relative_path:
                return False
        
        if relative_path not in self.files_changed:
            self.files_changed.append(relative_path)
            
            self._log_interaction_event("FILE_CHANGE", {
                "interaction_id": self.interaction_id,
                "file_path": relative_path,
                "timestamp": datetime.now().isoformat()
            })
            
        return True
    
    def should_auto_commit(self) -> bool:
        """
        Determine if interaction should trigger auto-commit.
        
        Returns:
            bool: True if auto-commit should be triggered
        """
        return self.is_active and len(self.files_changed) > 0
    
    def get_interaction_summary(self) -> Dict[str, Any]:
        """
        Get summary of current interaction for commit message.
        
        Returns:
            dict: Summary of interaction details
        """
        if not self.is_active:
            return {}
            
        duration = None
        if self.start_time:
            duration = (datetime.now() - self.start_time).total_seconds()
        
        return {
            "interaction_id": self.interaction_id,
            "user_request": self.interaction_reasoning,
            "files_changed": self.files_changed,
            "file_count": len(self.files_changed),
            "start_time": self.start_time.isoformat() if self.start_time else None,
            "duration_seconds": duration
        }
    
    def complete_interaction(self, interaction_id: str = None) -> Dict[str, Any]:
        """
        Finalize interaction and return result data.
        
        Args:
            interaction_id: Optional interaction ID to complete (uses current if None)
        
        Returns:
            dict: Interaction completion result
        """
        if not self.is_active:
            return {
                "success": False,
                "message": "No active interaction to complete",
                "should_auto_commit": False
            }
        
        # Check if specific interaction ID matches current
        if interaction_id and interaction_id != self.interaction_id:
            return {
                "success": False,
                "message": f"Interaction ID mismatch: {interaction_id} != {self.interaction_id}",
                "should_auto_commit": False
            }
            
        # Create completion result
        result = {
            "success": True,
            "interaction_id": self.interaction_id,
            "user_request": self.interaction_reasoning,
            "files_changed": self.files_changed.copy(),
            "file_count": len(self.files_changed),
            "should_auto_commit": self.should_auto_commit(),
            "start_time": self.start_time.isoformat() if self.start_time else None,
            "completion_time": datetime.now().isoformat()
        }
        
        if result["should_auto_commit"]:
            self._log_interaction_event("INTERACTION_COMPLETE_COMMIT", {
                "interaction_id": self.interaction_id,
                "files_changed": self.files_changed,
                "auto_commit": True
            })
        else:
            self._log_interaction_event("INTERACTION_COMPLETE_NO_COMMIT", {
                "interaction_id": self.interaction_id,
                "auto_commit": False,
                "reason": "no_files_changed"
            })
        
        # Reset tracker state
        self._reset_state()
        
        return result
    
    def cancel_interaction(self, reason: str = "cancelled"):
        """
        Cancel current interaction without triggering auto-commit.
        
        Args:
            reason: Reason for cancellation
        """
        if self.is_active:
            self._log_interaction_event("INTERACTION_CANCELLED", {
                "interaction_id": self.interaction_id,
                "reason": reason
            })
            
        self._reset_state()
    
    def set_memory_snapshot(self, snapshot: Dict[str, Any]):
        """
        Set memory snapshot for current interaction.
        
        Args:
            snapshot: Memory snapshot data
        """
        if self.is_active:
            self.memory_snapshot = snapshot
    
    def _reset_state(self):
        """Reset tracker to initial state."""
        self.interaction_id = None
        self.files_changed = []
        self.interaction_reasoning = ""
        self.start_time = None
        self.memory_snapshot = None
        self.is_active = False
    
    def _log_interaction_event(self, event_type: str, data: Dict[str, Any]):
        """
        Log interaction events for debugging and audit trail.
        
        Args:
            event_type: Type of event being logged
            data: Event data
        """
        log_entry = {
            "timestamp": datetime.now().isoformat(),
            "event_type": event_type,
            "data": data
        }
        
        # For now, we'll just print for debugging
        # In production, this would go to a proper logging system
        print(f"WORM: {event_type} - {data.get('interaction_id', 'unknown')}")


class WORMInteractionContext:
    """
    Context manager for WORM interactions to ensure proper lifecycle management.
    """
    
    def __init__(self, tracker: WORMInteractionTracker, user_request: str):
        self.tracker = tracker
        self.user_request = user_request
        self.interaction_id = None
        
    def __enter__(self):
        self.interaction_id = self.tracker.start_interaction(self.user_request)
        return self
        
    def __exit__(self, exc_type, exc_val, exc_tb):
        if exc_type is not None:
            # Exception occurred, cancel interaction
            self.tracker.cancel_interaction(f"exception: {exc_type.__name__}")
        else:
            # Normal completion, check for auto-commit
            commit_data = self.tracker.complete_interaction()
            if commit_data:
                # Auto-commit would be triggered here
                pass
        
        return False  # Don't suppress exceptions