"""
WORM Memory Snapshot

Captures memory.json state at interaction completion to preserve
reasoning context in commits, creating complete governance audit trails.
"""

import json
import os
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Any, Optional


class WORMMemorySnapshot:
    """
    Captures and formats memory snapshots for WORM commit context.
    
    Provides complete reasoning preservation by including relevant memory
    entries that document the decision process behind code changes.
    """
    
    def __init__(self, project_root: str = ".", memory_file: str = "memory.json"):
        self.project_root = Path(project_root)
        self.memory_file_path = self.project_root / memory_file
        
    def capture_interaction_snapshot(self, interaction_id: str, 
                                   user_request: str,
                                   timeframe_minutes: int = 30) -> Dict[str, Any]:
        """
        Capture memory snapshot relevant to current interaction.
        
        Args:
            interaction_id: Unique identifier for current interaction
            user_request: Original user request for context
            timeframe_minutes: How many minutes back to look for relevant entries
            
        Returns:
            dict: Formatted memory snapshot for commit inclusion
        """
        try:
            memory_data = self._load_memory_data()
            
            if not memory_data:
                return self._create_fallback_snapshot(interaction_id, user_request)
            
            # Filter for recent relevant entries
            relevant_entries = self._filter_interaction_memory(
                memory_data, interaction_id, user_request, timeframe_minutes
            )
            
            # Format for commit message
            snapshot = self._format_memory_for_commit(
                relevant_entries, interaction_id, user_request
            )
            
            return snapshot
            
        except Exception as e:
            return self._create_error_snapshot(interaction_id, user_request, str(e))
    
    def _load_memory_data(self) -> Optional[Dict[str, Any]]:
        """Load memory.json data safely."""
        try:
            if not self.memory_file_path.exists():
                return None
                
            with open(self.memory_file_path, 'r', encoding='utf-8') as f:
                return json.load(f)
                
        except (json.JSONDecodeError, IOError) as e:
            print(f"WORM: Warning - Could not load memory data: {e}")
            return None
    
    def _filter_interaction_memory(self, memory_data: Dict[str, Any], 
                                 interaction_id: str, user_request: str,
                                 timeframe_minutes: int) -> List[Dict[str, Any]]:
        """
        Filter memory entries relevant to current interaction.
        
        Args:
            memory_data: Complete memory data
            interaction_id: Current interaction ID
            user_request: User's request for context matching
            timeframe_minutes: Time window for relevance
            
        Returns:
            list: Relevant memory entries
        """
        relevant_entries = []
        cutoff_time = datetime.now() - timedelta(minutes=timeframe_minutes)
        
        # Look for auto-track entries
        if 'entities' in memory_data:
            for entity in memory_data['entities']:
                if entity.get('entityType') == 'session_state':
                    # Check for FlowLoom Session Auto-Track Status
                    if 'Auto-Track' in entity.get('name', ''):
                        recent_observations = self._get_recent_observations(
                            entity.get('observations', []), cutoff_time
                        )
                        
                        if recent_observations:
                            relevant_entries.append({
                                'type': 'auto_track',
                                'entity_name': entity.get('name'),
                                'observations': recent_observations
                            })
        
        # Look for decision entities related to the request
        if 'entities' in memory_data:
            for entity in memory_data['entities']:
                entity_type = entity.get('entityType', '')
                entity_name = entity.get('name', '')
                
                # Include strategic concepts, implementations, etc.
                if entity_type in ['strategic_concept', 'technical_architecture', 
                                 'business_strategy', 'implementation']:
                    # Check if entity is contextually relevant
                    if self._is_contextually_relevant(entity, user_request):
                        relevant_entries.append({
                            'type': 'context_entity',
                            'entity_type': entity_type,
                            'entity_name': entity_name,
                            'observations': entity.get('observations', [])[:3]  # Recent observations
                        })
        
        return relevant_entries
    
    def _get_recent_observations(self, observations: List[str], 
                               cutoff_time: datetime) -> List[str]:
        """Extract recent observations based on timestamp patterns."""
        recent = []
        
        for obs in observations:
            # Look for timestamp patterns in observations
            if 'AUTO:' in obs and '2025-' in obs:
                # Extract timestamp and check if recent
                try:
                    # Parse timestamp from AUTO entries
                    if 'AUTO: 2025-' in obs:
                        timestamp_part = obs.split('AUTO: ')[1].split(' |')[0]
                        obs_time = datetime.fromisoformat(timestamp_part.replace(' ', 'T'))
                        
                        if obs_time >= cutoff_time:
                            recent.append(obs)
                except (ValueError, IndexError):
                    # If timestamp parsing fails, include anyway if recent
                    recent.append(obs)
            elif len(recent) < 5:  # Include recent non-timestamped entries
                recent.append(obs)
        
        return recent[-5:]  # Last 5 relevant observations
    
    def _is_contextually_relevant(self, entity: Dict[str, Any], 
                                user_request: str) -> bool:
        """
        Check if entity is contextually relevant to user request.
        
        Args:
            entity: Memory entity to check
            user_request: User's request
            
        Returns:
            bool: True if entity seems relevant
        """
        entity_name = entity.get('name', '').lower()
        entity_type = entity.get('entityType', '').lower()
        user_request_lower = user_request.lower()
        
        # Keywords that suggest relevance
        relevant_keywords = [
            'worm', 'auto-commit', 'governance', 'implementation',
            'memory', 'tracking', 'commit', 'development'
        ]
        
        # Check if any relevant keywords appear
        for keyword in relevant_keywords:
            if (keyword in entity_name or 
                keyword in entity_type or 
                keyword in user_request_lower):
                return True
        
        # Check entity observations for relevance
        observations = entity.get('observations', [])
        for obs in observations[:3]:  # Check first few observations
            obs_lower = obs.lower()
            for keyword in relevant_keywords:
                if keyword in obs_lower:
                    return True
        
        return False
    
    def _format_memory_for_commit(self, entries: List[Dict[str, Any]], 
                                interaction_id: str, user_request: str) -> Dict[str, Any]:
        """
        Format memory data for inclusion in commit message.
        
        Args:
            entries: Filtered memory entries
            interaction_id: Current interaction ID
            user_request: User's request
            
        Returns:
            dict: Formatted snapshot for commit
        """
        snapshot = {
            'interaction_id': interaction_id,
            'user_request': user_request,
            'timestamp': datetime.now().isoformat(),
            'reasoning_context': [],
            'decision_trail': [],
            'summary': ''
        }
        
        # Process auto-track entries for reasoning context
        for entry in entries:
            if entry['type'] == 'auto_track':
                for obs in entry['observations']:
                    if 'AUTO:' in obs:
                        # Extract key information from auto-track entries
                        reasoning_item = self._extract_reasoning_from_auto_track(obs)
                        if reasoning_item:
                            snapshot['reasoning_context'].append(reasoning_item)
        
        # Process context entities for decision trail
        for entry in entries:
            if entry['type'] == 'context_entity':
                decision_item = {
                    'entity': entry['entity_name'],
                    'type': entry['entity_type'],
                    'key_points': entry['observations'][:2]  # Top observations
                }
                snapshot['decision_trail'].append(decision_item)
        
        # Create summary
        snapshot['summary'] = self._create_memory_summary(snapshot)
        
        return snapshot
    
    def _extract_reasoning_from_auto_track(self, auto_track_entry: str) -> Optional[Dict[str, str]]:
        """Extract structured reasoning from auto-track entry."""
        try:
            # Parse AUTO track format: "AUTO: timestamp | Request: ... | Tools: ... | Decisions: ... | Next: ..."
            if 'Request:' in auto_track_entry and 'Decisions:' in auto_track_entry:
                parts = auto_track_entry.split(' | ')
                
                reasoning = {}
                for part in parts:
                    if part.startswith('Request: '):
                        reasoning['request'] = part[9:]
                    elif part.startswith('Decisions: '):
                        reasoning['decisions'] = part[11:]
                    elif part.startswith('Next: '):
                        reasoning['next_steps'] = part[6:]
                
                if reasoning:
                    return reasoning
                    
        except Exception:
            pass
            
        return None
    
    def _create_memory_summary(self, snapshot: Dict[str, Any]) -> str:
        """Create concise summary of memory context."""
        reasoning_count = len(snapshot['reasoning_context'])
        decision_count = len(snapshot['decision_trail'])
        
        summary_parts = []
        
        if reasoning_count > 0:
            summary_parts.append(f"{reasoning_count} interaction steps documented")
        
        if decision_count > 0:
            summary_parts.append(f"{decision_count} decision contexts preserved")
        
        if not summary_parts:
            summary_parts.append("Complete interaction context captured")
        
        return "; ".join(summary_parts)
    
    def _create_fallback_snapshot(self, interaction_id: str, user_request: str) -> Dict[str, Any]:
        """Create fallback snapshot when memory data unavailable."""
        return {
            'interaction_id': interaction_id,
            'user_request': user_request,
            'timestamp': datetime.now().isoformat(),
            'reasoning_context': [],
            'decision_trail': [],
            'summary': 'No memory context available - interaction documented without historical reasoning'
        }
    
    def _create_error_snapshot(self, interaction_id: str, user_request: str, 
                             error: str) -> Dict[str, Any]:
        """Create error snapshot when memory capture fails."""
        return {
            'interaction_id': interaction_id,
            'user_request': user_request,
            'timestamp': datetime.now().isoformat(),
            'reasoning_context': [],
            'decision_trail': [],
            'summary': f'Memory capture failed: {error}',
            'error': error
        }
    
    def format_for_commit_message(self, snapshot: Dict[str, Any]) -> str:
        """
        Format memory snapshot for inclusion in commit message.
        
        Args:
            snapshot: Memory snapshot data
            
        Returns:
            str: Formatted text for commit message
        """
        lines = []
        
        lines.append("WORM Memory Context:")
        lines.append(f"Interaction: {snapshot.get('interaction_id', 'unknown')}")
        lines.append(f"Request: {snapshot.get('user_request', 'unknown')}")
        lines.append(f"Summary: {snapshot.get('summary', 'no summary')}")
        
        reasoning = snapshot.get('reasoning_context', [])
        if reasoning:
            lines.append("")
            lines.append("Reasoning Trail:")
            for i, step in enumerate(reasoning[:3], 1):  # Top 3 steps
                if isinstance(step, dict):
                    decision = step.get('decisions', 'no decision recorded')
                    lines.append(f"  {i}. {decision}")
        
        decisions = snapshot.get('decision_trail', [])
        if decisions:
            lines.append("")
            lines.append("Decision Context:")
            for decision in decisions[:2]:  # Top 2 contexts
                entity_name = decision.get('entity', 'unknown')
                lines.append(f"  - {entity_name}")
        
        return "\n".join(lines)