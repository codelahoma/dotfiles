"""
Main Memory Monitor module providing unified interface for file watching,
querying, and real-time monitoring of FlowLoom's memory graph.
"""

import asyncio
import json
import time
from pathlib import Path
from typing import Dict, Any, Optional, List, Union, AsyncIterator, Callable
from dataclasses import dataclass
import logging

from .file_observer import FileObserver, FileChange, ChangeType, MemorySnapshot
from .query import MemoryQuery, QueryResult, QueryError

logger = logging.getLogger(__name__)


@dataclass
class MonitorEvent:
    """Event emitted by memory monitor."""
    event_type: str  # 'entity_created', 'entity_updated', 'entity_deleted', 'query_result'
    timestamp: float
    data: Dict[str, Any]
    change_info: Optional[FileChange] = None


class MemoryMonitor:
    """
    High-performance memory monitor with optimized file watching, querying,
    and event streaming for FlowLoom's memory graph.
    """
    
    def __init__(self, memory_file_path: Union[str, Path], 
                 poll_interval: float = 0.5,  # Reduced for better responsiveness
                 auto_index: bool = True,
                 enable_caching: bool = True):
        self.memory_file_path = Path(memory_file_path)
        self.poll_interval = poll_interval
        self.auto_index = auto_index
        self.enable_caching = enable_caching
        
        # Components
        self.file_observer = FileObserver(str(memory_file_path), poll_interval)
        self.query_interface = MemoryQuery(memory_file_path)
        
        # State
        self.is_watching = False
        self.last_snapshot = None
        self.event_handlers = []
        
        # Performance optimizations
        self._entity_cache = {} if enable_caching else None
        self._cache_ttl = 60  # seconds
        self._last_cache_update = 0
        self._file_hash = ""
        self._batch_events = []  # Batch events for performance
        self._batch_size = 10
        self._last_batch_process = time.time()
        
        # Statistics
        self.stats = {
            'events_processed': 0,
            'queries_executed': 0,
            'entities_tracked': 0,
            'cache_hits': 0,
            'cache_misses': 0,
            'batch_operations': 0,
            'avg_query_time_ms': 0.0,
            'start_time': time.time()
        }
    
    def add_event_handler(self, handler: Callable[[MonitorEvent], None]):
        """Add event handler for monitor events."""
        self.event_handlers.append(handler)
    
    def remove_event_handler(self, handler: Callable[[MonitorEvent], None]):
        """Remove event handler."""
        if handler in self.event_handlers:
            self.event_handlers.remove(handler)
    
    def _emit_event(self, event: MonitorEvent):
        """Emit event to all handlers with batching optimization."""
        self.stats['events_processed'] += 1
        
        # Add to batch for performance
        self._batch_events.append(event)
        
        # Process batch if size threshold reached or time elapsed
        current_time = time.time()
        if (len(self._batch_events) >= self._batch_size or 
            current_time - self._last_batch_process > 1.0):  # 1 second max delay
            self._process_event_batch()
    
    def query(self, sql: str) -> QueryResult:
        """Execute SQL query against memory data with performance optimization."""
        start_time = time.time()
        
        # Check cache for frequent queries
        if self._entity_cache is not None:
            cache_key = f"query:{hash(sql)}"
            cached_result = self._get_cached_result(cache_key)
            if cached_result is not None:
                self.stats['cache_hits'] += 1
                return cached_result
            self.stats['cache_misses'] += 1
        
        result = self.query_interface.query(sql)
        
        # Update performance stats
        execution_time = (time.time() - start_time) * 1000
        self.stats['queries_executed'] += 1
        
        # Update average query time
        if self.stats['queries_executed'] > 0:
            total_time = self.stats['avg_query_time_ms'] * (self.stats['queries_executed'] - 1)
            self.stats['avg_query_time_ms'] = (total_time + execution_time) / self.stats['queries_executed']
        
        # Cache result for reuse
        if self._entity_cache is not None and execution_time > 10:  # Cache slow queries
            self._cache_result(cache_key, result)
        
        return result
    
    def explain(self, sql: str) -> Dict[str, Any]:
        """Explain query execution plan."""
        return self.query_interface.explain(sql)
    
    def get_current_snapshot(self) -> MemorySnapshot:
        """Get current memory snapshot."""
        content = self.memory_file_path.read_text()
        stat = self.memory_file_path.stat()
        return MemorySnapshot(content=content, timestamp=stat.st_mtime)
    
    def reload(self):
        """Force reload of memory data."""
        self.query_interface.reload()
        self.last_snapshot = self.get_current_snapshot()
        
        # Update stats
        if self.last_snapshot:
            self.stats['entities_tracked'] = self.last_snapshot.entity_count
    
    async def start_monitoring(self, emit_initial_state: bool = True):
        """Start real-time monitoring of memory file."""
        if self.is_watching:
            logger.warning("Monitor is already watching")
            return
        
        self.is_watching = True
        logger.info(f"Starting memory monitor for {self.memory_file_path}")
        
        # Get initial state
        if emit_initial_state:
            try:
                self.reload()
                self._emit_event(MonitorEvent(
                    event_type='monitor_started',
                    timestamp=time.time(),
                    data={
                        'file_path': str(self.memory_file_path),
                        'entity_count': self.stats['entities_tracked'],
                        'snapshot_timestamp': self.last_snapshot.timestamp if self.last_snapshot else None
                    }
                ))
            except Exception as e:
                logger.error(f"Failed to load initial state: {e}")
        
        # Start file watching
        try:
            async for change in self.file_observer.watch():
                if not self.is_watching:
                    break
                
                await self._process_file_change(change)
                
        except asyncio.CancelledError:
            logger.info("Memory monitoring cancelled")
        except Exception as e:
            logger.error(f"Error in memory monitoring: {e}")
        finally:
            self.is_watching = False
    
    async def _process_file_change(self, change: FileChange):
        """Process file change and emit appropriate events."""
        logger.debug(f"Processing file change: {change.change_type}")
        
        try:
            # Reload data
            old_snapshot = self.last_snapshot
            self.reload()
            new_snapshot = self.last_snapshot
            
            # Detect what changed
            changes = self._detect_changes(old_snapshot, new_snapshot)
            
            # Emit change events
            for change_event in changes:
                self._emit_event(change_event)
            
            # Emit file change event
            self._emit_event(MonitorEvent(
                event_type='file_changed',
                timestamp=change.timestamp,
                data={
                    'change_type': change.change_type.value,
                    'entity_count': new_snapshot.entity_count if new_snapshot else 0,
                    'changes_detected': len(changes)
                },
                change_info=change
            ))
            
        except Exception as e:
            logger.error(f"Error processing file change: {e}")
            self._emit_event(MonitorEvent(
                event_type='error',
                timestamp=time.time(),
                data={'error': str(e), 'change_type': change.change_type.value}
            ))
    
    def _detect_changes(self, old_snapshot: Optional[MemorySnapshot], 
                       new_snapshot: Optional[MemorySnapshot]) -> List[MonitorEvent]:
        """Detect changes between snapshots."""
        events = []
        
        if not old_snapshot or not new_snapshot:
            return events
        
        old_entities = {e.get('name'): e for e in old_snapshot.data.get('entities', [])}
        new_entities = {e.get('name'): e for e in new_snapshot.data.get('entities', [])}
        
        # Detect new entities
        for name, entity in new_entities.items():
            if name not in old_entities:
                events.append(MonitorEvent(
                    event_type='entity_created',
                    timestamp=time.time(),
                    data={'entity': entity}
                ))
        
        # Detect updated entities
        for name, entity in new_entities.items():
            if name in old_entities:
                old_entity = old_entities[name]
                if entity != old_entity:
                    events.append(MonitorEvent(
                        event_type='entity_updated',
                        timestamp=time.time(),
                        data={
                            'entity': entity,
                            'old_entity': old_entity,
                            'changes': self._calculate_entity_diff(old_entity, entity)
                        }
                    ))
        
        # Detect deleted entities
        for name, entity in old_entities.items():
            if name not in new_entities:
                events.append(MonitorEvent(
                    event_type='entity_deleted',
                    timestamp=time.time(),
                    data={'entity': entity}
                ))
        
        return events
    
    def _calculate_entity_diff(self, old_entity: Dict[str, Any], 
                              new_entity: Dict[str, Any]) -> Dict[str, Any]:
        """Calculate differences between entity versions."""
        changes = {}
        
        # Check observations changes
        old_obs = set(old_entity.get('observations', []))
        new_obs = set(new_entity.get('observations', []))
        
        added_obs = new_obs - old_obs
        removed_obs = old_obs - new_obs
        
        if added_obs or removed_obs:
            changes['observations'] = {
                'added': list(added_obs),
                'removed': list(removed_obs)
            }
        
        # Check other field changes
        for key in set(old_entity.keys()) | set(new_entity.keys()):
            if key == 'observations':
                continue  # Already handled above
            
            old_value = old_entity.get(key)
            new_value = new_entity.get(key)
            
            if old_value != new_value:
                changes[key] = {
                    'old': old_value,
                    'new': new_value
                }
        
        return changes
    
    def stop_monitoring(self):
        """Stop monitoring."""
        # Process any remaining batched events
        self._process_event_batch()
        
        self.is_watching = False
        logger.info("Memory monitoring stopped")
    
    def _process_event_batch(self):
        """Process batched events for better performance."""
        if not self._batch_events:
            return
        
        self.stats['batch_operations'] += 1
        
        for event in self._batch_events:
            for handler in self.event_handlers:
                try:
                    handler(event)
                except Exception as e:
                    logger.error(f"Error in event handler: {e}")
        
        self._batch_events.clear()
        self._last_batch_process = time.time()
    
    def _get_cached_result(self, cache_key: str):
        """Get cached query result with TTL check."""
        if not self._entity_cache:
            return None
        
        if cache_key in self._entity_cache:
            result, timestamp = self._entity_cache[cache_key]
            if time.time() - timestamp < self._cache_ttl:
                return result
            else:
                del self._entity_cache[cache_key]
        
        return None
    
    def _cache_result(self, cache_key: str, result):
        """Cache query result with timestamp."""
        if self._entity_cache is not None:
            self._entity_cache[cache_key] = (result, time.time())
            
            # Prevent cache from growing too large
            if len(self._entity_cache) > 100:
                self._cleanup_cache()
    
    def _cleanup_cache(self):
        """Clean up old cache entries."""
        current_time = time.time()
        expired_keys = []
        
        for key, (_, timestamp) in self._entity_cache.items():
            if current_time - timestamp > self._cache_ttl:
                expired_keys.append(key)
        
        for key in expired_keys:
            del self._entity_cache[key]
    
    def _needs_cache_refresh(self) -> bool:
        """Check if cache needs refresh based on file changes."""
        if not self.memory_file_path.exists():
            return False
        
        try:
            import hashlib
            content = self.memory_file_path.read_text()
            current_hash = hashlib.md5(content.encode()).hexdigest()
            
            if current_hash != self._file_hash:
                self._file_hash = current_hash
                return True
        except Exception:
            pass
        
        return False
    
    async def wait_for_condition(self, sql: str, timeout: Optional[float] = None) -> Optional[QueryResult]:
        """
        Wait for a query condition to be met.
        
        Args:
            sql: SQL query to check
            timeout: Maximum time to wait (None for no timeout)
            
        Returns:
            QueryResult if condition met, None if timeout
        """
        start_time = time.time()
        
        # Check initial condition
        try:
            result = self.query(sql)
            if not result.is_empty():
                return result
        except QueryError:
            pass  # Ignore initial query errors
        
        # Set up condition waiter
        condition_met = asyncio.Event()
        result_holder = [None]
        
        def check_condition(event: MonitorEvent):
            """Check if condition is met on each change."""
            if event.event_type in ('entity_created', 'entity_updated', 'file_changed'):
                try:
                    result = self.query(sql)
                    if not result.is_empty():
                        result_holder[0] = result
                        condition_met.set()
                except QueryError:
                    pass  # Ignore query errors during waiting
        
        # Add temporary handler
        self.add_event_handler(check_condition)
        
        try:
            # Start monitoring if not already started
            monitoring_task = None
            if not self.is_watching:
                monitoring_task = asyncio.create_task(self.start_monitoring(emit_initial_state=False))
            
            # Wait for condition or timeout
            if timeout:
                try:
                    await asyncio.wait_for(condition_met.wait(), timeout=timeout)
                except asyncio.TimeoutError:
                    return None
            else:
                await condition_met.wait()
            
            return result_holder[0]
            
        finally:
            # Clean up
            self.remove_event_handler(check_condition)
            if monitoring_task and not monitoring_task.done():
                monitoring_task.cancel()
                try:
                    await monitoring_task
                except asyncio.CancelledError:
                    pass
    
    def get_monitor_stats(self) -> Dict[str, Any]:
        """Get monitoring statistics."""
        uptime = time.time() - self.stats['start_time']
        
        return {
            **self.stats,
            'uptime_seconds': uptime,
            'is_watching': self.is_watching,
            'file_path': str(self.memory_file_path),
            'file_exists': self.memory_file_path.exists(),
            'handlers_count': len(self.event_handlers),
            'memory_stats': self.query_interface.get_stats()
        }
    
    def get_recent_events(self, limit: int = 10) -> List[MonitorEvent]:
        """Get recent monitor events (if event history is enabled)."""
        # This would require storing events in a ring buffer
        # For now, return empty list
        return []
    
    async def stream_events(self, event_filter: Optional[Callable[[MonitorEvent], bool]] = None) -> AsyncIterator[MonitorEvent]:
        """
        Stream monitor events in real-time.
        
        Args:
            event_filter: Optional filter function for events
            
        Yields:
            MonitorEvent: Real-time events from monitor
        """
        event_queue = asyncio.Queue()
        
        def queue_event(event: MonitorEvent):
            """Queue events for streaming."""
            if event_filter is None or event_filter(event):
                asyncio.create_task(event_queue.put(event))
        
        # Add event handler
        self.add_event_handler(queue_event)
        
        try:
            # Start monitoring if needed
            monitoring_task = None
            if not self.is_watching:
                monitoring_task = asyncio.create_task(self.start_monitoring())
            
            # Stream events
            while True:
                event = await event_queue.get()
                yield event
                
        except asyncio.CancelledError:
            logger.info("Event streaming cancelled")
        finally:
            # Clean up
            self.remove_event_handler(queue_event)
            if monitoring_task and not monitoring_task.done():
                monitoring_task.cancel()
                try:
                    await monitoring_task
                except asyncio.CancelledError:
                    pass


# Convenience functions for common use cases
async def wait_for_entity(memory_file: Union[str, Path], entity_name: str, 
                         timeout: Optional[float] = None) -> Optional[Dict[str, Any]]:
    """Wait for specific entity to appear."""
    monitor = MemoryMonitor(memory_file)
    sql = f"SELECT * FROM entities WHERE name = '{entity_name}'"
    
    result = await monitor.wait_for_condition(sql, timeout)
    if result and not result.is_empty():
        return result.fetch_first(1)[0]
    return None


async def wait_for_task_assignment(memory_file: Union[str, Path], worker_id: str,
                                  timeout: Optional[float] = None) -> Optional[Dict[str, Any]]:
    """Wait for task to be assigned to worker."""
    monitor = MemoryMonitor(memory_file)
    sql = f"SELECT * FROM entities WHERE entityType = 'CoordinationTask' AND observations CONTAINS 'assigned_to:{worker_id}' AND observations CONTAINS 'status:assigned'"
    
    result = await monitor.wait_for_condition(sql, timeout)
    if result and not result.is_empty():
        return result.fetch_first(1)[0]
    return None


# Example usage
if __name__ == "__main__":
    import asyncio
    
    async def example_monitoring():
        """Example monitoring usage."""
        monitor = MemoryMonitor("memory.json")
        
        # Add event handler
        def handle_events(event: MonitorEvent):
            print(f"Event: {event.event_type} at {event.timestamp}")
            if event.event_type == 'entity_created':
                entity = event.data['entity']
                print(f"  New entity: {entity.get('name', 'Unknown')} ({entity.get('entityType', 'Unknown')})")
        
        monitor.add_event_handler(handle_events)
        
        # Start monitoring
        print("Starting memory monitor...")
        
        # Monitor for 30 seconds
        try:
            await asyncio.wait_for(monitor.start_monitoring(), timeout=30.0)
        except asyncio.TimeoutError:
            print("Monitoring timeout reached")
        finally:
            monitor.stop_monitoring()
            
            # Print stats
            stats = monitor.get_monitor_stats()
            print(f"Processed {stats['events_processed']} events")
            print(f"Executed {stats['queries_executed']} queries")
    
    # Run example
    asyncio.run(example_monitoring())