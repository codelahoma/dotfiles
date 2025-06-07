"""
File Observer for memory.json monitoring.

Efficiently watches memory.json for changes using platform-specific
file system events when available, falling back to polling.
"""

import asyncio
import json
import hashlib
import time
from pathlib import Path
from typing import Optional, AsyncIterator, Dict, Any, Callable
from dataclasses import dataclass
from enum import Enum
import logging

try:
    from watchdog.observers import Observer
    from watchdog.events import FileSystemEventHandler
    WATCHDOG_AVAILABLE = True
except ImportError:
    WATCHDOG_AVAILABLE = False
    Observer = None
    FileSystemEventHandler = None

logger = logging.getLogger(__name__)


class ChangeType(Enum):
    CREATED = "created"
    MODIFIED = "modified"
    DELETED = "deleted"


@dataclass
class FileChange:
    """Represents a change to the memory file."""
    change_type: ChangeType
    timestamp: float
    file_path: Path
    new_content: Optional[str] = None
    checksum: Optional[str] = None


if WATCHDOG_AVAILABLE:
    class MemoryFileHandler(FileSystemEventHandler):
        """Watchdog event handler for memory.json changes."""
        
        def __init__(self, file_path: Path, callback: Callable[[FileChange], None]):
            self.file_path = file_path
            self.callback = callback
            self.last_checksum = None
            
        def on_modified(self, event):
            """Handle file modification events."""
            if event.is_directory:
                return
                
            if Path(event.src_path) == self.file_path:
                # Verify it's actually changed (not just timestamp)
                try:
                    content = self.file_path.read_text()
                    checksum = hashlib.md5(content.encode()).hexdigest()
                    
                    if checksum != self.last_checksum:
                        self.last_checksum = checksum
                        change = FileChange(
                            change_type=ChangeType.MODIFIED,
                            timestamp=time.time(),
                            file_path=self.file_path,
                            new_content=content,
                            checksum=checksum
                        )
                        self.callback(change)
                        
                except (FileNotFoundError, PermissionError) as e:
                    logger.warning(f"Could not read file {self.file_path}: {e}")
else:
    MemoryFileHandler = None


class FileObserver:
    """Observe memory.json file changes efficiently."""
    
    def __init__(self, file_path: str, poll_interval: float = 1.0):
        self.file_path = Path(file_path)
        self.poll_interval = poll_interval
        self.last_modified = 0.0
        self.last_checksum = None
        self.observer = None
        self.change_callbacks = []
        
    def add_change_callback(self, callback: Callable[[FileChange], None]):
        """Add callback for file changes."""
        self.change_callbacks.append(callback)
        
    def _notify_change(self, change: FileChange):
        """Notify all callbacks of change."""
        for callback in self.change_callbacks:
            try:
                callback(change)
            except Exception as e:
                logger.error(f"Error in change callback: {e}")
    
    def get_current_content(self) -> Optional[str]:
        """Get current file content."""
        try:
            return self.file_path.read_text()
        except (FileNotFoundError, PermissionError):
            return None
    
    def get_current_checksum(self) -> Optional[str]:
        """Get current file checksum."""
        content = self.get_current_content()
        if content is None:
            return None
        return hashlib.md5(content.encode()).hexdigest()
    
    def has_changed(self) -> bool:
        """Check if file has changed since last check."""
        if not self.file_path.exists():
            return False
            
        try:
            stat = self.file_path.stat()
            current_modified = stat.st_mtime
            
            if current_modified > self.last_modified:
                # Double-check with content hash
                current_checksum = self.get_current_checksum()
                if current_checksum != self.last_checksum:
                    self.last_modified = current_modified
                    self.last_checksum = current_checksum
                    return True
                    
        except (FileNotFoundError, PermissionError):
            pass
            
        return False
    
    def start_watching(self) -> None:
        """Start watching file with watchdog if available."""
        if not WATCHDOG_AVAILABLE:
            logger.info("Watchdog not available, falling back to polling")
            return
            
        if self.observer is not None:
            return  # Already watching
            
        try:
            if MemoryFileHandler is None:
                logger.warning("Watchdog handler not available")
                return
                
            self.observer = Observer()
            handler = MemoryFileHandler(self.file_path, self._notify_change)
            self.observer.schedule(handler, str(self.file_path.parent), recursive=False)
            self.observer.start()
            logger.info(f"Started watching {self.file_path} with watchdog")
            
        except Exception as e:
            logger.warning(f"Could not start watchdog observer: {e}")
            self.observer = None
    
    def stop_watching(self) -> None:
        """Stop watchdog observer."""
        if self.observer is not None:
            self.observer.stop()
            self.observer.join()
            self.observer = None
            logger.info("Stopped watchdog observer")
    
    async def watch_polling(self) -> AsyncIterator[FileChange]:
        """Watch file using polling method."""
        logger.info(f"Starting polling watch of {self.file_path} every {self.poll_interval}s")
        
        # Initialize state
        if self.file_path.exists():
            self.last_checksum = self.get_current_checksum()
            self.last_modified = self.file_path.stat().st_mtime
        
        while True:
            try:
                await asyncio.sleep(self.poll_interval)
                
                if self.has_changed():
                    content = self.get_current_content()
                    yield FileChange(
                        change_type=ChangeType.MODIFIED,
                        timestamp=time.time(),
                        file_path=self.file_path,
                        new_content=content,
                        checksum=self.last_checksum
                    )
                    
            except asyncio.CancelledError:
                logger.info("Polling watch cancelled")
                break
            except Exception as e:
                logger.error(f"Error in polling watch: {e}")
                await asyncio.sleep(self.poll_interval)
    
    async def watch_events(self) -> AsyncIterator[FileChange]:
        """Watch file using event queue."""
        self.start_watching()
        
        # Create queue for events
        event_queue = asyncio.Queue()
        
        def queue_change(change: FileChange):
            asyncio.create_task(event_queue.put(change))
        
        self.add_change_callback(queue_change)
        
        try:
            while True:
                change = await event_queue.get()
                yield change
                
        except asyncio.CancelledError:
            logger.info("Event watch cancelled")
        finally:
            self.stop_watching()
    
    async def watch(self, use_events: bool = True) -> AsyncIterator[FileChange]:
        """Watch file for changes using best available method."""
        if use_events and WATCHDOG_AVAILABLE:
            async for change in self.watch_events():
                yield change
        else:
            async for change in self.watch_polling():
                yield change
    
    def get_snapshot(self) -> Dict[str, Any]:
        """Get current file state without watching."""
        if not self.file_path.exists():
            return {
                'exists': False,
                'error': 'File not found'
            }
        
        try:
            content = self.get_current_content()
            stat = self.file_path.stat()
            
            return {
                'exists': True,
                'content': content,
                'size': stat.st_size,
                'modified': stat.st_mtime,
                'checksum': self.get_current_checksum()
            }
            
        except Exception as e:
            return {
                'exists': True,
                'error': str(e)
            }


class MemorySnapshot:
    """Snapshot of memory.json at a point in time."""
    
    def __init__(self, content: str, timestamp: float):
        self.content = content
        self.timestamp = timestamp
        self.checksum = hashlib.md5(content.encode()).hexdigest()
        self._parsed_data = None
    
    @property
    def data(self) -> Dict[str, Any]:
        """Parse and return memory data."""
        if self._parsed_data is None:
            try:
                # Handle JSON Lines format
                entities = []
                relations = []
                
                for line in self.content.strip().split('\n'):
                    if line.strip():
                        obj = json.loads(line)
                        if obj.get('type') == 'entity':
                            entities.append(obj)
                        elif obj.get('type') == 'relation':
                            relations.append(obj)
                
                self._parsed_data = {
                    'entities': entities,
                    'relations': relations,
                    'timestamp': self.timestamp,
                    'checksum': self.checksum
                }
                
            except json.JSONDecodeError as e:
                logger.error(f"Failed to parse memory data: {e}")
                self._parsed_data = {
                    'entities': [],
                    'relations': [],
                    'error': str(e),
                    'timestamp': self.timestamp
                }
        
        return self._parsed_data
    
    @property
    def entity_count(self) -> int:
        """Get number of entities."""
        return len(self.data.get('entities', []))
    
    @property
    def relation_count(self) -> int:
        """Get number of relations."""
        return len(self.data.get('relations', []))


# Example usage
if __name__ == "__main__":
    import asyncio
    
    async def monitor_memory():
        """Example monitoring function."""
        observer = FileObserver("memory.json", poll_interval=0.5)
        
        print(f"Monitoring {observer.file_path}")
        print("Watching for changes...")
        
        async for change in observer.watch():
            print(f"Change detected: {change.change_type} at {change.timestamp}")
            
            if change.new_content:
                snapshot = MemorySnapshot(change.new_content, change.timestamp)
                print(f"  Entities: {snapshot.entity_count}")
                print(f"  Relations: {snapshot.relation_count}")
                print(f"  Checksum: {snapshot.checksum[:8]}...")
    
    # Run example
    asyncio.run(monitor_memory())