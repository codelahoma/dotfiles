"""
High-level Query Interface for Memory Monitor.

Provides a simple API for executing SQL-like queries against memory.json data.
"""

import json
import time
from pathlib import Path
from typing import Dict, Any, Optional, List, Union
import logging

from .parser import Lexer, Parser, LexerError, ParseError
from .executor import QueryExecutor, QueryResult as ExecutorQueryResult, ExecutionStats
from .file_observer import FileObserver, MemorySnapshot

logger = logging.getLogger(__name__)


class QueryError(Exception):
    """Raised when query execution fails."""
    pass


class MemoryQuery:
    """High-level interface for querying memory data."""
    
    def __init__(self, memory_file_path: Union[str, Path]):
        self.memory_file_path = Path(memory_file_path)
        self.memory_data = None
        self.last_loaded = 0
        self.parser = Parser()
        self.executor = None
        self._load_memory_data()
    
    def _load_memory_data(self, force_reload: bool = False):
        """Load memory.json data."""
        if not self.memory_file_path.exists():
            raise QueryError(f"Memory file not found: {self.memory_file_path}")
        
        # Check if reload is needed
        stat = self.memory_file_path.stat()
        if not force_reload and stat.st_mtime <= self.last_loaded:
            return  # Already up to date
        
        try:
            content = self.memory_file_path.read_text()
            self.memory_data = self._parse_memory_content(content)
            self.last_loaded = stat.st_mtime
            
            # Create new executor with fresh data
            self.executor = QueryExecutor(self.memory_data)
            
            logger.info(f"Loaded {len(self.memory_data['entities'])} entities, "
                       f"{len(self.memory_data['relations'])} relations")
            
        except Exception as e:
            raise QueryError(f"Failed to load memory data: {e}")
    
    def _parse_memory_content(self, content: str) -> Dict[str, Any]:
        """Parse memory.json content (JSON Lines format)."""
        entities = []
        relations = []
        
        for line_num, line in enumerate(content.strip().split('\n'), 1):
            line = line.strip()
            if not line:
                continue
                
            try:
                obj = json.loads(line)
                
                if obj.get('type') == 'entity':
                    entities.append(obj)
                elif obj.get('type') == 'relation':
                    relations.append(obj)
                else:
                    logger.warning(f"Unknown object type on line {line_num}: {obj.get('type')}")
                    
            except json.JSONDecodeError as e:
                logger.error(f"JSON parse error on line {line_num}: {e}")
                continue
        
        return {
            'entities': entities,
            'relations': relations
        }
    
    def reload(self):
        """Force reload of memory data."""
        self._load_memory_data(force_reload=True)
    
    def query(self, sql: str) -> 'QueryResult':
        """Execute SQL query and return results."""
        # Ensure data is current
        self._load_memory_data()
        
        try:
            # Parse query
            lexer = Lexer(sql)
            tokens = lexer.tokenize()
            ast = self.parser.parse(tokens)
            
            # Execute query
            executor_result = ExecutorQueryResult(self.executor, ast)
            
            # Wrap in our QueryResult class
            return QueryResult(executor_result, sql, self)
            
        except (LexerError, ParseError) as e:
            raise QueryError(f"Query parse error: {e}")
        except Exception as e:
            raise QueryError(f"Query execution error: {e}")
    
    def explain(self, sql: str) -> Dict[str, Any]:
        """Explain query execution plan."""
        try:
            # Parse query
            lexer = Lexer(sql)
            tokens = lexer.tokenize()
            ast = self.parser.parse(tokens)
            
            # Get execution plan info
            return {
                'sql': sql,
                'ast': {
                    'select': ast.select.columns,
                    'from': ast.from_clause.table,
                    'where': str(ast.where.condition) if ast.where else None,
                    'order_by': ast.order_by.fields if ast.order_by else None,
                    'limit': ast.limit.limit if ast.limit else None,
                    'offset': ast.limit.offset if ast.limit and ast.limit.offset else None
                },
                'indexes_available': list(self.executor.indexes.keys()) if self.executor else [],
                'entity_count': len(self.memory_data['entities']) if self.memory_data else 0
            }
            
        except (LexerError, ParseError) as e:
            raise QueryError(f"Query parse error: {e}")
    
    def get_stats(self) -> Dict[str, Any]:
        """Get memory data statistics."""
        if not self.memory_data:
            return {'error': 'No data loaded'}
        
        entities = self.memory_data['entities']
        relations = self.memory_data['relations']
        
        # Calculate entity type distribution
        entity_types = {}
        for entity in entities:
            entity_type = entity.get('entityType', 'Unknown')
            entity_types[entity_type] = entity_types.get(entity_type, 0) + 1
        
        # Calculate relation type distribution
        relation_types = {}
        for relation in relations:
            relation_type = relation.get('relationType', 'Unknown')
            relation_types[relation_type] = relation_types.get(relation_type, 0) + 1
        
        return {
            'file_path': str(self.memory_file_path),
            'last_loaded': self.last_loaded,
            'entity_count': len(entities),
            'relation_count': len(relations),
            'entity_types': entity_types,
            'relation_types': relation_types,
            'indexes': list(self.executor.indexes.keys()) if self.executor else []
        }


class QueryResult:
    """Query result with additional convenience methods."""
    
    def __init__(self, executor_result: ExecutorQueryResult, sql: str, query_interface: MemoryQuery):
        self.executor_result = executor_result
        self.sql = sql
        self.query_interface = query_interface
        self._start_time = time.time()
    
    def fetch_all(self) -> List[Dict[str, Any]]:
        """Fetch all results."""
        return self.executor_result.fetch_all()
    
    def fetch_page(self, page: int = 1, page_size: int = 20) -> Dict[str, Any]:
        """Fetch specific page of results."""
        result = self.executor_result.fetch_page(page, page_size)
        
        # Add query metadata
        result['query'] = {
            'sql': self.sql,
            'execution_time_ms': (time.time() - self._start_time) * 1000
        }
        
        return result
    
    def fetch_first(self, n: int = 1) -> List[Dict[str, Any]]:
        """Fetch first n results."""
        all_results = self.fetch_all()
        return all_results[:n]
    
    def count(self) -> int:
        """Get total result count."""
        return self.executor_result.count()
    
    def is_empty(self) -> bool:
        """Check if result set is empty."""
        return self.count() == 0
    
    def get_stats(self) -> ExecutionStats:
        """Get execution statistics."""
        return self.executor_result.get_stats()
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert result to dictionary format."""
        return {
            'query': self.sql,
            'results': self.fetch_all(),
            'count': self.count(),
            'stats': self.get_stats().__dict__,
            'execution_time_ms': (time.time() - self._start_time) * 1000
        }
    
    def print_summary(self):
        """Print a summary of the results."""
        count = self.count()
        stats = self.get_stats()
        
        print(f"Query: {self.sql}")
        print(f"Results: {count} entities")
        print(f"Scanned: {stats.entities_scanned} entities")
        print(f"Execution: {stats.execution_time_ms:.2f}ms")
        
        if stats.index_hits > 0:
            print(f"Index hits: {stats.index_hits}")
        if stats.index_misses > 0:
            print(f"Index misses: {stats.index_misses}")
    
    def __iter__(self):
        """Make result iterable."""
        return iter(self.fetch_all())
    
    def __len__(self):
        """Support len() function."""
        return self.count()


class MemoryMonitor:
    """Main interface for memory monitoring with query support."""
    
    def __init__(self, memory_file_path: Union[str, Path], auto_reload: bool = True):
        self.memory_file_path = Path(memory_file_path)
        self.auto_reload = auto_reload
        self.query_interface = MemoryQuery(memory_file_path)
        self.file_observer = None
        
        if auto_reload:
            self.file_observer = FileObserver(str(memory_file_path))
    
    def query(self, sql: str) -> QueryResult:
        """Execute query with auto-reload if enabled."""
        if self.auto_reload:
            self.query_interface.reload()
        
        return self.query_interface.query(sql)
    
    def explain(self, sql: str) -> Dict[str, Any]:
        """Explain query execution plan."""
        return self.query_interface.explain(sql)
    
    def get_stats(self) -> Dict[str, Any]:
        """Get memory statistics."""
        return self.query_interface.get_stats()
    
    def wait_for_condition(self, sql: str, timeout: Optional[float] = None) -> Optional[QueryResult]:
        """Wait for query condition to be met."""
        import asyncio
        
        async def wait_async():
            if not self.file_observer:
                # No observer, just check once
                result = self.query(sql)
                return result if not result.is_empty() else None
            
            # Check initial condition
            result = self.query(sql)
            if not result.is_empty():
                return result
            
            # Wait for changes
            start_time = time.time()
            async for change in self.file_observer.watch():
                # Reload and check condition
                self.query_interface.reload()
                result = self.query(sql)
                
                if not result.is_empty():
                    return result
                
                # Check timeout
                if timeout and (time.time() - start_time) > timeout:
                    return None
            
            return None
        
        # Run async function
        try:
            loop = asyncio.get_event_loop()
        except RuntimeError:
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
        
        return loop.run_until_complete(wait_async())
    
    def get_snapshot(self) -> MemorySnapshot:
        """Get current memory snapshot."""
        if self.file_observer:
            snapshot_data = self.file_observer.get_snapshot()
            if snapshot_data.get('exists') and 'content' in snapshot_data:
                return MemorySnapshot(
                    content=snapshot_data['content'],
                    timestamp=snapshot_data['modified']
                )
        
        # Fallback to direct file read
        content = self.memory_file_path.read_text()
        stat = self.memory_file_path.stat()
        return MemorySnapshot(content=content, timestamp=stat.st_mtime)


# Convenience functions
def quick_query(memory_file: Union[str, Path], sql: str) -> List[Dict[str, Any]]:
    """Quick query execution without creating persistent objects."""
    query_interface = MemoryQuery(memory_file)
    result = query_interface.query(sql)
    return result.fetch_all()


def find_entities(memory_file: Union[str, Path], entity_type: str, **filters) -> List[Dict[str, Any]]:
    """Find entities of specific type with optional filters."""
    conditions = [f"entityType = '{entity_type}'"]
    
    for field, value in filters.items():
        if isinstance(value, str):
            conditions.append(f"{field} = '{value}'")
        else:
            conditions.append(f"{field} = {value}")
    
    where_clause = " AND ".join(conditions)
    sql = f"SELECT * FROM entities WHERE {where_clause}"
    
    return quick_query(memory_file, sql)


def search_observations(memory_file: Union[str, Path], search_term: str) -> List[Dict[str, Any]]:
    """Search for entities containing term in observations."""
    sql = f"SELECT * FROM entities WHERE observations CONTAINS '{search_term}'"
    return quick_query(memory_file, sql)


# Example usage
if __name__ == "__main__":
    # Test memory monitor
    try:
        monitor = MemoryMonitor("memory.json")
        
        # Get statistics
        stats = monitor.get_stats()
        print(f"Memory contains {stats['entity_count']} entities")
        
        # Query for tasks
        result = monitor.query("SELECT * FROM entities WHERE entityType = 'Task'")
        print(f"Found {result.count()} tasks")
        
        # Print some results
        for task in result.fetch_first(3):
            print(f"  Task: {task.get('name', 'Unknown')}")
        
        # Search observations
        search_result = monitor.query("SELECT * FROM entities WHERE observations CONTAINS 'status:active'")
        print(f"Found {search_result.count()} active entities")
        
        # Pagination example
        page_result = result.fetch_page(page=1, page_size=5)
        print(f"Page 1 contains {len(page_result['items'])} items")
        print(f"Total pages: {page_result['total_pages']}")
        
    except QueryError as e:
        print(f"Query error: {e}")
    except Exception as e:
        print(f"Error: {e}")