"""
Query Executor for Memory Monitor.

Executes optimized query plans against memory.json data with
efficient filtering, sorting, and pagination.
"""

import json
import re
from typing import List, Dict, Any, Iterator, Optional, Callable, Tuple
from datetime import datetime, timezone
from dataclasses import dataclass
from enum import Enum
import logging

from .parser import (
    QueryAST, Condition, ComparisonCondition, ContainsCondition, 
    MatchCondition, InCondition, BetweenCondition, LogicalCondition,
    NotCondition, FunctionCondition
)

logger = logging.getLogger(__name__)


class IndexType(Enum):
    BTREE = "btree"
    HASH = "hash"
    FULLTEXT = "fulltext"


@dataclass
class Index:
    name: str
    type: IndexType
    field: str
    data: Dict[Any, List[int]]  # value -> list of entity indices


@dataclass
class ExecutionStats:
    entities_scanned: int = 0
    entities_filtered: int = 0
    entities_returned: int = 0
    execution_time_ms: float = 0.0
    index_hits: int = 0
    index_misses: int = 0


class QueryExecutor:
    """Execute queries against memory data."""
    
    def __init__(self, memory_data: Dict[str, Any]):
        self.memory_data = memory_data
        self.entities = memory_data.get('entities', [])
        self.relations = memory_data.get('relations', [])
        self.indexes = {}
        self.stats = ExecutionStats()
        self._build_indexes()
    
    def _build_indexes(self):
        """Build indexes for common query patterns."""
        # Entity type index (hash)
        entity_type_index = {}
        for i, entity in enumerate(self.entities):
            entity_type = entity.get('entityType', 'Unknown')
            if entity_type not in entity_type_index:
                entity_type_index[entity_type] = []
            entity_type_index[entity_type].append(i)
        
        self.indexes['entityType'] = Index(
            name='entityType_idx',
            type=IndexType.HASH,
            field='entityType',
            data=entity_type_index
        )
        
        # Entity name index (btree-like)
        name_index = {}
        for i, entity in enumerate(self.entities):
            name = entity.get('name', '')
            if name not in name_index:
                name_index[name] = []
            name_index[name].append(i)
        
        self.indexes['name'] = Index(
            name='name_idx',
            type=IndexType.BTREE,
            field='name',
            data=name_index
        )
        
        logger.info(f"Built indexes: {list(self.indexes.keys())}")
    
    def execute(self, ast: QueryAST) -> Iterator[Dict[str, Any]]:
        """Execute query AST and yield results."""
        import time
        start_time = time.time()
        
        try:
            # Reset stats
            self.stats = ExecutionStats()
            
            # Start with all entities or filtered set
            result_set = self._apply_where_clause(ast.where)
            
            # Apply sorting if needed
            if ast.order_by:
                result_set = self._apply_sorting(result_set, ast.order_by.fields)
            
            # Apply limit/offset if needed
            if ast.limit:
                result_set = self._apply_limit(result_set, ast.limit.limit, ast.limit.offset or 0)
            
            # Apply column selection
            result_set = self._apply_select(result_set, ast.select.columns)
            
            # Yield results
            for entity in result_set:
                self.stats.entities_returned += 1
                yield entity
                
        finally:
            self.stats.execution_time_ms = (time.time() - start_time) * 1000
    
    def _apply_where_clause(self, where_clause) -> List[Dict[str, Any]]:
        """Apply WHERE clause filtering."""
        if not where_clause:
            self.stats.entities_scanned = len(self.entities)
            return list(self.entities)
        
        # Try to use indexes for initial filtering
        indexed_entities = self._try_index_scan(where_clause.condition)
        
        if indexed_entities is not None:
            # Use index results
            entities = indexed_entities
            self.stats.index_hits += 1
        else:
            # Full table scan
            entities = list(self.entities)
            self.stats.index_misses += 1
        
        self.stats.entities_scanned = len(entities)
        
        # Apply remaining conditions
        filtered_entities = []
        for entity in entities:
            if self._evaluate_condition(where_clause.condition, entity):
                filtered_entities.append(entity)
        
        self.stats.entities_filtered = len(filtered_entities)
        return filtered_entities
    
    def _try_index_scan(self, condition: Condition) -> Optional[List[Dict[str, Any]]]:
        """Try to use indexes for initial filtering."""
        if isinstance(condition, ComparisonCondition):
            if (condition.field in self.indexes and 
                condition.operator == '=' and
                self.indexes[condition.field].type == IndexType.HASH):
                
                index = self.indexes[condition.field]
                entity_indices = index.data.get(condition.value, [])
                return [self.entities[i] for i in entity_indices]
        
        elif isinstance(condition, InCondition):
            if (condition.field in self.indexes and
                self.indexes[condition.field].type == IndexType.HASH):
                
                index = self.indexes[condition.field]
                entity_indices = []
                for value in condition.values:
                    entity_indices.extend(index.data.get(value, []))
                
                # Remove duplicates while preserving order
                seen = set()
                unique_indices = []
                for i in entity_indices:
                    if i not in seen:
                        seen.add(i)
                        unique_indices.append(i)
                
                return [self.entities[i] for i in unique_indices]
        
        return None
    
    def _evaluate_condition(self, condition: Condition, entity: Dict[str, Any]) -> bool:
        """Evaluate condition against entity."""
        if isinstance(condition, ComparisonCondition):
            return self._evaluate_comparison(condition, entity)
        
        elif isinstance(condition, ContainsCondition):
            return self._evaluate_contains(condition, entity)
        
        elif isinstance(condition, MatchCondition):
            return self._evaluate_match(condition, entity)
        
        elif isinstance(condition, InCondition):
            return self._evaluate_in(condition, entity)
        
        elif isinstance(condition, BetweenCondition):
            return self._evaluate_between(condition, entity)
        
        elif isinstance(condition, LogicalCondition):
            return self._evaluate_logical(condition, entity)
        
        elif isinstance(condition, NotCondition):
            return not self._evaluate_condition(condition.condition, entity)
        
        elif isinstance(condition, FunctionCondition):
            return self._evaluate_function(condition, entity)
        
        else:
            logger.warning(f"Unknown condition type: {type(condition)}")
            return False
    
    def _evaluate_comparison(self, condition: ComparisonCondition, entity: Dict[str, Any]) -> bool:
        """Evaluate comparison condition."""
        field_value = self._get_field_value(entity, condition.field)
        
        if field_value is None:
            return False
        
        try:
            if condition.operator == '=':
                return field_value == condition.value
            elif condition.operator == '!=':
                return field_value != condition.value
            elif condition.operator == '<':
                return field_value < condition.value
            elif condition.operator == '>':
                return field_value > condition.value
            elif condition.operator == '<=':
                return field_value <= condition.value
            elif condition.operator == '>=':
                return field_value >= condition.value
            else:
                return False
                
        except TypeError:
            # Type mismatch in comparison
            return False
    
    def _evaluate_contains(self, condition: ContainsCondition, entity: Dict[str, Any]) -> bool:
        """Evaluate CONTAINS condition."""
        if condition.field == 'observations':
            observations = entity.get('observations', [])
            search_value = condition.value.lower()
            
            for obs in observations:
                if search_value in obs.lower():
                    return True
            return False
        
        else:
            # Contains on other fields
            field_value = self._get_field_value(entity, condition.field)
            if isinstance(field_value, str):
                return condition.value.lower() in field_value.lower()
            elif isinstance(field_value, list):
                return any(condition.value.lower() in str(item).lower() for item in field_value)
            
        return False
    
    def _evaluate_match(self, condition: MatchCondition, entity: Dict[str, Any]) -> bool:
        """Evaluate MATCH condition (regex)."""
        if condition.field == 'observations':
            observations = entity.get('observations', [])
            
            try:
                pattern = re.compile(condition.pattern, re.IGNORECASE)
                for obs in observations:
                    if pattern.search(obs):
                        return True
                return False
                
            except re.error:
                logger.warning(f"Invalid regex pattern: {condition.pattern}")
                return False
        
        else:
            # Regex on other fields
            field_value = self._get_field_value(entity, condition.field)
            if isinstance(field_value, str):
                try:
                    pattern = re.compile(condition.pattern, re.IGNORECASE)
                    return bool(pattern.search(field_value))
                except re.error:
                    return False
            
        return False
    
    def _evaluate_in(self, condition: InCondition, entity: Dict[str, Any]) -> bool:
        """Evaluate IN condition."""
        field_value = self._get_field_value(entity, condition.field)
        return field_value in condition.values
    
    def _evaluate_between(self, condition: BetweenCondition, entity: Dict[str, Any]) -> bool:
        """Evaluate BETWEEN condition."""
        field_value = self._get_field_value(entity, condition.field)
        
        if field_value is None:
            return False
        
        try:
            return condition.start <= field_value <= condition.end
        except TypeError:
            return False
    
    def _evaluate_logical(self, condition: LogicalCondition, entity: Dict[str, Any]) -> bool:
        """Evaluate logical condition (AND/OR)."""
        left_result = self._evaluate_condition(condition.left, entity)
        
        if condition.operator == 'AND':
            # Short-circuit evaluation for AND
            if not left_result:
                return False
            return self._evaluate_condition(condition.right, entity)
        
        elif condition.operator == 'OR':
            # Short-circuit evaluation for OR
            if left_result:
                return True
            return self._evaluate_condition(condition.right, entity)
        
        return False
    
    def _evaluate_function(self, condition: FunctionCondition, entity: Dict[str, Any]) -> bool:
        """Evaluate function condition."""
        if condition.function == 'RELATED_TO':
            # Find entities related to given entity
            if len(condition.args) != 1:
                return False
            
            target_entity = condition.args[0]
            entity_name = entity.get('name', '')
            
            # Check if there's a relation between entities
            for relation in self.relations:
                if ((relation.get('from') == entity_name and relation.get('to') == target_entity) or
                    (relation.get('from') == target_entity and relation.get('to') == entity_name)):
                    return True
            
            return False
        
        elif condition.function == 'HAS_RELATION':
            # Check if entity has specific relation type
            if len(condition.args) != 2:
                return False
            
            relation_type = condition.args[0]
            target_entity = condition.args[1]
            entity_name = entity.get('name', '')
            
            for relation in self.relations:
                if (relation.get('from') == entity_name and 
                    relation.get('to') == target_entity and
                    relation.get('relationType') == relation_type):
                    return True
            
            return False
        
        return False
    
    def _get_field_value(self, entity: Dict[str, Any], field: str) -> Any:
        """Get field value from entity, handling nested fields."""
        if '.' in field:
            # Handle nested field access
            parts = field.split('.')
            value = entity
            for part in parts:
                if isinstance(value, dict) and part in value:
                    value = value[part]
                else:
                    return None
            return value
        else:
            return entity.get(field)
    
    def _apply_sorting(self, entities: List[Dict[str, Any]], 
                      sort_fields: List[Tuple[str, str]]) -> List[Dict[str, Any]]:
        """Apply sorting to entities."""
        def sort_key(entity):
            """Generate sort key for entity."""
            key = []
            for field, direction in sort_fields:
                value = self._get_field_value(entity, field)
                
                # Handle None values (put them last)
                if value is None:
                    value = '' if direction == 'ASC' else 'zzz'
                
                # Handle date strings
                if isinstance(value, str) and self._looks_like_date(value):
                    try:
                        value = datetime.fromisoformat(value.replace('Z', '+00:00'))
                    except ValueError:
                        pass
                
                # Reverse for DESC
                if direction == 'DESC':
                    if isinstance(value, (int, float)):
                        value = -value
                    elif isinstance(value, str):
                        # Reverse string sorting is complex, use tuple trick
                        key.append((1, value))  # Will be sorted in reverse
                        continue
                    elif isinstance(value, datetime):
                        value = datetime.max - value
                
                key.append((0, value))
            
            return key
        
        try:
            return sorted(entities, key=sort_key)
        except TypeError as e:
            logger.warning(f"Sorting failed: {e}")
            return entities
    
    def _looks_like_date(self, value: str) -> bool:
        """Check if string looks like ISO date."""
        return (isinstance(value, str) and 
                len(value) >= 10 and
                ('T' in value or '-' in value[:10]))
    
    def _apply_limit(self, entities: List[Dict[str, Any]], 
                    limit: int, offset: int) -> List[Dict[str, Any]]:
        """Apply LIMIT and OFFSET."""
        start = offset
        end = offset + limit
        return entities[start:end]
    
    def _apply_select(self, entities: List[Dict[str, Any]], 
                     columns: List[str]) -> Iterator[Dict[str, Any]]:
        """Apply column selection."""
        if '*' in columns:
            # Return all columns
            yield from entities
        else:
            # Return only selected columns
            for entity in entities:
                selected_entity = {}
                for column in columns:
                    if column in entity:
                        selected_entity[column] = entity[column]
                    else:
                        selected_entity[column] = None
                yield selected_entity


@dataclass 
class QueryResult:
    """Query result with metadata and pagination support."""
    
    def __init__(self, executor: QueryExecutor, ast: QueryAST):
        self.executor = executor
        self.ast = ast
        self._results = None
        self._count = None
    
    def fetch_all(self) -> List[Dict[str, Any]]:
        """Fetch all results."""
        if self._results is None:
            self._results = list(self.executor.execute(self.ast))
        return self._results
    
    def fetch_page(self, page: int, page_size: int) -> Dict[str, Any]:
        """Fetch specific page of results."""
        offset = (page - 1) * page_size
        
        # Create modified AST with pagination
        paginated_ast = QueryAST(
            select=self.ast.select,
            from_clause=self.ast.from_clause,
            where=self.ast.where,
            order_by=self.ast.order_by,
            limit=type('LimitClause', (), {'limit': page_size, 'offset': offset})()
        )
        
        items = list(self.executor.execute(paginated_ast))
        total = self.count()
        
        return {
            'items': items,
            'page': page,
            'page_size': page_size,
            'total': total,
            'has_more': offset + len(items) < total,
            'total_pages': (total + page_size - 1) // page_size
        }
    
    def count(self) -> int:
        """Get total result count without limit."""
        if self._count is None:
            # Create count AST (without limit/offset)
            count_ast = QueryAST(
                select=self.ast.select,
                from_clause=self.ast.from_clause,
                where=self.ast.where,
                order_by=None,
                limit=None
            )
            
            self._count = sum(1 for _ in self.executor.execute(count_ast))
        
        return self._count
    
    def get_stats(self) -> ExecutionStats:
        """Get execution statistics."""
        return self.executor.stats


# Example usage and testing
if __name__ == "__main__":
    # Test data
    test_memory_data = {
        'entities': [
            {
                'name': 'Task-1',
                'entityType': 'Task',
                'observations': ['status:active', 'priority:high'],
                'created': '2025-05-28T10:00:00Z'
            },
            {
                'name': 'Task-2', 
                'entityType': 'Task',
                'observations': ['status:completed', 'priority:low'],
                'created': '2025-05-28T11:00:00Z'
            },
            {
                'name': 'Worker-1',
                'entityType': 'Worker',
                'observations': ['status:active', 'assigned_to:Task-1'],
                'created': '2025-05-28T09:00:00Z'
            }
        ],
        'relations': [
            {
                'from': 'Worker-1',
                'to': 'Task-1', 
                'relationType': 'assigned_to'
            }
        ]
    }
    
    # Test executor
    executor = QueryExecutor(test_memory_data)
    
    # Test simple AST execution
    from .parser import QueryAST, SelectClause, FromClause, WhereClause, ComparisonCondition
    
    ast = QueryAST(
        select=SelectClause(columns=['*']),
        from_clause=FromClause(table='entities'),
        where=WhereClause(
            condition=ComparisonCondition(field='entityType', operator='=', value='Task')
        )
    )
    
    result = QueryResult(executor, ast)
    tasks = result.fetch_all()
    
    print(f"Found {len(tasks)} tasks:")
    for task in tasks:
        print(f"  {task['name']}: {task['observations']}")
    
    print(f"Execution stats: {result.get_stats()}")