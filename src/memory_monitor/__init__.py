"""
FlowLoom Memory Monitor

A real-time observer and query engine for FlowLoom's memory graph.
Provides SQL-like querying, change detection, and streaming capabilities.
"""

from .monitor import MemoryMonitor
from .query import MemoryQuery, QueryResult
from .parser import Parser, QueryAST
from .executor import QueryExecutor
from .file_observer import FileObserver

__version__ = "0.1.0"
__all__ = [
    "MemoryMonitor", 
    "MemoryQuery", 
    "QueryResult",
    "Parser", 
    "QueryAST",
    "QueryExecutor",
    "FileObserver"
]