#!/usr/bin/env python3
"""Performance benchmark comparing direct JSONL vs MCP memory operations"""
import time
import json
import tempfile
import os
from pathlib import Path
import subprocess

def benchmark_direct_operations():
    """Benchmark our direct log_observation.py operations"""
    print("🚀 Benchmarking Direct JSONL Operations")
    print("-" * 50)
    
    times = []
    
    # Test 1: Read statistics
    start = time.time()
    result = subprocess.run([
        'python3', 'bin/log_observation.py', 
        '--memory-file', 'fl-memory.json', 
        'stats'
    ], capture_output=True, text=True)
    read_time = (time.time() - start) * 1000
    times.append(('Stats/Read', read_time))
    print(f"✅ Stats/Read: {read_time:.2f}ms")
    
    # Test 2: Add entity/observation
    start = time.time()
    result = subprocess.run([
        'python3', 'bin/log_observation.py',
        '--memory-file', 'fl-memory.json',
        'add-entity', 'BenchmarkTest', 'Tool', 
        f'Benchmark test observation at {time.time()}'
    ], capture_output=True, text=True)
    write_time = (time.time() - start) * 1000
    times.append(('Add Entity/Obs', write_time))
    print(f"✅ Add Entity/Obs: {write_time:.2f}ms")
    
    # Test 3: Get specific entity
    start = time.time()
    result = subprocess.run([
        'python3', 'bin/log_observation.py',
        '--memory-file', 'fl-memory.json',
        'get-entity', 'FlowLoom', '--type', 'Project'
    ], capture_output=True, text=True)
    get_time = (time.time() - start) * 1000
    times.append(('Get Entity', get_time))
    print(f"✅ Get Entity: {get_time:.2f}ms")
    
    # Test 4: List entities by type
    start = time.time()
    result = subprocess.run([
        'python3', 'bin/log_observation.py',
        '--memory-file', 'fl-memory.json',
        'list-entities', '--type', 'Session'
    ], capture_output=True, text=True)
    list_time = (time.time() - start) * 1000
    times.append(('List by Type', list_time))
    print(f"✅ List by Type: {list_time:.2f}ms")
    
    return times

def benchmark_memory_monitor():
    """Benchmark memory monitor operations"""
    print("\n🔍 Benchmarking Memory Monitor Operations")
    print("-" * 50)
    
    times = []
    
    # Test 1: Stats
    start = time.time()
    result = subprocess.run([
        'python3', '-m', 'src.memory_monitor.cli',
        'stats', 'fl-memory.json'
    ], capture_output=True, text=True)
    stats_time = (time.time() - start) * 1000
    times.append(('Monitor Stats', stats_time))
    print(f"✅ Monitor Stats: {stats_time:.2f}ms")
    
    # Test 2: Query entities
    start = time.time()
    result = subprocess.run([
        'python3', '-m', 'src.memory_monitor.cli',
        'query', 'fl-memory.json',
        "SELECT name FROM entities WHERE entityType = 'Project'"
    ], capture_output=True, text=True)
    query_time = (time.time() - start) * 1000
    times.append(('SQL Query', query_time))
    print(f"✅ SQL Query: {query_time:.2f}ms")
    
    # Test 3: Search
    start = time.time()
    result = subprocess.run([
        'python3', '-m', 'src.memory_monitor.cli',
        'search', 'fl-memory.json',
        '--search', 'FlowLoom', '--limit', '5'
    ], capture_output=True, text=True)
    search_time = (time.time() - start) * 1000
    times.append(('Search', search_time))
    print(f"✅ Search: {search_time:.2f}ms")
    
    return times

def get_file_stats():
    """Get current file statistics"""
    memory_file = Path('fl-memory.json')
    if memory_file.exists():
        size_kb = memory_file.stat().st_size / 1024
        
        # Count entities and relations
        entities = 0
        relations = 0
        with open(memory_file, 'r') as f:
            for line in f:
                try:
                    data = json.loads(line.strip())
                    if data.get('type') == 'entity':
                        entities += 1
                    elif data.get('type') == 'relation':
                        relations += 1
                except:
                    continue
        
        return {
            'size_kb': size_kb,
            'entities': entities,
            'relations': relations
        }
    return {}

def print_summary(direct_times, monitor_times, file_stats):
    """Print performance summary"""
    print("\n📊 Performance Summary")
    print("=" * 60)
    print(f"Memory File: {file_stats.get('size_kb', 0):.1f} KB")
    print(f"Entities: {file_stats.get('entities', 0)}")
    print(f"Relations: {file_stats.get('relations', 0)}")
    print()
    
    print("Direct JSONL Operations:")
    for operation, time_ms in direct_times:
        print(f"  {operation:<15}: {time_ms:>7.2f}ms")
    
    print("\nMemory Monitor Operations:")
    for operation, time_ms in monitor_times:
        print(f"  {operation:<15}: {time_ms:>7.2f}ms")
    
    print("\nComparison Notes:")
    print("- Direct operations include full file read/write")
    print("- Memory monitor includes SQL parsing and indexing")
    print("- Both provide different capabilities and use cases")

def main():
    print("🏁 FlowLoom Memory Performance Benchmark")
    print("=" * 60)
    
    # Get file statistics
    file_stats = get_file_stats()
    
    # Run benchmarks
    direct_times = benchmark_direct_operations()
    monitor_times = benchmark_memory_monitor()
    
    # Print summary
    print_summary(direct_times, monitor_times, file_stats)
    
    print(f"\n⏱️  Benchmark completed at {time.strftime('%Y-%m-%d %H:%M:%S')}")

if __name__ == "__main__":
    main()