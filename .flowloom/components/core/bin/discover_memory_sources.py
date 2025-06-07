#!/usr/bin/env python3
"""Comprehensive memory source discovery with git archaeology"""
import os
import json
import subprocess
from pathlib import Path
from typing import List, Dict, Tuple
import argparse

def find_all_memory_sources():
    """Find ALL memory.json files from every possible source"""
    memory_sources = {
        'filesystem': [],
        'git_history': [],
        'branches': [],
        'mcp_cache': [],
        'sessions': [],
        'backups': []
    }
    
    # 1. Current filesystem
    project_root = Path.cwd()
    if (project_root / "memory.json").exists():
        memory_sources['filesystem'].append({
            'path': project_root / "memory.json",
            'source': 'current_working_directory',
            'timestamp': os.path.getmtime(project_root / "memory.json"),
            'size': os.path.getsize(project_root / "memory.json")
        })
    
    # 2. Git history archaeology - extract from ALL commits
    print("🔍 Mining git history for memory.json...")
    git_memory_files = extract_git_memory_history()
    memory_sources['git_history'].extend(git_memory_files)
    
    # 3. Branch archaeology - get from all branch heads
    print("🌿 Extracting memory.json from all branches...")
    branch_memory_files = extract_branch_memory_files()
    memory_sources['branches'].extend(branch_memory_files)
    
    # 4. MCP cache directories
    print("📦 Scanning MCP cache directories...")
    npx_cache = Path.home() / ".npm/_npx"
    if npx_cache.exists():
        for cache_dir in npx_cache.glob("*"):
            memory_path = cache_dir / "node_modules/@modelcontextprotocol/server-memory/dist/memory.json"
            if memory_path.exists():
                memory_sources['mcp_cache'].append({
                    'path': memory_path,
                    'source': f'mcp_cache_{cache_dir.name}',
                    'timestamp': os.path.getmtime(memory_path),
                    'size': os.path.getsize(memory_path)
                })
    
    # 5. Session directories and worktrees
    print("📁 Checking session directories...")
    for session_dir in project_root.glob("sessions/*/"):
        session_memory = session_dir / "memory.json"
        if session_memory.exists():
            memory_sources['sessions'].append({
                'path': session_memory,
                'source': f'session_{session_dir.name}',
                'timestamp': os.path.getmtime(session_memory),
                'size': os.path.getsize(session_memory)
            })
    
    # 6. Backup files
    print("💾 Finding backup files...")
    for backup in project_root.glob("memory-backup-*.json"):
        memory_sources['backups'].append({
            'path': backup,
            'source': 'backup_file',
            'timestamp': os.path.getmtime(backup),
            'size': os.path.getsize(backup)
        })
    
    # 7. Other potential locations
    for potential_dir in ['.flowloom', '.knowledge', '.knowledge-curated']:
        potential_path = project_root / potential_dir / "memory.json"
        if potential_path.exists():
            memory_sources['filesystem'].append({
                'path': potential_path,
                'source': f'directory_{potential_dir}',
                'timestamp': os.path.getmtime(potential_path),
                'size': os.path.getsize(potential_path)
            })
    
    return memory_sources

def extract_git_memory_history() -> List[Dict]:
    """Extract memory.json from all commits in git history"""
    memory_files = []
    
    try:
        # Get all commits that touched memory.json across all branches
        result = subprocess.run([
            'git', 'log', '--all', '--oneline', '--', 'memory.json'
        ], capture_output=True, text=True, check=True)
        
        commits = [line for line in result.stdout.strip().split('\n') if line]
        print(f"Found {len(commits)} commits with memory.json changes")
        
        for line in commits:
            if line:
                commit_hash = line.split()[0]
                commit_msg = ' '.join(line.split()[1:])
                
                # Extract memory.json content from this commit
                try:
                    content_result = subprocess.run([
                        'git', 'show', f'{commit_hash}:memory.json'
                    ], capture_output=True, text=True, check=True)
                    
                    memory_files.append({
                        'content': content_result.stdout,
                        'source': f'git_commit_{commit_hash}',
                        'commit_hash': commit_hash,
                        'commit_message': commit_msg,
                        'timestamp': get_commit_timestamp(commit_hash),
                        'size': len(content_result.stdout.encode())
                    })
                except subprocess.CalledProcessError:
                    print(f"  ⚠️  Could not extract memory.json from commit {commit_hash}")
                    continue
    
    except subprocess.CalledProcessError:
        print("  ❌ No git history found for memory.json")
    
    return memory_files

def extract_branch_memory_files() -> List[Dict]:
    """Extract memory.json from all branch heads"""
    memory_files = []
    
    try:
        # Get all branches (local and remote)
        result = subprocess.run([
            'git', 'branch', '-a'
        ], capture_output=True, text=True, check=True)
        
        branches = [line.strip().replace('* ', '').replace('remotes/', '') 
                   for line in result.stdout.split('\n') if line.strip()]
        branches = [b for b in branches if b and not b.startswith('origin/HEAD')]
        
        print(f"Found {len(branches)} branches to check")
        
        for branch in branches:
            try:
                content_result = subprocess.run([
                    'git', 'show', f'{branch}:memory.json'
                ], capture_output=True, text=True, check=True)
                
                memory_files.append({
                    'content': content_result.stdout,
                    'source': f'branch_{branch}',
                    'branch': branch,
                    'timestamp': get_branch_timestamp(branch),
                    'size': len(content_result.stdout.encode())
                })
            except subprocess.CalledProcessError:
                print(f"  ⚠️  No memory.json in branch {branch}")
                continue
    
    except subprocess.CalledProcessError:
        print("  ❌ Could not list git branches")
    
    return memory_files

def get_commit_timestamp(commit_hash: str) -> int:
    """Get timestamp of a commit"""
    try:
        result = subprocess.run([
            'git', 'show', '-s', '--format=%ct', commit_hash
        ], capture_output=True, text=True, check=True)
        return int(result.stdout.strip())
    except:
        return 0

def get_branch_timestamp(branch: str) -> int:
    """Get timestamp of branch head commit"""
    try:
        result = subprocess.run([
            'git', 'show', '-s', '--format=%ct', branch
        ], capture_output=True, text=True, check=True)
        return int(result.stdout.strip())
    except:
        return 0

def analyze_memory_content(content: str, source: str) -> Dict:
    """Analyze JSONL memory content structure"""
    entities = 0
    relations = 0
    lines = 0
    errors = []
    
    for line_num, line in enumerate(content.split('\n'), 1):
        if line.strip():
            lines += 1
            try:
                data = json.loads(line)
                if isinstance(data, dict):
                    if data.get('type') == 'entity':
                        entities += 1
                    elif data.get('type') == 'relation':
                        relations += 1
                else:
                    errors.append(f"Line {line_num}: Not a JSON object")
            except json.JSONDecodeError as e:
                errors.append(f"Line {line_num}: {str(e)}")
            except Exception as e:
                errors.append(f"Line {line_num}: Unexpected error - {str(e)}")
    
    return {
        'source': source,
        'lines': lines,
        'entities': entities,
        'relations': relations,
        'errors': errors,
        'valid': len(errors) == 0
    }

def generate_discovery_report(memory_sources: Dict) -> None:
    """Generate comprehensive discovery report"""
    print("\n" + "="*60)
    print("🔍 COMPREHENSIVE MEMORY SOURCE DISCOVERY REPORT")
    print("="*60)
    
    total_sources = 0
    total_entities = 0
    total_relations = 0
    
    for category, sources in memory_sources.items():
        if sources:
            print(f"\n📂 {category.upper()} ({len(sources)} sources)")
            print("-" * 40)
            
            for source in sources:
                total_sources += 1
                if 'path' in source:
                    # File-based source
                    print(f"  📄 {source['source']}")
                    print(f"      Path: {source['path']}")
                    print(f"      Size: {source['size']} bytes")
                    
                    # Analyze content
                    try:
                        with open(source['path'], 'r') as f:
                            content = f.read()
                        analysis = analyze_memory_content(content, source['source'])
                        print(f"      Entities: {analysis['entities']}, Relations: {analysis['relations']}")
                        print(f"      Status: {'✅ Valid' if analysis['valid'] else '❌ Has errors'}")
                        total_entities += analysis['entities']
                        total_relations += analysis['relations']
                    except Exception as e:
                        print(f"      ❌ Error reading file: {e}")
                        
                elif 'content' in source:
                    # Git-based source
                    print(f"  📄 {source['source']}")
                    if 'commit_hash' in source:
                        print(f"      Commit: {source['commit_hash']}")
                        print(f"      Message: {source['commit_message'][:50]}...")
                    elif 'branch' in source:
                        print(f"      Branch: {source['branch']}")
                    print(f"      Size: {source['size']} bytes")
                    
                    analysis = analyze_memory_content(source['content'], source['source'])
                    print(f"      Entities: {analysis['entities']}, Relations: {analysis['relations']}")
                    print(f"      Status: {'✅ Valid' if analysis['valid'] else '❌ Has errors'}")
                    total_entities += analysis['entities']
                    total_relations += analysis['relations']
    
    print(f"\n📊 SUMMARY")
    print(f"   Total Sources: {total_sources}")
    print(f"   Total Entities: {total_entities}")
    print(f"   Total Relations: {total_relations}")
    print("="*60)

def main():
    parser = argparse.ArgumentParser(description='Discover all memory.json sources')
    parser.add_argument('--output', '-o', help='Output file for results JSON')
    parser.add_argument('--report-only', action='store_true', help='Only generate report')
    args = parser.parse_args()
    
    print("🚀 Starting comprehensive memory source discovery...")
    memory_sources = find_all_memory_sources()
    
    generate_discovery_report(memory_sources)
    
    if args.output:
        # Convert paths to strings for JSON serialization
        json_sources = {}
        for category, sources in memory_sources.items():
            json_sources[category] = []
            for source in sources:
                json_source = source.copy()
                if 'path' in json_source:
                    json_source['path'] = str(json_source['path'])
                json_sources[category].append(json_source)
        
        with open(args.output, 'w') as f:
            json.dump(json_sources, f, indent=2)
        print(f"\n💾 Results saved to {args.output}")

if __name__ == "__main__":
    main()