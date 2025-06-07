#!/usr/bin/env python3
"""
timeline.py - Visualize FlowLoom memory activity in chronological order

This tool queries the memory system and displays activities in a timeline format,
making it easy to see what happened when and by which shell session.

Usage:
    ./bin/timeline.py              # Show today's timeline
    ./bin/timeline.py 2025-05-26   # Show specific date
    ./bin/timeline.py --shell 58883 # Filter by shell ID
    ./bin/timeline.py --hours 2     # Show last 2 hours
"""

import json
import sys
import subprocess
from datetime import datetime, timedelta, timezone
from pathlib import Path
import argparse

def get_project_root():
    """Find the FlowLoom project root."""
    current = Path.cwd()
    while current != current.parent:
        if (current / ".flowloom").exists() or (current / "flowloom.json").exists():
            return current
        current = current.parent
    # Default to current directory if no markers found
    return Path.cwd()

def search_memory(query):
    """Search memory using mcp__memory__search_nodes."""
    # For this example, we'll read memory.json directly
    # In production, this would use the MCP server
    project_root = get_project_root()
    memory_file = project_root / "memory.json"
    
    if not memory_file.exists():
        return []
    
    entities = []
    with open(memory_file, 'r') as f:
        for line in f:
            try:
                entity = json.loads(line.strip())
                entities.append(entity)
            except json.JSONDecodeError:
                continue
    
    # Filter by query
    results = []
    for entity in entities:
        if entity.get('type') == 'entity' and 'observations' in entity:
            matching_observations = []
            for obs in entity['observations']:
                if query.lower() in obs.lower():
                    matching_observations.append(obs)
            
            if matching_observations:
                results.append({
                    'name': entity.get('name', 'Unknown'),
                    'type': entity.get('entityType', 'Unknown'),
                    'observations': matching_observations
                })
    
    return results

def parse_observation(obs):
    """Parse an observation to extract timestamp, shell ID, and content."""
    # Format: Shell_ID: 58883 - 2025-05-27T20:50:00Z | content
    parts = obs.split(' | ', 1)
    if len(parts) < 2:
        return None
    
    header = parts[0]
    content = parts[1] if len(parts) > 1 else ""
    
    # Extract shell ID and timestamp
    if ' - ' in header and 'Shell_ID:' in header:
        shell_part, timestamp_str = header.split(' - ', 1)
        shell_id = shell_part.replace('Shell_ID:', '').strip()
        
        try:
            # Parse ISO timestamp
            timestamp = datetime.fromisoformat(timestamp_str.replace('Z', '+00:00'))
            return {
                'timestamp': timestamp,
                'shell_id': shell_id,
                'content': content,
                'raw': obs
            }
        except ValueError:
            pass
    
    return None

def format_timeline_entry(parsed_obs, entity_name, entity_type):
    """Format a timeline entry for display."""
    time_str = parsed_obs['timestamp'].strftime('%H:%M:%S')
    shell_str = f"[{parsed_obs['shell_id']}]"
    
    # Color coding for different types
    type_colors = {
        'Component': '\033[94m',      # Blue
        'Configuration': '\033[93m',   # Yellow
        'Architecture': '\033[95m',    # Magenta
        'Implementation': '\033[92m',  # Green
        'activity_log': '\033[96m',    # Cyan
    }
    
    color = type_colors.get(entity_type, '\033[90m')  # Default gray
    reset = '\033[0m'
    
    return f"{time_str} {shell_str:>8} {color}{entity_name}{reset}: {parsed_obs['content']}"

def main():
    parser = argparse.ArgumentParser(description='View FlowLoom activity timeline')
    parser.add_argument('date', nargs='?', help='Date to view (YYYY-MM-DD), defaults to today')
    parser.add_argument('--shell', type=str, help='Filter by shell ID')
    parser.add_argument('--hours', type=int, help='Show last N hours')
    parser.add_argument('--all', action='store_true', help='Show all entries (no date filter)')
    
    args = parser.parse_args()
    
    # Determine query
    if args.all:
        query = "Shell_ID:"  # Match all timestamped entries
    elif args.hours:
        start_time = datetime.now(timezone.utc) - timedelta(hours=args.hours)
        query = start_time.strftime('%Y-%m-%dT%H:')
    elif args.date:
        query = args.date + 'T'
    else:
        # Default to today
        query = datetime.now(timezone.utc).strftime('%Y-%m-%dT')
    
    # Search memory
    print(f"\n🕐 FlowLoom Activity Timeline")
    print(f"{'='*60}")
    
    results = search_memory(query)
    
    # Parse and collect all timeline entries
    timeline_entries = []
    for result in results:
        entity_name = result['name']
        entity_type = result['type']
        
        for obs in result['observations']:
            parsed = parse_observation(obs)
            if parsed:
                # Apply shell filter if specified
                if args.shell and parsed['shell_id'] != args.shell:
                    continue
                
                timeline_entries.append({
                    'parsed': parsed,
                    'entity_name': entity_name,
                    'entity_type': entity_type
                })
    
    # Sort by timestamp
    timeline_entries.sort(key=lambda x: x['parsed']['timestamp'])
    
    # Display timeline
    if not timeline_entries:
        print("No activities found for the specified criteria.")
    else:
        current_date = None
        for entry in timeline_entries:
            # Add date header when date changes
            entry_date = entry['parsed']['timestamp'].date()
            if entry_date != current_date:
                current_date = entry_date
                print(f"\n📅 {current_date.strftime('%A, %B %d, %Y')}")
                print(f"{'-'*60}")
            
            print(format_timeline_entry(
                entry['parsed'],
                entry['entity_name'],
                entry['entity_type']
            ))
    
    print(f"\n{'='*60}")
    print(f"Total entries: {len(timeline_entries)}")
    
    if args.shell:
        print(f"Filtered by Shell ID: {args.shell}")
    
    # Show unique shell IDs
    shell_ids = set(e['parsed']['shell_id'] for e in timeline_entries)
    if shell_ids and not args.shell:
        print(f"Active shells: {', '.join(sorted(shell_ids))}")

if __name__ == '__main__':
    main()