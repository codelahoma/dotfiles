#!/usr/bin/env python3
"""
Get current session ID for footer display without permission prompts.
Reads from memory graph to find established shell ID.
"""

import json
import subprocess
import sys
from pathlib import Path

def get_shell_id_from_memory():
    """Extract shell ID from memory graph without triggering security prompts."""
    try:
        # Use MCP memory search to find shell ID
        result = subprocess.run([
            'mcp__memory__search_nodes', 
            json.dumps({"query": "Shell_ID"})
        ], capture_output=True, text=True)
        
        if result.returncode != 0:
            return None
            
        data = json.loads(result.stdout)
        
        # Look for shell ID in observations
        for entity in data.get('entities', []):
            for obs in entity.get('observations', []):
                if 'Shell_ID:' in obs:
                    # Extract shell ID from observation
                    parts = obs.split('Shell_ID:')
                    if len(parts) > 1:
                        shell_id = parts[1].split('-')[0].strip()
                        return shell_id
                        
        return None
        
    except Exception:
        return None

def get_current_session():
    """Get current session ID for footer display."""
    shell_id = get_shell_id_from_memory()
    
    if shell_id:
        return f"Session {shell_id}"
    else:
        return "No active session"

if __name__ == "__main__":
    print(get_current_session())