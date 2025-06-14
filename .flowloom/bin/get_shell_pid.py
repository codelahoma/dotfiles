#!/usr/bin/env python
"""
Get the shell PID by finding the parent node process of Claude
"""

import os
import psutil

def get_shell_pid():
    """Find the shell that is the parent of Claude (node process)."""
    current_pid = os.getpid()
    
    try:
        current_process = psutil.Process(current_pid)
        
        # Walk up the process tree to find the node process (Claude)
        node_process = None
        while current_process.parent():
            current_process = current_process.parent()
            
            # Check if this is a node process (Claude)
            if 'node' in current_process.name().lower():
                node_process = current_process
                break
        
        # If we found the node process, get its parent (the shell)
        if node_process and node_process.parent():
            shell_process = node_process.parent()
            # Return the parent PID regardless of what type of process it is
            # In interactive mode it will be a shell
            return str(shell_process.pid)
        
        # If no node process found, walk up from current to find any shell
        current_process = psutil.Process(current_pid)
        while current_process.parent():
            parent = current_process.parent()
            parent_name = parent.name().lower()
            
            # Look for common shell names
            if any(shell in parent_name for shell in ['bash', 'zsh', 'fish', 'sh', 'tcsh', 'csh']):
                return str(parent.pid)
            
            current_process = parent
        
        # Fallback to current PID
        return str(current_pid)
        
    except (psutil.NoSuchProcess, psutil.AccessDenied):
        # Fallback to current PID
        return str(current_pid)

if __name__ == "__main__":
    print(get_shell_pid())