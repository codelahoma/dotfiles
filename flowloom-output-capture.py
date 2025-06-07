#!/usr/bin/env python3
"""
FlowLoom Output Capture
Captures Claude Code output and writes it to the UI response file
"""

import sys
import os
import time
from pathlib import Path

class OutputCapture:
    def __init__(self):
        self.response_file = Path("/tmp/flowloom-ui-response.md")
        self.original_stdout = sys.stdout
        self.original_stderr = sys.stderr
        
    def start_capture(self):
        """Start capturing stdout and stderr"""
        sys.stdout = self
        sys.stderr = self
        
    def stop_capture(self):
        """Stop capturing and restore original streams"""
        sys.stdout = self.original_stdout
        sys.stderr = self.original_stderr
        
    def write(self, text):
        """Write to both original stream and response file"""
        # Write to original stream
        self.original_stdout.write(text)
        self.original_stdout.flush()
        
        # Append to response file if it's substantial content
        if text.strip() and len(text.strip()) > 1:
            try:
                with open(self.response_file, 'a', encoding='utf-8') as f:
                    f.write(text)
                    f.flush()
            except Exception as e:
                # Don't let file writing errors break the output
                self.original_stdout.write(f"[Output capture error: {e}]\n")
                
    def flush(self):
        """Flush both streams"""
        self.original_stdout.flush()

# Global instance
_capture = OutputCapture()

def start_capture():
    """Start output capture"""
    _capture.start_capture()
    
def stop_capture():
    """Stop output capture"""
    _capture.stop_capture()

if __name__ == "__main__":
    # Test the capture
    start_capture()
    print("This should appear in both terminal and response file")
    stop_capture()