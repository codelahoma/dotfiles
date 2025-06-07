#!/usr/bin/env python3
"""
FlowLoom Launcher - Python implementation

Validates FlowLoom projects and launches Claude Code with proper context.
"""

import sys
import os
import json
import subprocess
import argparse
from pathlib import Path
from typing import Optional, Dict, Any
import logging

# ANSI color codes
class Colors:
    RED = '\033[0;31m'
    GREEN = '\033[0;32m'
    YELLOW = '\033[1;33m'
    BLUE = '\033[0;34m'
    NC = '\033[0m'  # No Color

# FlowLoom project markers
FLOWLOOM_MARKERS = ["flowloom.json", "CLAUDE.md", "memory.json", ".knowledge"]

# Possible Claude executable locations
CLAUDE_PATHS = [
    "claude",  # In PATH
    "/usr/local/bin/claude",
    "/opt/homebrew/bin/claude",
    Path.home() / ".local/bin/claude",
    Path.home() / "Library/Application Support/Code/User/globalStorage/anthropic.claude-code/claude/claude",
]

class FlowLoomLauncher:
    def __init__(self, verbose: bool = False):
        self.setup_logging(verbose)
        self.project_root = None
        
    def setup_logging(self, verbose: bool):
        """Configure logging"""
        level = logging.DEBUG if verbose else logging.INFO
        logging.basicConfig(
            format='%(message)s',
            level=level
        )
        self.logger = logging.getLogger("flowloom")
        
    def print_color(self, color: str, message: str):
        """Print colored output"""
        print(f"{color}{message}{Colors.NC}")
        
    def find_project_root(self, start_path: Path = Path.cwd()) -> Optional[Path]:
        """Find FlowLoom project root by searching upward"""
        current = start_path.resolve()
        
        while current != current.parent:
            # Check for any FlowLoom marker
            for marker in FLOWLOOM_MARKERS:
                if (current / marker).exists():
                    return current
            current = current.parent
            
        return None
        
    def is_flowloom_project(self, path: Path) -> bool:
        """Check if directory is a FlowLoom project"""
        return any((path / marker).exists() for marker in FLOWLOOM_MARKERS)
        
    def find_claude(self) -> Optional[Path]:
        """Find Claude executable"""
        for claude_path in CLAUDE_PATHS:
            if isinstance(claude_path, str):
                # Check if it's in PATH
                result = subprocess.run(
                    ["which", claude_path],
                    capture_output=True,
                    text=True
                )
                if result.returncode == 0:
                    return Path(result.stdout.strip())
            else:
                # Check absolute path
                if claude_path.exists() and os.access(claude_path, os.X_OK):
                    return claude_path
                    
        return None
        
    def show_project_info(self, project_dir: Path):
        """Display project information"""
        self.print_color(Colors.BLUE, "FlowLoom Project Information:")
        print(f"  Directory: {project_dir}")
        
        # Project name from flowloom.json
        flowloom_json = project_dir / "flowloom.json"
        if flowloom_json.exists():
            try:
                with open(flowloom_json) as f:
                    config = json.load(f)
                    project_name = config.get("name", "Unnamed Project")
                    print(f"  Project: {project_name}")
            except:
                print("  Project: Unnamed Project")
                
        # Knowledge articles count
        knowledge_dir = project_dir / ".knowledge"
        if knowledge_dir.exists():
            article_count = len(list(knowledge_dir.glob("*.md")))
            print(f"  Knowledge articles: {article_count}")
            
        # Git branch
        result = subprocess.run(
            ["git", "branch", "--show-current"],
            cwd=project_dir,
            capture_output=True,
            text=True
        )
        if result.returncode == 0 and result.stdout.strip():
            print(f"  Git Branch: {result.stdout.strip()}")
            
        print()
        
    def run_startup_tasks(self, project_dir: Path):
        """Run startup tasks"""
        self.print_color(Colors.BLUE, "Running startup tasks...")
        
        # Import and run startup tasks if available
        startup_script = Path(__file__).parent / "flowloom-startup-tasks.sh"
        if startup_script.exists():
            result = subprocess.run(
                ["bash", str(startup_script), str(project_dir)],
                cwd=project_dir,
                env={**os.environ, "PROJECT_DIR": str(project_dir)}
            )
            if result.returncode != 0:
                self.logger.warning("Some startup tasks failed")
        else:
            # Basic startup tasks in Python
            self._ensure_directories(project_dir)
            self._check_memory_json(project_dir)
            
    def _ensure_directories(self, project_dir: Path):
        """Ensure required directories exist"""
        dirs = ["config", "plans", "docs", "bin"]
        for dir_name in dirs:
            (project_dir / dir_name).mkdir(exist_ok=True)
            
    def _check_memory_json(self, project_dir: Path):
        """Ensure memory.json exists"""
        memory_file = project_dir / "memory.json"
        if not memory_file.exists():
            memory_file.write_text("[]")
            print("  Initialized memory.json")
            
    def init_project(self):
        """Initialize new FlowLoom project"""
        self.print_color(Colors.YELLOW, "Initializing new FlowLoom project...")
        
        # Find installer
        installer_paths = [
            Path(__file__).parent.parent / "install-flowloom.sh",
            Path.cwd() / "install-flowloom.sh"
        ]
        
        for installer in installer_paths:
            if installer.exists():
                subprocess.run([str(installer)])
                return
                
        self.print_color(Colors.RED, "Error: FlowLoom installer not found")
        sys.exit(1)
        
    def launch(self, args: list = None):
        """Launch Claude in FlowLoom project"""
        # Find project root
        if self.is_flowloom_project(Path.cwd()):
            self.project_root = Path.cwd()
        else:
            self.project_root = self.find_project_root()
            
        if not self.project_root:
            self.print_color(Colors.YELLOW, "Not in a FlowLoom project directory")
            response = input("Initialize FlowLoom in current directory? (y/N) ")
            if response.lower() == 'y':
                self.init_project()
                self.project_root = Path.cwd()
            else:
                sys.exit(1)
                
        # Find Claude
        claude_path = self.find_claude()
        if not claude_path:
            self.print_color(Colors.RED, "Error: Claude Code not found")
            self.print_color(Colors.YELLOW, "Please install Claude Code from: https://claude.ai/download")
            sys.exit(1)
            
        # Change to project directory
        os.chdir(self.project_root)
        
        # Show project info
        self.show_project_info(self.project_root)
        
        # Run startup tasks
        self.run_startup_tasks(self.project_root)
        
        # Launch Claude
        self.print_color(Colors.GREEN, "Launching Claude Code...")
        self.print_color(Colors.BLUE, "Tip: Claude will read CLAUDE.md and CLAUDE.local.md for project context")
        print()
        
        # Execute Claude with any additional arguments
        claude_args = [str(claude_path)]
        if args:
            claude_args.extend(args)
            
        os.execv(str(claude_path), claude_args)
        
    def check(self):
        """Check if current directory is a FlowLoom project"""
        if self.is_flowloom_project(Path.cwd()):
            self.print_color(Colors.GREEN, "✓ FlowLoom project detected")
            self.show_project_info(Path.cwd())
        else:
            project_root = self.find_project_root()
            if project_root:
                self.print_color(Colors.YELLOW, f"FlowLoom project found at: {project_root}")
                self.show_project_info(project_root)
            else:
                self.print_color(Colors.RED, "✗ Not in a FlowLoom project")
                sys.exit(1)

def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description="FlowLoom - AI Development Framework for Claude Code",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Commands:
  launch, start   Launch Claude in the current FlowLoom project (default)
  init           Initialize FlowLoom in the current directory
  check          Check if current directory is a FlowLoom project
  help           Show this help message

Examples:
  flowloom                    # Launch Claude in current project
  flowloom init              # Initialize new FlowLoom project
  flowloom check             # Verify FlowLoom setup
  flowloom launch --help     # Pass arguments to Claude

For more information: https://github.com/codelahoma/flowloom
"""
    )
    
    parser.add_argument("command", nargs="?", default="launch",
                        choices=["launch", "start", "init", "check", "help"],
                        help="Command to run")
    parser.add_argument("-v", "--verbose", action="store_true",
                        help="Enable verbose output")
    parser.add_argument("args", nargs=argparse.REMAINDER,
                        help="Additional arguments to pass to Claude")
    
    args = parser.parse_args()
    
    # Handle help
    if args.command == "help":
        parser.print_help()
        return 0
        
    # Create launcher
    launcher = FlowLoomLauncher(verbose=args.verbose)
    
    # Execute command
    if args.command in ["launch", "start"]:
        launcher.launch(args.args)
    elif args.command == "init":
        launcher.init_project()
    elif args.command == "check":
        launcher.check()
        
    return 0

if __name__ == "__main__":
    sys.exit(main())