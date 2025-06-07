#!/usr/bin/env python3
"""
FlowLoom Footer Formatter
Generates consistent interactive footer with proper formatting.
Future: Will be called from post-interaction hook.
"""

import os
import sys
import subprocess
import argparse
from datetime import datetime
from pathlib import Path


class FooterFormatter:
    def __init__(self):
        self.working_dir = ""
        self.active_mode = ""
        self.session_context = ""
        self.session_id = ""
        self.git_branch = ""
        self.next_steps = ""
        self.claude_model = ""
        self.demo_mode = False
        
    def detect_git_branch(self):
        """Detect current git branch."""
        try:
            result = subprocess.run(
                ["git", "rev-parse", "--git-dir"],
                capture_output=True,
                check=True
            )
            
            result = subprocess.run(
                ["git", "branch", "--show-current"],
                capture_output=True,
                text=True,
                check=True
            )
            return result.stdout.strip() or "detached"
        except subprocess.CalledProcessError:
            return "no-git"
    
    def detect_current_session(self):
        """Detect current session ID."""
        script_path = Path(__file__).parent / "get_current_session.sh"
        if script_path.exists() and script_path.is_file():
            try:
                result = subprocess.run(
                    [str(script_path)],
                    capture_output=True,
                    text=True,
                    check=True
                )
                return result.stdout.strip()
            except subprocess.CalledProcessError:
                pass
        return "no-session"
    
    def detect_active_mode(self):
        """Detect active FlowLoom mode."""
        # For now, default to "None" - could be enhanced to detect actual modes
        return "None"
    
    def detect_claude_model(self):
        """Detect active Claude model."""
        # Note: Claude can directly provide model information
        return "ask-claude-directly"
    
    def get_shell_pid(self):
        """Get current shell PID."""
        script_path = Path(__file__).parent / "get_shell_pid.sh"
        if script_path.exists():
            try:
                result = subprocess.run(
                    [str(script_path)],
                    capture_output=True,
                    text=True,
                    check=True
                )
                return result.stdout.strip()
            except subprocess.CalledProcessError:
                pass
        return str(os.getppid())  # Fallback to parent PID
    
    def capture_to_memory(self):
        """Capture footer context to memory."""
        shell_pid = self.get_shell_pid()
        timestamp = datetime.now().strftime("%Y-%m-%dT%H:%M:%SZ")
        
        memory_observation = (
            f"Shell_ID: {shell_pid} - FOOTER: {timestamp} | "
            f"Context: {self.session_context} | Branch: {self.git_branch} | "
            f"Mode: {self.active_mode} | Next: {self.formatted_next_steps}"
        )
        
        session_entity = f"Session-{shell_pid}-footer-{int(datetime.now().timestamp())}"
        
        try:
            result = subprocess.run([
                "python3",
                str(Path(__file__).parent / "log_observation.py"),
                "--memory-file", "fl-memory.json",
                "add-entity", session_entity, "Session", memory_observation
            ], capture_output=True, check=True)
            
            # Create a brief description of what was captured
            context_brief = self.session_context[:30] + "..." if len(self.session_context) > 30 else self.session_context
            return f"✅ Memory captured: session state, {context_brief}"
        except subprocess.CalledProcessError:
            return "⚠️ Memory capture failed"
    
    def format_next_steps(self, next_steps):
        """Format next steps with proper numbering."""
        if not next_steps:
            return "", "**Next steps:**"
        
        # Handle both literal \n and actual newlines
        if "\\n" in next_steps or "\n" in next_steps:
            # Multiple steps
            steps = next_steps.replace("\\n", "\n").split("\n")
            steps = [step.strip() for step in steps if step.strip()]
            
            # Check if already numbered
            if not any(step.startswith(("1.", "2.", "3.")) for step in steps):
                # Auto-number steps
                formatted_steps = []
                for i, step in enumerate(steps, 1):
                    formatted_steps.append(f"{i}. {step}")
                formatted = "\n".join(formatted_steps)
            else:
                # Already numbered
                formatted = "\n".join(steps)
            
            label = "**Next steps (type 'go' for all, or numbers like '1 3'):**"
        else:
            # Single step
            formatted = next_steps
            label = "**Next steps:**"
        
        return formatted, label
    
    def get_demo_showcase(self):
        """Get a rotating FlowLoom capability showcase for demo mode."""
        import hashlib
        import time
        
        showcases = [
            "🧠 **FlowLoom Memory**: I maintain persistent context across sessions and coordinate with other Claude instances through our shared memory graph.",
            
            "🔄 **Multi-Claude Coordination**: I can work with multiple Claude instances simultaneously, sharing context and coordinating complex development workflows.",
            
            "📋 **Smart Task Management**: I automatically break down complex requests into tracked todos and provide clear progress updates throughout implementation.",
            
            "🎯 **WORM Governance**: I automatically capture all development interactions for compliance, creating an immutable audit trail of every decision and change.",
            
            "⚡ **Permission Prompt Avoidance**: I use advanced techniques like file-based input and direct tool usage to minimize interruptions and maintain flow state.",
            
            "🔧 **Auto-Installation**: I can install myself into any project with a single command, setting up complete development workflows automatically.",
            
            "📊 **Session Awareness**: I track shell contexts, git branches, and development modes to provide situational awareness across your entire workflow.",
            
            "🚀 **Hook Architecture**: I integrate with your development process through pre/post-interaction hooks, capturing context without disrupting your flow.",
            
            "🎨 **Rich Formatting**: I preserve complex markdown formatting perfectly, ensuring professional-quality documentation and responses with proper headers, lists, and code blocks.",
            
            "🌊 **Flow State**: I'm designed to enhance your development flow, not interrupt it - working as your intelligent development partner."
        ]
        
        # Use session context + current day for interesting rotation
        context_hash = abs(hash(self.session_context)) if self.session_context else 0
        day_of_year = int(time.strftime("%j"))
        showcase_index = (context_hash + day_of_year) % len(showcases)
        
        return showcases[showcase_index]
    
    def generate_footer(self):
        """Generate the complete footer."""
        # Set defaults for optional parameters
        if not self.working_dir:
            self.working_dir = os.getcwd()
        
        if not self.active_mode:
            self.active_mode = self.detect_active_mode()
        
        if not self.session_id:
            self.session_id = self.detect_current_session()
        
        if not self.git_branch:
            self.git_branch = self.detect_git_branch()
        
        if not self.claude_model:
            self.claude_model = self.detect_claude_model()
        
        # Format next steps
        self.formatted_next_steps, next_steps_label = self.format_next_steps(self.next_steps)
        
        # Capture to memory
        memory_status = self.capture_to_memory()
        
        # Generate footer
        demo_section = ""
        if self.demo_mode:
            demo_showcase = self.get_demo_showcase()
            demo_section = f"\n**✨ FlowLoom Showcase:** {demo_showcase}\n"
        
        footer = f"""---

**Working directory:** {self.working_dir}
**Active mode:** {self.active_mode}
**Session context:** {self.session_context}
**Session ID:** {self.session_id}
**Git branch:** {self.git_branch}
**Claude model:** {self.claude_model}
**Memory status:** {memory_status}
{next_steps_label}
{self.formatted_next_steps}{demo_section}
<!-- CLAUDE: Stop here and wait for user input. Do not continue working or take actions after displaying this footer. This is an interactive pause point. REMEMBER: Always show footer at the end of EVERY interaction. -->"""

        return footer


def main():
    parser = argparse.ArgumentParser(
        description="FlowLoom Footer Formatter - Generates consistent interactive footer",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s -s "Working on footer script" -n "1. Test script\\n2. Commit changes"
  %(prog)s --session "Bug fixing" --next-steps "Fix remaining issues"
  %(prog)s --session "Demo session" --next-steps "Show capabilities" --demo-mode
  
Next steps format:
  - Single step: "Complete the implementation"
  - Multiple steps: "1. First step\\n2. Second step\\n3. Third step"

Demo mode:
  Adds rotating FlowLoom capability showcases to footer for demonstrations.
  Showcases rotate based on session context and day for consistent variety.
        """
    )
    
    parser.add_argument("-w", "--working-dir", 
                       help="Set working directory (default: current pwd)")
    parser.add_argument("-m", "--mode", dest="active_mode",
                       help="Set active mode (default: detect or 'None')")
    parser.add_argument("-s", "--session", dest="session_context", required=True,
                       help="Set session context (required)")
    parser.add_argument("-b", "--branch", dest="git_branch",
                       help="Set git branch (default: auto-detect)")
    parser.add_argument("-i", "--session-id", 
                       help="Set session ID (default: auto-detect)")
    parser.add_argument("-n", "--next-steps", required=True,
                       help="Set next steps (required)")
    parser.add_argument("--model", dest="claude_model",
                       help="Set Claude model (default: auto-detect)")
    parser.add_argument("--demo-mode", action="store_true",
                       help="Enable demo mode with FlowLoom capability showcase")
    
    args = parser.parse_args()
    
    # Create formatter and set values
    formatter = FooterFormatter()
    formatter.working_dir = args.working_dir or ""
    formatter.active_mode = args.active_mode or ""
    formatter.session_context = args.session_context
    formatter.session_id = args.session_id or ""
    formatter.git_branch = args.git_branch or ""
    formatter.next_steps = args.next_steps
    formatter.claude_model = args.claude_model or ""
    formatter.demo_mode = args.demo_mode
    
    # Generate and print footer
    footer = formatter.generate_footer()
    print(footer)


if __name__ == "__main__":
    main()