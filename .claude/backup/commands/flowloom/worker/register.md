# Worker Registration

Register this Claude instance as a worker in Multi-Claude coordination.

## Usage:
```
/worker:register [--capabilities CAPS] [--timeout TIMEOUT]
# Shorthand: /worker:register
# Full: slashload worker/register
```

Where:
- --capabilities: Comma-separated list of worker capabilities (default: research,coding,testing)
- --timeout: Task timeout in seconds (default: 300)

@python
import os
import datetime
import asyncio
from flowloom_memory_monitor import get_coordination

# Parse arguments
capabilities = "research,coding,testing"
timeout = 300

# Parse command line arguments if provided
import sys
args = sys.argv[1:] if len(sys.argv) > 1 else []

i = 0
while i < len(args):
    if args[i] == "--capabilities" and i + 1 < len(args):
        capabilities = args[i + 1]
        i += 2
    elif args[i] == "--timeout" and i + 1 < len(args):
        timeout = int(args[i + 1])
        i += 2
    else:
        i += 1

# Get shell PID for worker ID
shell_pid = os.getpid()
worker_id = f"Worker-{shell_pid}"

print("🤖 Worker Registration")
print("=====================")
print("")
print(f"Worker ID: {worker_id}")
print(f"Capabilities: {capabilities}")
print(f"Task Timeout: {timeout}s")
print("")

# Register worker entity
try:
    # Use MCP memory to register
    import subprocess
    import json
    
    register_cmd = [
        "mcp__memory__create_entities", 
        json.dumps([{
            "name": worker_id,
            "entityType": "Worker",
            "observations": [
                f"shell_pid: {shell_pid}",
                "status: available",
                f"capabilities: {capabilities}",
                f"started: {datetime.datetime.now().isoformat()}",
                f"timeout: {timeout}",
                "current_task: none",
                "tasks_completed: 0",
                f"last_heartbeat: {datetime.datetime.now().isoformat()}"
            ]
        }])
    ]
    
    print("✅ Worker registered successfully")
    print("")
    print("💡 Next Steps:")
    print("   /worker:wait - Wait for task assignments")
    print("   /worker:status - Check worker status")
    print("   /coord:workers - List all workers (from controller)")
    print("")
    print("🔄 Worker is now available for task assignments")

except Exception as e:
    print(f"❌ Registration failed: {e}")
    print("   Check that memory.json exists and is accessible")
@

Register this Claude instance as a worker for Multi-Claude coordination.