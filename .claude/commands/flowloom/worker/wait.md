# Worker Wait for Tasks

Wait for task assignments using blocking mode (efficient worker pattern).

## Usage:
```
/worker:wait [--timeout TIMEOUT]
# Shorthand: /worker:wait
# Full: slashload worker/wait
```

Where:
- --timeout: Maximum wait time in seconds (default: 300)

@python
import os
import asyncio
import datetime
import json

# Parse arguments
timeout = 300

import sys
args = sys.argv[1:] if len(sys.argv) > 1 else []

i = 0
while i < len(args):
    if args[i] == "--timeout" and i + 1 < len(args):
        timeout = int(args[i + 1])
        i += 2
    else:
        i += 1

# Get worker ID
shell_pid = os.getpid()
worker_id = f"Worker-{shell_pid}"

print("⏳ Worker Task Monitor")
print("====================")
print("")
print(f"Worker ID: {worker_id}")
print(f"Timeout: {timeout}s")
print("")
print("🔍 Waiting for task assignments...")
print("   (Press Ctrl+C to stop)")
print("")

async def wait_for_tasks():
    try:
        from flowloom_memory_monitor import wait_for_worker_task
        
        # Wait for task assignment
        task = await wait_for_worker_task(worker_id, timeout=timeout)
        
        if task:
            print("🎯 Task Assignment Received!")
            print("=" * 40)
            print(f"Task ID: {task.get('name', 'Unknown')}")
            print(f"Type: {[obs for obs in task.get('observations', []) if obs.startswith('type:')]}")
            print(f"Description: {[obs for obs in task.get('observations', []) if obs.startswith('description:')]}")
            print(f"Priority: {[obs for obs in task.get('observations', []) if obs.startswith('priority:')]}")
            print("")
            print("💡 Next Steps:")
            print("   /worker:accept <task-id> - Accept this task")
            print("   /worker:status - Check current status")
            print("   Task processing workflow:")
            print("     1. Accept task")
            print("     2. Update progress")
            print("     3. Complete task")
            print("")
            
            return task
        else:
            print("⏰ No tasks received within timeout period")
            print("")
            print("💡 Options:")
            print("   /worker:wait - Wait again for more tasks")
            print("   /worker:status - Check worker status")
            print("   /coord:status - Check coordination session (from controller)")
            print("")
            return None
            
    except Exception as e:
        print(f"❌ Error waiting for tasks: {e}")
        print("")
        print("🔧 Troubleshooting:")
        print("   - Ensure worker is registered: /worker:register")
        print("   - Check memory.json exists and is accessible")
        print("   - Verify controller is creating tasks")
        return None

# Run the async function
try:
    task = asyncio.run(wait_for_tasks())
except KeyboardInterrupt:
    print("\n🛑 Task waiting cancelled by user")
    print("   Worker remains registered and available")
except Exception as e:
    print(f"\n❌ Unexpected error: {e}")
@

Efficiently wait for task assignments using memory-monitor blocking mode.