# List Available Workers

Display all available workers in the coordination session.

## Usage:
```
/coord:workers [--status STATUS]
# Shorthand: /coord:workers
# Full: slashload coord/workers
```

Where:
- --status: Filter by worker status (available, busy, offline)

@python
from flowloom_memory_monitor import find_available_workers, search_entities

# Parse arguments
status_filter = None

import sys
args = sys.argv[1:] if len(sys.argv) > 1 else []

i = 0
while i < len(args):
    if args[i] == "--status" and i + 1 < len(args):
        status_filter = args[i + 1]
        i += 2
    else:
        i += 1

print("👥 Worker Status Report")
print("======================")
print("")

try:
    if status_filter:
        workers = search_entities(entity_type="Worker", search_term=f"status: {status_filter}")
        print(f"Workers with status '{status_filter}':")
    else:
        workers = search_entities(entity_type="Worker")
        print("All registered workers:")
    
    print("")
    
    if not workers:
        print("🚫 No workers found")
        if status_filter:
            print(f"   No workers with status '{status_filter}'")
        else:
            print("   No workers have been registered yet")
        print("")
        print("💡 To register workers:")
        print("   1. Start Claude in another terminal/session")
        print("   2. Run: /worker:register")
        print("   3. Run: /worker:wait (to listen for tasks)")
        print("")
    else:
        # Display worker details
        available_count = 0
        busy_count = 0
        offline_count = 0
        
        for i, worker in enumerate(workers, 1):
            name = worker.get('name', 'Unknown')
            observations = worker.get('observations', [])
            
            # Extract status
            status = "unknown"
            capabilities = "unknown"
            shell_pid = "unknown"
            last_heartbeat = "unknown"
            
            for obs in observations:
                if obs.startswith('status:'):
                    status = obs.split(':', 1)[1].strip()
                elif obs.startswith('capabilities:'):
                    capabilities = obs.split(':', 1)[1].strip()
                elif obs.startswith('shell_pid:'):
                    shell_pid = obs.split(':', 1)[1].strip()
                elif obs.startswith('last_heartbeat:'):
                    last_heartbeat = obs.split(':', 1)[1].strip()
            
            # Count by status
            if status == "available":
                available_count += 1
                status_icon = "✅"
            elif status == "busy":
                busy_count += 1
                status_icon = "🔄"
            else:
                offline_count += 1
                status_icon = "❌"
            
            print(f"{status_icon} {name}")
            print(f"   Status: {status}")
            print(f"   Shell PID: {shell_pid}")
            print(f"   Capabilities: {capabilities}")
            print(f"   Last Heartbeat: {last_heartbeat}")
            if i < len(workers):
                print("")
        
        print("📊 Summary:")
        print(f"   Available: {available_count}")
        print(f"   Busy: {busy_count}")
        print(f"   Offline: {offline_count}")
        print(f"   Total: {len(workers)}")
        print("")
        
        if available_count > 0:
            print("💡 Ready for task dispatch:")
            print("   /coord:dispatch 'task description' --type research --priority high")
        else:
            print("⚠️  No available workers for task dispatch")
            print("   Wait for workers to become available or register new ones")

except Exception as e:
    print(f"❌ Error listing workers: {e}")
    print("")
    print("🔧 Troubleshooting:")
    print("   - Check that memory.json exists and is accessible")
    print("   - Ensure flowloom-memory-monitor package is installed")
    print("   - Verify workers have been registered")
@

List and display status of all workers in the coordination session.