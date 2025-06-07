# View Worker Status

Display the current status of this worker including assignments, capabilities, and session information.

@bash
shell_pid=$(./bin/get_shell_pid.sh)
worker_id="worker_${shell_pid}"

echo "👤 Worker Status"
echo "================"
echo ""

# Check if worker is registered
worker_info=$(mcp__memory__search_nodes '{
  "query": "Claude Worker '${worker_id}'"
}')

if echo "$worker_info" | grep -q "ClaudeWorker"; then
  echo "🔍 Worker Information:"
  echo "$worker_info" | grep -E "(Worker ID|Role|Status|Session|Capabilities|Last Heartbeat)" | sed 's/^/   /'
  echo ""
  
  # Get session info
  session_id=$(echo "$worker_info" | grep "Session:" | sed 's/.*Session: //' | tr -d '"')
  if [ -n "$session_id" ]; then
    echo "📋 Session Information:"
    session_info=$(mcp__memory__search_nodes '{
      "query": "Coordination Session '${session_id}'"
    }')
    echo "$session_info" | grep -E "(Session ID|Status|Created)" | sed 's/^/   /'
    echo ""
  fi
  
  # Check current task assignment
  current_task=$(echo "$worker_info" | grep "Current Task:" | sed 's/.*Current Task: //' | tr -d '"')
  if [ "$current_task" != "none" ] && [ -n "$current_task" ]; then
    echo "📝 Current Task:"
    task_info=$(mcp__memory__search_nodes '{
      "query": "Task '${current_task}'"
    }')
    echo "$task_info" | grep -E "(Task ID|Description|Priority|Status)" | sed 's/^/   /'
    echo ""
  else
    echo "📝 Current Task: None (Available for assignment)"
    echo ""
  fi
  
  # Show recent completed tasks
  echo "📈 Recent Activity:"
  recent_tasks=$(mcp__memory__search_nodes '{
    "query": "Completed By: '${worker_id}'"
  }')
  
  if echo "$recent_tasks" | grep -q "Task"; then
    echo "$recent_tasks" | grep -A3 -B1 "Task" | grep -E "(name|Completion Time|Results)" | head -9 | sed 's/^/   /'
  else
    echo "   No completed tasks yet"
  fi
  
else
  echo "❌ Worker not registered in any coordination session"
  echo ""
  echo "Available commands:"
  echo "   /worker:announce [session_id] [capabilities] - Join a coordination session"
  echo ""
  echo "Active coordination sessions:"
  mcp__memory__search_nodes '{
    "query": "CoordinationSession"
  }' | grep -E "name.*Coordination Session" | sed 's/.*name": "Coordination Session /   Session: /' | sed 's/".*//'
fi

echo ""
echo "💡 Available Actions:"
echo "   /worker:announce [session_id] - Join coordination session"
echo "   /worker:complete [task_id] - Mark assigned task complete"
echo "   /coord:status - View overall coordination status (if controller)"
@

Display comprehensive worker status including registration, current assignments, session details, and recent activity for coordination awareness.