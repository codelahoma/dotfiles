# Shell ID Management

Display or establish the shell ID for this Claude session with proper security consent.

## Usage:
```
/shell:id [--force] [--export]
# Shorthand: /shell:id
# Full: slashload shell/id
```

Where:
- --force: Force re-detection even if ID is already known
- --export: Output as export statement for environment variables

@bash
force_detection=false
export_format=false

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --force)
      force_detection=true
      shift
      ;;
    --export)
      export_format=true
      shift
      ;;
    *)
      echo "❌ Unknown option: $1"
      echo "Usage: /shell:id [--force] [--export]"
      exit 1
      ;;
  esac
done

echo "🆔 Shell ID Management"
echo "====================="
echo ""
@

Check if shell ID is already known in memory:

@memory
# Check if we already have a shell ID stored
shell_id_check=$(mcp__memory__search_nodes '{
  "query": "Shell ID"
}' 2>/dev/null || echo "")

if echo "$shell_id_check" | grep -q "Shell ID:" && [ "$force_detection" = false ]; then
  stored_shell_id=$(echo "$shell_id_check" | grep "Shell ID:" | head -1 | sed 's/.*Shell ID: //' | tr -d '"' | tr -d ' ')
  
  if [ -n "$stored_shell_id" ]; then
    echo "✅ Shell ID already established: $stored_shell_id"
    echo "   (Use --force to re-detect)"
    echo ""
    
    if [ "$export_format" = true ]; then
      echo "export SHELL_PID=$stored_shell_id"
    else
      echo "$stored_shell_id"
    fi
    exit 0
  fi
fi
@

Display security warning and get consent:

@bash
echo "⚠️  SECURITY & PERMISSION NOTICE"
echo "================================"
echo ""
echo "FlowLoom needs to detect your shell process ID (PID) for coordination features."
echo ""
echo "WHY THIS IS NEEDED:"
echo "- Enables multi-Claude session coordination"
echo "- Provides unique identifiers for worker/controller roles"
echo "- Allows proper session tracking and memory correlation"
echo "- Prevents conflicts between multiple Claude instances"
echo ""
echo "HOW WE RESPECT CLAUDE'S SECURITY MODEL:"
echo "- Uses only standard process inspection tools (ps, pstree)"
echo "- No privileged access or system modification"
echo "- Read-only process information only"
echo "- Transparent detection methods documented"
echo ""
echo "DETECTION METHODS:"
echo "1. Environment variables (CLAUDE_PPID if set)"
echo "2. Process tree walk to find shell that launched Claude"
echo "3. Standard pstree analysis (if available)"
echo "4. PPID fallback for subprocess detection"
echo ""
echo "Your consent is required each time Claude restarts as we cannot cache"
echo "permission decisions across sessions."
echo ""
echo -n "Proceed with shell PID detection? (y/N): "
read -r consent

if [[ "$consent" != "y" && "$consent" != "Y" ]]; then
  echo ""
  echo "❌ Shell PID detection cancelled by user."
  echo "   Some FlowLoom coordination features will be unavailable."
  exit 1
fi

echo ""
echo "✅ Proceeding with shell PID detection..."
echo ""
@

Perform shell ID detection and store result:

@bash
# Use the centralized detection script with warning bypassed
SKIP_SECURITY_WARNING=true shell_pid=$(./bin/get_shell_pid.sh)

echo "🔍 Detection Results:"
echo "   Shell PID: $shell_pid"
echo "   Detection Time: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
echo ""
@

Store shell ID in memory for future reference:

@memory
# Store shell ID in memory for future sessions
mcp__memory__create_entities '{
  "entities": [
    {
      "name": "FlowLoom Shell Session",
      "entityType": "ShellSession",
      "observations": [
        "Shell ID: '${shell_pid}'",
        "Detection Time: $(date -u +\"%Y-%m-%dT%H:%M:%SZ\")",
        "Detection Method: Centralized script with user consent",
        "User Consent: Granted on $(date -u +\"%Y-%m-%dT%H:%M:%SZ\")",
        "Security Notice: Displayed and acknowledged",
        "Shell_ID: '${shell_pid}' - $(date -u +\"%Y-%m-%dT%H:%M:%SZ\") | Shell ID established with user consent"
      ]
    }
  ]
}'
@

Display final result:

@bash
echo "✅ Shell ID established: $shell_pid"
echo ""
echo "This ID will be used for:"
echo "  ✓ Coordination session management"
echo "  ✓ Worker/controller identification"
echo "  ✓ Memory observation tagging"
echo "  ✓ Cross-session tracking"
echo ""

if [ "$export_format" = true ]; then
  echo "Export command:"
  echo "export SHELL_PID=$shell_pid"
else
  echo "Shell ID: $shell_pid"
fi

echo ""
echo "💡 Next Steps:"
echo "   /worker:announce - Join coordination as worker"
echo "   /coord:init - Start coordination as controller"
echo "   /shell:id --export - Get export command for scripts"
@

Establish shell ID with proper security consent, store in memory, and provide for coordination features.