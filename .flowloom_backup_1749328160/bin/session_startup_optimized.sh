#!/bin/bash
# session_startup_optimized.sh - High-performance session initialization
# Optimizes FlowLoom session startup time and performance

set -euo pipefail

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Performance tracking
START_TIME=$(date +%s.%N)

# Configuration
ENABLE_PERFORMANCE_MONITORING=${FLOWLOOM_ENABLE_PERFORMANCE:-true}
PERFORMANCE_INTERVAL=${FLOWLOOM_PERFORMANCE_INTERVAL:-30}
ENABLE_MEMORY_OPTIMIZATION=${FLOWLOOM_ENABLE_MEMORY_OPT:-true}
AUTO_CLEANUP_SESSIONS=${FLOWLOOM_AUTO_CLEANUP:-true}

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging function
log() {
    local level="$1"
    shift
    local message="$*"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    
    case "$level" in
        INFO)
            echo -e "${GREEN}[INFO]${NC} ${timestamp} - ${message}"
            ;;
        WARN)
            echo -e "${YELLOW}[WARN]${NC} ${timestamp} - ${message}"
            ;;
        ERROR)
            echo -e "${RED}[ERROR]${NC} ${timestamp} - ${message}"
            ;;
        DEBUG)
            if [[ "${FLOWLOOM_DEBUG:-false}" == "true" ]]; then
                echo -e "${BLUE}[DEBUG]${NC} ${timestamp} - ${message}"
            fi
            ;;
    esac
}

# Performance measurement
measure_time() {
    local start_time="$1"
    local end_time=$(date +%s.%N)
    local duration=$(echo "$end_time - $start_time" | bc -l)
    echo "${duration}s"
}

# Pre-flight checks
preflight_checks() {
    log INFO "Running pre-flight checks..."
    local check_start=$(date +%s.%N)
    
    # Check if we're in a FlowLoom project
    if [[ ! -f "$PROJECT_ROOT/memory.json" ]]; then
        log WARN "No memory.json found, creating minimal structure"
        echo '{"type": "entity", "name": "FlowLoom Project", "entityType": "Project", "observations": ["Initialized with optimized session startup"]}' > "$PROJECT_ROOT/memory.json"
    fi
    
    # Check sessions directory
    local sessions_dir="$PROJECT_ROOT/sessions"
    if [[ ! -d "$sessions_dir" ]]; then
        log INFO "Creating sessions directory"
        mkdir -p "$sessions_dir"
    fi
    
    # Check .flowloom directory structure
    local flowloom_dir="$PROJECT_ROOT/.flowloom"
    mkdir -p "$flowloom_dir/sessions" "$flowloom_dir/performance" "$flowloom_dir/indexes"
    
    # Check Python dependencies
    if ! python3 -c "import psutil" 2>/dev/null; then
        log WARN "psutil not installed, performance monitoring may be limited"
    fi
    
    local check_time=$(measure_time "$check_start")
    log INFO "Pre-flight checks completed in $check_time"
}

# Memory optimization
optimize_memory() {
    if [[ "$ENABLE_MEMORY_OPTIMIZATION" != "true" ]]; then
        return 0
    fi
    
    log INFO "Running memory optimization..."
    local opt_start=$(date +%s.%N)
    
    # Check if memory file needs optimization
    local memory_file="$PROJECT_ROOT/memory.json"
    local memory_size=$(stat -f%z "$memory_file" 2>/dev/null || echo "0")
    
    # Only optimize if file is larger than 10KB
    if [[ "$memory_size" -gt 10240 ]]; then
        log INFO "Memory file is ${memory_size} bytes, running optimization"
        
        if python3 "$SCRIPT_DIR/optimize_memory.py" --memory-file "$memory_file" analyze >/dev/null 2>&1; then
            # Run quick optimization
            python3 "$SCRIPT_DIR/optimize_memory.py" --memory-file "$memory_file" optimize >/dev/null 2>&1
            log INFO "Memory optimization completed"
        else
            log WARN "Memory optimization skipped (optimizer not available)"
        fi
    else
        log DEBUG "Memory file size OK ($memory_size bytes), skipping optimization"
    fi
    
    local opt_time=$(measure_time "$opt_start")
    log INFO "Memory optimization completed in $opt_time"
}

# Session cleanup
cleanup_old_sessions() {
    if [[ "$AUTO_CLEANUP_SESSIONS" != "true" ]]; then
        return 0
    fi
    
    log INFO "Cleaning up old sessions..."
    local cleanup_start=$(date +%s.%N)
    
    local sessions_dir="$PROJECT_ROOT/sessions"
    local cleaned_count=0
    local cutoff_time=$(date -d '7 days ago' +%s 2>/dev/null || date -v-7d +%s 2>/dev/null || echo "0")
    
    if [[ -d "$sessions_dir" ]]; then
        for session_dir in "$sessions_dir"/*; do
            if [[ -d "$session_dir" ]]; then
                local metadata_file="$session_dir/metadata.json"
                
                if [[ -f "$metadata_file" ]]; then
                    # Check last activity time
                    local last_activity=$(python3 -c "
import json, sys
try:
    with open('$metadata_file') as f:
        data = json.load(f)
    print(data.get('lastActivity', '1970-01-01'))
except:
    print('1970-01-01')
" 2>/dev/null)
                    
                    local activity_timestamp=$(date -d "$last_activity" +%s 2>/dev/null || echo "0")
                    
                    if [[ "$activity_timestamp" -lt "$cutoff_time" ]]; then
                        log DEBUG "Removing old session: $(basename "$session_dir")"
                        rm -rf "$session_dir"
                        ((cleaned_count++))
                    fi
                fi
            fi
        done
    fi
    
    local cleanup_time=$(measure_time "$cleanup_start")
    if [[ "$cleaned_count" -gt 0 ]]; then
        log INFO "Cleaned up $cleaned_count old sessions in $cleanup_time"
    else
        log DEBUG "No old sessions to clean up (completed in $cleanup_time)"
    fi
}

# Initialize performance indexes
initialize_indexes() {
    log INFO "Initializing performance indexes..."
    local index_start=$(date +%s.%N)
    
    # Build memory indexes for faster querying
    if python3 "$SCRIPT_DIR/optimize_memory.py" --memory-file "$PROJECT_ROOT/memory.json" index >/dev/null 2>&1; then
        log INFO "Performance indexes built successfully"
    else
        log WARN "Failed to build performance indexes"
    fi
    
    local index_time=$(measure_time "$index_start")
    log INFO "Index initialization completed in $index_time"
}

# Start background performance monitoring
start_performance_monitoring() {
    if [[ "$ENABLE_PERFORMANCE_MONITORING" != "true" ]]; then
        return 0
    fi
    
    log INFO "Starting background performance monitoring..."
    
    # Check if monitor is already running
    local monitor_pid_file="$PROJECT_ROOT/.flowloom/performance/monitor.pid"
    
    if [[ -f "$monitor_pid_file" ]]; then
        local existing_pid=$(cat "$monitor_pid_file")
        if kill -0 "$existing_pid" 2>/dev/null; then
            log INFO "Performance monitor already running (PID: $existing_pid)"
            return 0
        else
            log DEBUG "Removing stale monitor PID file"
            rm -f "$monitor_pid_file"
        fi
    fi
    
    # Start monitor in background
    if command -v python3 >/dev/null 2>&1; then
        nohup python3 "$SCRIPT_DIR/performance_monitor.py" \
            --project-root "$PROJECT_ROOT" \
            --interval "$PERFORMANCE_INTERVAL" \
            monitor --duration 1440 > "$PROJECT_ROOT/.flowloom/performance/monitor.log" 2>&1 &
        
        local monitor_pid=$!
        echo "$monitor_pid" > "$monitor_pid_file"
        log INFO "Performance monitor started (PID: $monitor_pid)"
    else
        log WARN "Python3 not available, skipping performance monitoring"
    fi
}

# Set up shell integration
setup_shell_integration() {
    log INFO "Setting up shell integration..."
    local shell_start=$(date +%s.%N)
    
    # Export FlowLoom environment variables
    export FLOWLOOM_PROJECT_ROOT="$PROJECT_ROOT"
    export FLOWLOOM_SESSIONS_DIR="$PROJECT_ROOT/sessions"
    export FLOWLOOM_MEMORY_FILE="$PROJECT_ROOT/memory.json"
    
    # Get shell PID for session tracking
    local shell_pid
    if shell_pid=$("$SCRIPT_DIR/get_shell_pid.sh" 2>/dev/null); then
        export FLOWLOOM_SHELL_PID="$shell_pid"
        log DEBUG "Shell PID detected: $shell_pid"
    else
        log WARN "Failed to detect shell PID, using fallback"
        export FLOWLOOM_SHELL_PID="$$"
    fi
    
    # Set up command aliases for performance
    alias fl-status='python3 -m flowloom_session.cli status'
    alias fl-start='python3 -m flowloom_session.cli start'
    alias fl-stop='python3 -m flowloom_session.cli stop'
    alias fl-list='python3 -m flowloom_session.cli list'
    alias fl-perf='python3 '"$SCRIPT_DIR"'/performance_monitor.py status'
    alias fl-optimize='python3 '"$SCRIPT_DIR"'/optimize_memory.py full'
    
    log DEBUG "Shell aliases configured"
    
    local shell_time=$(measure_time "$shell_start")
    log INFO "Shell integration completed in $shell_time"
}

# Main optimization routine
main() {
    log INFO "🚀 Starting FlowLoom optimized session initialization"
    
    # Set working directory
    cd "$PROJECT_ROOT"
    
    # Run optimization steps
    preflight_checks
    optimize_memory
    cleanup_old_sessions
    initialize_indexes
    setup_shell_integration
    start_performance_monitoring
    
    # Calculate total startup time
    local total_time=$(measure_time "$START_TIME")
    
    log INFO "✅ FlowLoom session initialization completed in $total_time"
    
    # Show current status
    if [[ "${FLOWLOOM_SHOW_STATUS:-true}" == "true" ]]; then
        echo ""
        echo "🔧 FlowLoom Status:"
        echo "  Project Root:    $PROJECT_ROOT"
        echo "  Shell PID:       ${FLOWLOOM_SHELL_PID:-unknown}"
        echo "  Sessions Dir:    ${FLOWLOOM_SESSIONS_DIR}"
        echo "  Performance:     ${ENABLE_PERFORMANCE_MONITORING}"
        echo "  Memory Size:     $(stat -f%z "$PROJECT_ROOT/memory.json" 2>/dev/null || echo "0") bytes"
        
        # Show session count
        local session_count=0
        if [[ -d "$PROJECT_ROOT/sessions" ]]; then
            session_count=$(find "$PROJECT_ROOT/sessions" -name "metadata.json" | wc -l | tr -d ' ')
        fi
        echo "  Active Sessions: $session_count"
        
        echo ""
        echo "Available commands:"
        echo "  fl-status    - Show session status"
        echo "  fl-start     - Start new session"
        echo "  fl-list      - List sessions"
        echo "  fl-perf      - Performance monitor"
        echo "  fl-optimize  - Optimize memory"
    fi
}

# Run if executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi