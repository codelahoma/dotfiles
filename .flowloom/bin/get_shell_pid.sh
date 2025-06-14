#!/bin/bash
# Get the proper shell PID by walking up the process tree

# Start with current process
CURRENT_PID=$$

# Walk up the process tree to find the shell
while [ $CURRENT_PID -gt 1 ]; do
    # Get process info
    if command -v ps >/dev/null 2>&1; then
        PROCESS_INFO=$(ps -p $CURRENT_PID -o comm= 2>/dev/null)
        
        # Check if this is a shell process
        case "$PROCESS_INFO" in
            *bash*|*zsh*|*fish*|*sh)
                echo $CURRENT_PID
                exit 0
                ;;
        esac
        
        # Get parent PID
        PARENT_PID=$(ps -p $CURRENT_PID -o ppid= 2>/dev/null | tr -d ' ')
        if [ -z "$PARENT_PID" ] || [ "$PARENT_PID" = "0" ]; then
            break
        fi
        CURRENT_PID=$PARENT_PID
    else
        # Fallback if ps is not available
        break
    fi
done

# Fallback to current process if no shell found
echo $$