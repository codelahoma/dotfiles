Let message = "$ARGUMENTS"

# FlowLoom Timestamp Logger

Record timestamps for tracking build times and operation durations.

## Record Timestamp

@bash
# Get current UTC timestamp
timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
echo "⏰ [$timestamp] $message"

# Also save to temporary file for duration calculations
echo "$timestamp|$message" >> /tmp/flowloom-timestamps.log
@end

## Calculate Duration if End Marker

@bash
# Check if this is an end marker
if [[ "$message" == *"completed"* ]] || [[ "$message" == *"finished"* ]]; then
    # Get the last start timestamp
    if [ -f /tmp/flowloom-timestamps.log ]; then
        start_line=$(grep -E "(start|begin|Starting)" /tmp/flowloom-timestamps.log | tail -1)
        if [ ! -z "$start_line" ]; then
            start_time=$(echo "$start_line" | cut -d'|' -f1)
            
            # Calculate duration
            if [[ "$OSTYPE" == "darwin"* ]]; then
                # macOS
                start_seconds=$(date -j -f "%Y-%m-%dT%H:%M:%SZ" "$start_time" +%s 2>/dev/null)
                end_seconds=$(date -j -f "%Y-%m-%dT%H:%M:%SZ" "$timestamp" +%s 2>/dev/null)
            else
                # Linux
                start_seconds=$(date -d "$start_time" +%s 2>/dev/null)
                end_seconds=$(date -d "$timestamp" +%s 2>/dev/null)
            fi
            
            if [ ! -z "$start_seconds" ] && [ ! -z "$end_seconds" ]; then
                duration=$((end_seconds - start_seconds))
                hours=$((duration / 3600))
                minutes=$(((duration % 3600) / 60))
                seconds=$((duration % 60))
                
                echo ""
                echo "⏱️  Duration: ${hours}h ${minutes}m ${seconds}s"
                echo ""
            fi
        fi
    fi
fi
@end

Display the timestamp information above.