# FlowLoom Footer Formatter

Let input_args = "$ARGUMENTS"

Generate a properly formatted interactive footer with automatic auto-tracking verification and memory logging prompts.

## Memory Logging Protocol

**BEFORE displaying footer, I must:**
1. **Log current session activities** to fl-memory.json via flowloom-memory
2. **Capture any decisions made** since last footer
3. **Document progress and discoveries** to basic-memory
4. **Update context and relationships** between entities

**Memory logging reminder text to include in my internal process:**
"🧠 MEMORY CHECK: Have I logged all activities, decisions, and discoveries since the last interaction? Use both fl-memory.json and basic-memory systems to capture this session's work."

## Auto-Track Verification

Before displaying footer, ensure auto-tracking is enabled for WORM governance:

slashload flowloom/system/auto-track enable

## Implementation

**STEP 1: Memory Logging Check**
Remind myself: "🧠 MEMORY CHECK: Have I logged all activities, decisions, and discoveries since the last interaction? Use both fl-memory.json and basic-memory systems to capture this session's work."

**STEP 2: Generate Footer**
Execute the Python footer script and display its output:

@bash python3 ./bin/footer.py --session "${input_args:-Configuration Mode Session}" --next-steps "1. Review output\\n2. Continue with next task"

**STEP 3: Output Sequencing**
CRITICAL OUTPUT HANDLING:
- Hold ALL output intended for user viewing until all tool calls complete
- Run the footer script and capture its output
- Append the footer output to any held content before displaying
- Display everything together as a single response

**STEP 4: Memory Status**
Add a brief memory status line showing:
- Last fl-memory update time
- Recent observations count
- Reminder about dual system tracking

**STEP 5: Display Complete Output**
Show the complete response with footer appended at the end.

**CRITICAL**: Do not continue working or take any actions after displaying the footer. The footer is an interactive pause point that requires user input to proceed.

The footer includes:
- **Working directory** (auto-detected)
- **Active mode** (auto-detected or "None")  
- **Session context** (from arguments)
- **Git branch** (auto-detected)
- **Next steps** (with usage hints for 'go' and numbered selection)

## Advanced Usage

The script supports manual overrides for all fields:

```bash
python3 ./bin/footer.py \
  --session "Custom session context" \
  --next-steps "1. First step\\n2. Second step\\n3. Third step" \
  --mode "Configuration Mode" \
  --working-dir "/custom/path" \
  --branch "feature-branch"
```

## Integration Notes

- Auto-detects working directory using `pwd`
- Auto-detects git branch using `git branch --show-current`
- Mode detection defaults to "None" (can be enhanced)
- Properly formats single vs multiple next steps
- Includes usage hints for interactive features