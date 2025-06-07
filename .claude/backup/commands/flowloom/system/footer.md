# FlowLoom Footer Formatter

Let input_args = "$ARGUMENTS"

Generate a properly formatted interactive footer with automatic auto-tracking verification.

## Usage

This command uses the footer formatting script to generate consistent footer display with automatic data detection and proper formatting.

## Auto-Track Verification

Before displaying footer, ensure auto-tracking is enabled for WORM governance:

slashload flowloom/system/auto-track enable

## Implementation

Execute the Python footer script and display its output:

@bash python3 ./bin/footer.py --session "${input_args:-Configuration Mode Session}" --next-steps "1. Review output\\n2. Continue with next task"

After running the bash command above, display all the output exactly as returned by the script. The output will be a formatted footer that you should show to the user.

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