# Update FlowLoom Dependencies with uv

Let input_args = "$ARGUMENTS"

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Update all dependencies to latest compatible versions
- If input_args is "lock": Only update the lock file
- If input_args is a package name: Update specific package

## Update Process

I'll help you update FlowLoom dependencies using uv.

@bash
# Ensure we're in the FlowLoom directory
if [ ! -f "pyproject.toml" ] || [ ! -d ".claude" ]; then
    echo "Error: Not in FlowLoom root directory"
    exit 1
fi

# Ensure uv is available
export PATH="$HOME/.local/bin:$PATH"

# Determine if we should use --active flag
ACTIVE_FLAG=""
if [ -n "$VIRTUAL_ENV" ]; then
    echo "Detected active virtual environment"
    ACTIVE_FLAG="--active"
fi

if [ -z "$input_args" ]; then
    echo "Updating all dependencies to latest compatible versions..."
    uv lock --upgrade
    uv sync --all-packages $ACTIVE_FLAG
elif [ "$input_args" = "lock" ]; then
    echo "Updating lock file only..."
    uv lock
else
    echo "Updating specific package: $input_args"
    uv add "$input_args" --upgrade
fi

# Show updated packages
echo -e "\nCurrent package versions:"
uv pip list | grep -E "(flowloom|fastapi|click|watchdog|websockets)"

echo -e "\nUpdate complete!"

Show the user the update results and any version changes that occurred.