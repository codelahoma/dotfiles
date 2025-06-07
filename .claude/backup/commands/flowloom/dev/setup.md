# Initialize FlowLoom Development Environment with uv

Let input_args = "$ARGUMENTS"

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Standard setup
- If input_args is "fresh": Clean setup (remove existing venv)
- If input_args is "update": Update dependencies only

## Setup Process

I'll help you set up your FlowLoom development environment using uv.

@bash
# Ensure we're in the FlowLoom directory
if [ ! -f "pyproject.toml" ] || [ ! -d ".claude" ]; then
    echo "Error: Not in FlowLoom root directory"
    exit 1
fi

# Check if uv is installed
if ! command -v uv &> /dev/null; then
    echo "Installing uv..."
    curl -LsSf https://astral.sh/uv/install.sh | sh
    export PATH="$HOME/.local/bin:$PATH"
fi

# Clean setup if requested
if [ "$input_args" = "fresh" ]; then
    echo "Performing fresh setup..."
    rm -rf .venv uv.lock
fi

# Run uv sync to install all dependencies
echo "Installing FlowLoom dependencies with uv..."
# Use --active flag if VIRTUAL_ENV is set (e.g., direnv users)
if [ -n "$VIRTUAL_ENV" ]; then
    echo "Detected active virtual environment, using --active flag"
    uv sync --all-packages --active
else
    uv sync --all-packages
fi

# Show installed packages
echo -e "\nInstalled packages:"
uv pip list | head -20

echo -e "\nSetup complete! You can now:"
if [ -n "$VIRTUAL_ENV" ]; then
    echo "- Run 'uv run --active flowloom-web start' to start the web interface"
    echo "- Run 'uv run --active python -m memory_monitor' to use memory monitor"
else
    echo "- Run 'uv run flowloom-web start' to start the web interface"
    echo "- Run 'uv run python -m memory_monitor' to use memory monitor"
fi
echo "- Use 'uv add <package>' to add new dependencies"

Show the user the setup results and provide guidance on using uv for FlowLoom development.