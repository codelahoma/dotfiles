# Add New Dependency to FlowLoom with uv

Let input_args = "$ARGUMENTS"

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is a package name: Add to root FlowLoom package
- If input_args contains "--package": Add to specific workspace package
- If input_args contains "--dev": Add as development dependency

## Add Dependency Process

I'll help you add a new dependency to FlowLoom using uv.

@bash
# Ensure we're in the FlowLoom directory
if [ ! -f "pyproject.toml" ] || [ ! -d ".claude" ]; then
    echo "Error: Not in FlowLoom root directory"
    exit 1
fi

# Ensure uv is available
export PATH="$HOME/.local/bin:$PATH"

# Parse arguments
if [ -z "$input_args" ]; then
    echo "Error: Please specify a package to add"
    echo "Usage: /flowloom:dev:add <package-name> [--package workspace-name] [--dev]"
    exit 1
fi

# Handle the add operation
if [[ "$input_args" == *"--package flowloom-web"* ]]; then
    package_name=$(echo "$input_args" | sed 's/--package flowloom-web//' | xargs)
    echo "Adding $package_name to flowloom-web..."
    cd flowloom-web && uv add $package_name
elif [[ "$input_args" == *"--package flowloom-session"* ]]; then
    package_name=$(echo "$input_args" | sed 's/--package flowloom-session//' | xargs)
    echo "Adding $package_name to flowloom-session..."
    cd packages/flowloom_session && uv add $package_name
else
    echo "Adding $input_args to root FlowLoom package..."
    uv add $input_args
fi

# Show updated dependencies
echo -e "\nUpdated dependencies:"
uv pip list | tail -20

echo -e "\nDependency added successfully!"

Show the user the results and confirm the package was added correctly.