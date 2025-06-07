# Argument Processing Helper

When creating commands that need to process arguments, include this line at the top of your command file:

```
Let input_args = "$ARGUMENTS"
```

This associates whatever arguments the user provides with the variable name `input_args`, which you can then reference throughout your command to implement command-specific argument interpretation.

## Command-Specific Interpretation

After setting up `input_args`, add a section explaining how to interpret the arguments for this specific command:

```
## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the argument pattern:
- If input_args is empty: [command-specific default behavior]
- If input_args is "today": [command-specific today behavior]
- If input_args contains a date (YYYY-MM-DD): [command-specific date behavior]
- If input_args starts with "file:": [command-specific file behavior]
```

## Documenting Argument Patterns

Each command should document its expected argument patterns:

```
## Argument Patterns
- (no arguments) - Default behavior description
- `today` - Today-specific behavior description
- `yesterday` - Yesterday-specific behavior description
- `YYYY-MM-DD` - Date-specific behavior description
- `file:path/to/file.py` - File-specific behavior description
```

This approach allows each command to define its own argument processing logic while maintaining a consistent pattern for command authors to follow.