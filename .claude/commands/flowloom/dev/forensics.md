# Permission Forensics Command

This command helps gather information when Claude Code denies permission for a requested operation.

## Usage
Run this immediately after denying a permission request to capture forensic data.

## Information to Gather
1. **What command was attempted**: User provides the exact command that was blocked
2. **Permission prompt details**: What permission was Claude asking for?
3. **Context**: What was the goal/purpose of the blocked command?
4. **Workaround discovered**: Did we find an alternative approach?

## Process
Ask user for:
- The blocked command: `$ARGUMENTS`
- Permission type requested by Claude Code
- The intended goal
- Any error messages shown

Then document this in the knowledge base for future reference.

## Documentation Pattern
Create note: "Claude Code Permission Block: [command]" with:
- Command attempted
- Permission type
- Use case context  
- Alternative solutions found
- Date/session info

This builds a database of Claude Code's security boundaries to improve future command design.