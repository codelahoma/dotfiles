# FlowLoom Startup Context Command

## Description
Initializes FlowLoom AI session with comprehensive startup context for immediate productive work.

## Usage
```bash
# Full startup context (default)
/project:startup:context

# Quick essential context only  
/project:startup:context quick

# Recent session focus
/project:startup:context session

# Command reference summary
/project:startup:context commands
```

## Implementation
Delegates to stdlib for reusable context management, then displays footer:

/stdlib:startup-context $ARGUMENTS

After context loading completes, display the footer:

slashload flowloom/system/footer "Startup Context Loaded"

## Benefits
- **Consistent context gathering** across different commands
- **Reusable stdlib pattern** for other context-dependent features
- **Easier maintenance** and iteration on context logic
- **Composable architecture** for future FlowLoom capabilities
- **Immediate status display** with memory indicators and next steps

This command ensures FlowLoom AI can immediately resume productive work with full context awareness, preventing repetitive explanations and maintaining methodological consistency. The footer provides instant visibility into system status and memory tracking.