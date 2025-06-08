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
Delegates to stdlib for reusable context management:

/stdlib:startup-context $ARGUMENTS

## Benefits
- **Consistent context gathering** across different commands
- **Reusable stdlib pattern** for other context-dependent features
- **Easier maintenance** and iteration on context logic
- **Composable architecture** for future FlowLoom capabilities

This command ensures FlowLoom AI can immediately resume productive work with full context awareness, preventing repetitive explanations and maintaining methodological consistency.