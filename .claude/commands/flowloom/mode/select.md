Select and activate specialized FlowLoom working modes
Let input_args = "$ARGUMENTS"

## Mode Selection Assistant

FlowLoom provides specialized modes to help focus your workflow. You can activate any of these modes using the corresponding slash command, or continue working without a specific mode.

### Available Modes

#### Planning & Architecture
- **`/project:mode:opus`** - Deep planning mode for Claude Opus (planning only, no implementation)
- **`/project:mode:workflow`** - Comprehensive workflow assistance with task planning and execution
- **`/project:mode:specification`** - Requirements gathering and specification development

#### Development & Code Quality
- **`/project:mode:pair`** - Collaborative pair programming assistance  
- **`/project:mode:code_review`** - Code review and quality analysis
- **`/project:mode:files`** - File organization and structure management

#### Documentation & Communication
- **`/project:mode:documentation`** - Technical writing and documentation creation
- **`/project:mode:story`** - Narrative-driven development and storytelling

#### Configuration & Setup
- **`/project:mode:config`** - Claude command and configuration management

### How to Use Modes

1. **To activate a mode:** Simply use the slash command (e.g., `/project:mode:workflow`)
2. **To work without a mode:** Continue with your regular workflow - no mode selection required
3. **To switch modes:** Activate any other mode at any time

### Argument Interpretation

If input_args is provided:
- If input_args matches a mode name (e.g., "workflow", "config"), provide detailed information about that specific mode
- If input_args is "list" or empty, show the full mode selection above

**Note:** Mode selection is entirely optional. You can work effectively without selecting a mode, and modes can be switched at any time based on your current needs.

## Mode Recommendations

Based on your current task, consider:
- **Deep strategic planning with Claude Opus?** Try `/project:mode:opus`
- **Starting a new project?** Try `/project:mode:workflow` or `/project:mode:specification`
- **Writing or reviewing code?** Try `/project:mode:pair` or `/project:mode:code_review`
- **Organizing files?** Try `/project:mode:files`
- **Creating documentation?** Try `/project:mode:documentation`
- **Working on Claude commands?** Try `/project:mode:config`
- **Working on narrative content?** Try `/project:mode:story`