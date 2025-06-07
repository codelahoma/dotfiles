# FlowLoom Command System

## Command Syntax

FlowLoom supports two command syntaxes for convenience:

### Full Syntax
```
slashload category/command
```

### Shorthand Syntax
```
/category:command
/command  (when category is clear from context)
```

## Examples

| Shorthand | Full Syntax | Description |
|-----------|-------------|-------------|
| `/worker:status` | `slashload worker/status` | Check worker coordination status |
| `/coord:init` | `slashload coord/init` | Initialize coordination session |
| `/shell:id` | `slashload shell/id` | Manage shell ID with security consent |
| `/mode:workflow` | `slashload flowloom/mode/workflow` | Activate workflow mode |
| `/config:development` | `slashload flowloom/config/development` | Load development configuration |

## Command Categories

### Coordination Commands
- **`/worker:*`** - Worker role management
  - `/worker:announce` - Join coordination session
  - `/worker:status` - Check worker state
  - `/worker:complete` - Mark tasks complete

- **`/coord:*`** - Coordination session management
  - `/coord:init` - Initialize session
  - `/coord:status` - View session state
  - `/coord:dispatch` - Assign tasks
  - `/coord:cancel` - Cancel tasks
  - `/coord:mode` - Set coordination mode

- **`/shell:*`** - Shell session management
  - `/shell:id` - Manage shell ID with consent

### FlowLoom Commands
- **`/mode:*`** - Development modes
- **`/config:*`** - Configuration management
- **`/system:*`** - System utilities

## Usage Notes

1. **Shorthand Priority**: Use shorthand for frequently used commands
2. **Context Clarity**: When command name might be ambiguous, use full category
3. **Tab Completion**: Both syntaxes support tab completion
4. **Documentation**: All commands include help and examples

## Security & Consent

Commands that require system access (like shell ID detection) will:
1. Display security warnings
2. Request explicit user consent
3. Explain why access is needed
4. Respect Claude's security model

See `/shell:id` for an example of proper consent workflow.