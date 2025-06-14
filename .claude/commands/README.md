# FlowLoom Command System

## Command Syntax

FlowLoom supports multiple command syntaxes for convenience:

### Full Syntax
```
slashload category/command
```

### Shorthand Syntax
```
/category:command
/command  (when category is clear from context)
```

### Fuzzy Match Shortcuts
```
/stat     → /system/status
/cont     → /startup/context  
/com      → /flowloom/commands
```

## Essential Commands (Ultra-Short)

| Short | Full Command | Description |
|-------|--------------|-------------|
| `/stat` | `/system/status` | System status (most important) |
| `/cont` | `/startup/context` | Load startup context |
| `/q` | `/q` | Quick help & command search |
| `/com` | `/flowloom/commands` | Command reference |
| `/as` | `/flowloom/app/status` | App status |
| `/cs` | `/flowloom/coord/status` | Coordination status |
| `/ds` | `/flowloom/docker/status` | Docker status |
| `/ps` | `/flowloom/plan/status` | Plan status |
| `/ss` | `/flowloom/session/status` | Session status |
| `/ws` | `/flowloom/worker/status` | Worker status |

## Category Prefixes

### Core Categories
- **`/c:`** - Coordination (`/c:init`, `/c:dispatch`, `/c:status`)
- **`/m:`** - Memory (`/m:query`, `/m:monitor`, `/m:track`)
- **`/p:`** - Planning (`/p:arch`, `/p:status`, `/p:review`)
- **`/s:`** - Session (`/s:start`, `/s:stop`, `/s:status`)
- **`/w:`** - Worker (`/w:register`, `/w:status`, `/w:complete`)

### Development Categories  
- **`/mo:`** - Mode (`/mo:pair`, `/mo:opus`, `/mo:story`)
- **`/d:`** - Docker (`/d:setup`, `/d:status`, `/d:logs`)
- **`/dv:`** - Dev Tools (`/dv:setup`, `/dv:add`, `/dv:sync`)
- **`/g:`** - Git (`/g:sync`, `/g:backup`, `/g:cleanup`)

### System Categories
- **`/sy:`** - System (`/sy:status`, `/sy:debug`, `/sy:reload`)
- **`/u:`** - UI (`/u:start`, `/u:stop`, `/u:toggle`)

## Quick Reference by Workflow

### Starting Work Session
```bash
/cont           # Load startup context
/s:st           # Start session  
/mo:p           # Enter pair mode
```

### Status Checks
```bash
/stat           # System status
/as             # App status
/cs             # Coordination status  
/ds             # Docker status
```

### Planning & Architecture
```bash
/p:a            # Architecture planning
/p:h            # High-level planning
/p:s            # Plan status
/p:r            # Plan review
```

### Coordination Workflow
```bash
/c:i            # Initialize coordination
/c:d task       # Dispatch task
/c:s            # Check coordination status
/c:w            # List workers
```

### Memory & Context
```bash
/m:q term       # Query memory
/m:m            # Monitor memory
/m:c            # Create session
/m:t            # Track progress
```

## Fuzzy Matching Rules

1. **Start with "/" for shortcuts**
2. **Minimal disambiguation** - Use shortest unique prefix
3. **Category prefixes** when conflicts exist
4. **Common abbreviations** (stat, cont, etc.)
5. **Status priority** - `/stat` reserved for system status

## 🔍 Getting Help

- `/q` → Quick help and command overview
- `/q <term>` → Search commands (e.g., `/q status`, `/q coord`, `/q docker`)
- `/com` → Full command reference
- Individual commands include usage examples

## Examples

| Shorthand | Full Syntax | Fuzzy Short | Description |
|-----------|-------------|-------------|-------------|
| `/worker:status` | `slashload worker/status` | `/ws` | Check worker coordination status |
| `/coord:init` | `slashload coord/init` | `/c:i` | Initialize coordination session |
| `/shell:id` | `slashload shell/id` | `/sh:id` | Manage shell ID with security consent |
| `/mode:workflow` | `slashload flowloom/mode/workflow` | `/mo:w` | Activate workflow mode |
| `/config:development` | `slashload flowloom/config/development` | `/co:d` | Load development configuration |

## Security & Consent

Commands that require system access (like shell ID detection) will:
1. Display security warnings
2. Request explicit user consent
3. Explain why access is needed
4. Respect Claude's security model

See `/shell:id` for an example of proper consent workflow.

## Complete Command Categories

### Coordination Commands
- **`/worker:*`** - Worker role management
  - `/worker:announce` (`/w:a`) - Join coordination session
  - `/worker:status` (`/ws`) - Check worker state
  - `/worker:complete` (`/w:c`) - Mark tasks complete

- **`/coord:*`** - Coordination session management
  - `/coord:init` (`/c:i`) - Initialize session
  - `/coord:status` (`/cs`) - View session state
  - `/coord:dispatch` (`/c:d`) - Assign tasks
  - `/coord:cancel` (`/c:c`) - Cancel tasks
  - `/coord:mode` (`/c:m`) - Set coordination mode

### FlowLoom Commands
- **`/mode:*`** - Development modes (`/mo:*`)
- **`/config:*`** - Configuration management (`/co:*`)
- **`/system:*`** - System utilities (`/sy:*`)
- **`/plan:*`** - Planning and architecture (`/p:*`)
- **`/memory:*`** - Memory and context management (`/m:*`)

## Usage Notes

1. **Fuzzy Priority**: Use ultra-short forms for most common commands
2. **Category Clarity**: Use prefixes when command name might be ambiguous  
3. **Tab Completion**: All syntaxes support tab completion
4. **Documentation**: All commands include help and examples
5. **Memorization**: Learn `/stat` and `/cont` first, then category patterns