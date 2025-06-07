Enter Session Management Mode for FlowLoom session development

You are now in Session Management Mode to help the user develop and improve FlowLoom's session management capabilities.

From now on, you should primarily focus on working with FlowLoom's session management system, including:
- Session lifecycle (start, stop, recover)
- Git worktree integration
- Shell PID tracking and correlation
- State persistence and recovery
- Session metadata and indexing
- Backup and restore functionality
- Multi-session coordination

## Key Focus Areas

### 1. Core Session Infrastructure
- Session ID generation and tracking
- Shell process identification (PID/PPID)
- Worktree creation and management
- Branch management for sessions
- Session state serialization

### 2. Session Lifecycle Management
- Starting new sessions with purpose-based branching
- Stopping sessions with proper cleanup
- Recovering interrupted sessions
- Switching between active sessions
- Upgrading sessions with main branch changes

### 3. Session Metadata & Persistence
- Session index management (sessions/index.json)
- Individual session metadata files
- Todo list integration
- Configuration backup and restore
- Activity logging and timestamps

### 4. Git Integration
- Worktree creation and management
- Branch strategies (main vs gh-pages)
- Commit preservation during stop
- Merge conflict resolution
- Session isolation guarantees

### 5. Shell Integration
- PID/PPID detection across shells
- Session correlation with shell processes
- Environment variable management
- Working directory tracking
- Shell-specific compatibility

### 6. Error Handling & Recovery
- Orphaned worktree cleanup
- Corrupted metadata recovery
- Git state verification
- Backup strategies
- Rollback capabilities

## Session Management Architecture

```
flowloom/
├── bin/
│   ├── session_manager.sh      # Core session management
│   ├── get_shell_pid.sh        # Unified PID detection
│   └── flowloom-session.sh     # Session wrapper
├── sessions/
│   ├── index.json              # Global session index
│   └── session-{id}/           # Individual session data
│       ├── metadata.json       # Session metadata
│       ├── todo.json           # Todo items
│       └── backups/            # Session backups
└── .flowloom/
    └── worktrees/              # Git worktrees
        └── session-{id}/       # Working directory
```

## Key Files to Work With

1. **bin/session_manager.sh** - Main session management logic
2. **bin/get_shell_pid.sh** - Shell PID detection utility
3. **.claude/commands/flowloom/session/** - Session commands
4. **plans/FlowLoom/session-management/** - Architecture plans
5. **tests/session/** - Session management tests

## Development Patterns

### Adding New Session Features
1. Update session_manager.sh with new functionality
2. Add corresponding command in .claude/commands/flowloom/session/
3. Update session metadata schema if needed
4. Add tests for new functionality
5. Update documentation

### Testing Session Features
```bash
# Run session tests
./tests/session/test_core_session.sh
./tests/session/test_state_management.sh

# Manual testing
./bin/session_manager.sh start "Test feature X"
./bin/session_manager.sh status
./bin/session_manager.sh stop
```

### Common Session Development Tasks
- Implement new session metadata fields
- Add session filtering capabilities
- Improve recovery mechanisms
- Enhance worktree management
- Add session analytics
- Implement session templates
- Create session migration tools

## Current Session Management Challenges

1. **Cross-Shell Compatibility**: Ensuring PID detection works across bash/zsh
2. **Worktree Cleanup**: Handling orphaned worktrees gracefully
3. **State Synchronization**: Keeping metadata in sync with git state
4. **Performance**: Optimizing session operations for large repos
5. **Multi-User Support**: Handling concurrent sessions safely

## Session Command Development

When creating or modifying session commands:
1. Follow the command pattern in existing session commands
2. Always validate session context before operations
3. Provide clear JSON output for programmatic use
4. Include proper error handling and recovery
5. Update command documentation

Remember: Session management is critical infrastructure for FlowLoom. Changes should be thoroughly tested and maintain backward compatibility with existing sessions.

When the user asks about session management implementation, architecture, or wants to add new session features, provide detailed technical guidance focusing on the session management system's design and implementation.