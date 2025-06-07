---
title: FlowLoom Teammate Briefing - Current Status
type: note
permalink: team/flow-loom-teammate-briefing-current-status
---

# FlowLoom Teammate Briefing - Current Status

**Date**: May 27, 2025  
**Prepared for**: Atlas UP development teammates  
**Current State**: Ready for internal collaboration

## What FlowLoom Is

FlowLoom is a **Claude Code development framework** that transforms AI-assisted development from ad-hoc interactions into systematic, reproducible workflows. Think of it as a sophisticated wrapper around Claude Code that adds:

- **Persistent memory** across sessions
- **Multi-agent coordination** capabilities  
- **Comprehensive installation system**
- **Advanced workflow automation**

## Current Capabilities

### ✅ **Working Systems**
1. **Complete Installer**: Multiple modes (interactive, unattended, development) with security templates
2. **Dual Memory System**: 
   - Structured memory (`memory.json`) for entities/relationships
   - Knowledge base (`.knowledge/`) for documentation and decisions
3. **Session Management**: Git worktree isolation with shell PID tracking
4. **MCP Integration**: 8 servers (filesystem, git, GitHub, web, database, etc.)
5. **Command System**: 24+ organized slash commands in `.claude/commands/`
6. **Cross-platform Support**: macOS and Linux with bash 3.2+ compatibility

### 🚧 **In Development**
1. **Multi-agent coordination** (architecture complete, implementation in progress)
2. **Docker isolation** (working prototype, needs production hardening)
3. **Advanced workflow orchestration** (framework exists, needs expansion)

## Repository Structure

```
flowloom/
├── installer/              # Complete installation system
│   ├── lib/                # Installation modules
│   ├── templates/          # Configuration templates
│   └── tests/              # Installation testing
├── .claude/                # Command system (24+ commands)
├── .knowledge/             # Markdown knowledge base
├── plans/                  # Hierarchical planning documents
├── docs/                   # Comprehensive documentation
├── bin/                    # Utility scripts
└── sessions/               # Active session workspaces (gitignored)
```

## Getting Started for Teammates

### 1. Quick Setup
```bash
# Clone and install FlowLoom
git clone <repository-url>
cd flowloom
./install-flowloom.sh --dev-mode

# Start Claude Code with FlowLoom
claude
```

### 2. Key Commands to Know
- `/session:start "description"` - Start isolated development session
- `/flowloom:memory:track-progress` - Record development progress
- `/flowloom:plan:review` - Review current planning documents
- `/project:git:sync` - Comprehensive git and config sync

### 3. Development Workflow
1. **Session-based development**: Each major feature gets its own session
2. **Memory tracking**: AI maintains context across sessions and team members
3. **Planning-driven**: Use hierarchical numbering (000-400 phases)
4. **Knowledge documentation**: Capture decisions and rationale

## Current Development Focus

### Immediate Priorities
1. **Multi-agent coordination implementation** (Plans 100-series)
2. **Docker security hardening** (Plans 1200-series)  
3. **Session management refinement** (Plans 3000-series)

### Architecture Decisions Made
- **MCP-first approach**: Leverage Model Context Protocol for extensibility
- **Dual memory strategy**: Structured + narrative knowledge systems
- **Session isolation**: Git worktrees for parallel development
- **Security by design**: Granular permissions and user consent

## Working with FlowLoom

### Memory System Usage
- **Entities**: Track components, features, decisions
- **Relationships**: Map dependencies and connections
- **Observations**: Record progress with shell PID correlation
- **Cross-reference**: Link structured memory with knowledge base articles

### Planning System
- **Phase-based numbering**: 000 (vision) → 100 (design) → 200 (implementation) → 300 (validation) → 400 (release)
- **Hierarchical organization**: Plans in `plans/FlowLoom/` by category
- **Documentation-driven**: Every significant decision gets a plan

### Session Management  
- **Shell PID tracking**: Consistent identification across memory and sessions
- **Git worktree isolation**: Each session gets separate workspace
- **Context preservation**: AI remembers project state across restarts

## Known Issues & Considerations

### Security Note
- **Critical discovery**: Identified unrestricted filesystem access vulnerability in Claude Code's built-in tools
- **Documented**: Security analysis in `.knowledge/Security/`
- **Mitigation**: Use MCP filesystem tools instead of Read/LS when possible

### Development Notes
- **FlowLoom is recursive**: We're using Claude Code + FlowLoom to build FlowLoom
- **Self-improving**: Each development session enhances the framework
- **Documentation-heavy**: Every decision and pattern gets captured

## Questions for Teammates

1. **Development focus**: Which components interest you most?
2. **Use cases**: What specific workflows would you like to enable?
3. **Architecture feedback**: Any concerns about current design decisions?
4. **Testing priorities**: Which areas need the most validation?

FlowLoom represents a new paradigm for AI-assisted development. We're not just building a tool—we're creating a framework for systematic AI collaboration that could transform how development teams work with AI assistants.