---
title: 'Master-Agent Mode: Foreground/Background Claude Coordination'
type: note
permalink: flow-loom/technical/master-agent-mode-foreground/background-claude-coordination
---

# Master-Agent Mode: Foreground/Background Claude Coordination

## Concept: Explicit Master/Agent Roles

Beyond automatic last-joined coordination, implement a mode where one Claude instance explicitly becomes the master session, with others operating as background agents.

## Use Cases

### Development Workflow:
- **Master**: Interactive session with user, coordinating overall work
- **Agent 1**: Running tests continuously in background
- **Agent 2**: Monitoring file changes, updating documentation
- **Agent 3**: Code analysis, security scanning

### Research & Analysis:
- **Master**: Main conversation thread with user
- **Background Agents**: Parallel research, data gathering, validation
- **Coordination**: Master delegates tasks, agents report findings

### Long-Running Tasks:
- **Master**: Handles user interaction, quick responses
- **Agents**: Execute time-intensive operations (builds, deployments, analysis)
- **Status Updates**: Agents push progress to shared state

## Implementation Design

### Mode Activation:
```bash
# Become master (claims coordination)
/flowloom:mode:master

# Join as background agent
/flowloom:mode:agent

# Query current coordination state
/flowloom:system:coordination-status
```

### Coordination Mechanisms:

#### Task Queue System:
- Master pushes tasks to shared queue (memory/filesystem)
- Agents poll for available work
- Results pushed back to shared state

#### Status Broadcasting:
- Agents periodically update status
- Master aggregates and presents to user
- Real-time progress visibility

#### Resource Management:
- Prevent conflicts (file locks, git operations)
- Coordinate shared resources (databases, external APIs)
- Graceful handoff when master changes

## Technical Architecture

### Shared State:
```
.flowloom-coordination/
├── master.lock          # Current master session ID
├── agents/              # Active agent registrations
│   ├── agent-001.json   # Agent capabilities, status
│   └── agent-002.json
├── tasks/               # Task queue
│   ├── pending/
│   ├── active/
│   └── completed/
└── status/              # Real-time status updates
    └── current.json
```

### Agent Capabilities:
- **Specialized roles**: test-runner, doc-writer, analyzer, monitor
- **Resource requirements**: filesystem, git, external APIs
- **Availability status**: busy, idle, error, offline

## User Experience

### Master Session:
```
User: "Run tests while I work on the UI"
Master: "Delegating test execution to Agent-1..."
[Agent-1 starts continuous testing]
Master: "You can continue working. I'll notify you of test results."
```

### Background Coordination:
```
Master: "Agent-2 is updating documentation..."
Master: "Agent-1 reports: 3 tests failing in auth module"
Master: "All agents idle. Ready for next task."
```

## Implementation Priority

### Phase 1: Basic Master/Agent Mode
- Simple master election
- Task delegation via shared files
- Status reporting

### Phase 2: Specialized Agents
- Agent capability registration
- Resource conflict resolution
- Advanced task routing

### Phase 3: Intelligent Coordination
- AI-driven task distribution
- Predictive resource allocation
- Auto-scaling agent deployment

## Integration with Last-Joined Pattern

- **Default**: Last-joined becomes master automatically
- **Override**: Explicit master mode takes precedence
- **Graceful transition**: Current master can delegate to new instance
- **Fallback**: If master disconnects, agents auto-elect new master

This creates a flexible hierarchy: automatic coordination for simple cases, explicit control for complex workflows.