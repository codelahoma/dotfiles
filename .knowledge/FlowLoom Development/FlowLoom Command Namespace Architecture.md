---
title: FlowLoom Command Namespace Architecture
type: note
permalink: flow-loom-development/flow-loom-command-namespace-architecture
---

# FlowLoom Command Namespace Architecture

## Critical Clarification: Namespace vs Template Directory

**IMPORTANT**: `.claude/commands/flowloom/` is NOT a template directory - it's a **namespace directory** designed to prevent command conflicts when FlowLoom is installed.

## Namespace Structure Purpose

### Conflict Prevention
- FlowLoom commands are organized under `flowloom:*` namespace
- User commands can exist in `.claude/commands/` without conflicts
- Auto-detection patterns like `plan:vision` resolve to `flowloom/plan/vision`
- FlowLoom maintains its command structure when installed in different projects

### Directory Organization
```
.claude/commands/
├── flowloom/              # FlowLoom namespace (NOT templates)
│   ├── plan/             # Planning commands
│   ├── docs/             # Documentation commands  
│   ├── impl/             # Implementation commands
│   ├── mode/             # Mode commands
│   └── system/           # System commands
└── [user-commands]/      # User's custom commands (no conflicts)
```

### Command Resolution
- `plan:vision` → resolves to `flowloom/plan/vision`
- `flowloom:plan:vision` → explicit namespace reference
- `project:plan:vision` → legacy support, resolves to `flowloom/plan/vision`

## Development Guidelines

### Creating New FlowLoom Commands
- **Always create under** `.claude/commands/flowloom/` namespace
- **Never create in** `.claude/commands/` root (reserved for user commands)
- Follow category structure: `flowloom/category/command.md`

### Installation Considerations
- FlowLoom commands remain isolated in their namespace
- Users can create their own commands without conflicts
- Clear separation between FlowLoom and user functionality

This namespace approach is fundamental to FlowLoom's non-intrusive installation strategy and command system architecture.