---
title: FlowLoom Development Workflow Documentation
type: note
permalink: development/flow-loom-development-workflow-documentation
---

# FlowLoom Development Workflow Documentation

## Configuration Architecture

### Problem Solved
FlowLoom needed a way to develop its own configuration system without polluting the user's final configuration. The solution implements a clean separation between development and user configurations.

### Implementation

#### 1. Development Configuration Structure
```
.claude/commands/flowloom/config/development.md
```
Contains all FlowLoom-specific development configuration including:
- Variable naming conventions
- Git commit formats
- Plan file naming conventions
- Tool technical quirks
- FlowLoom-specific features

#### 2. CLAUDE.local.md Pattern
```markdown
# FlowLoom Development Configuration
slashload flowloom/config/development
```
Uses slashload to include the development configuration instead of containing it directly.

#### 3. CLAUDE.md Separation
- **Development**: CLAUDE.md is .gitignored and contains minimal user template
- **Release**: Development config from slashload file gets merged to user's CLAUDE.local.md

### Benefits

1. **Clean Development**: Developers work with consistent FlowLoom configuration
2. **User-Ready Release**: End users get clean configuration without development artifacts
3. **Modular Maintenance**: FlowLoom config is maintainable in dedicated command files
4. **Version Control**: Development config is tracked, user config templates are not

### Usage Patterns

#### For FlowLoom Developers
1. Work with CLAUDE.local.md that slashloads development config
2. Modify `.claude/commands/flowloom/config/development.md` for config changes
3. CLAUDE.md stays untracked during development

#### For FlowLoom Users
1. Receive clean CLAUDE.md template that includes base_project.md
2. Add personal customizations below the base configuration
3. No development artifacts in their configuration

### Technical Implementation
- **Git ignore**: CLAUDE.md added to .gitignore
- **Slashload pattern**: Configuration loaded via command system
- **Modular structure**: Separate files for different configuration aspects
- **Release merge**: Development config can be merged to user templates

This pattern enables clean separation of concerns while maintaining a consistent development experience.