---
title: FlowLoom Standard Library Concept
type: note
permalink: architecture/flow-loom-standard-library-concept
tags:
- '#stdlib'
- '#prompts'
- '#architecture'
- '#slashload'
---

# FlowLoom Standard Library Concept

## Overview

A collection of private, reusable prompts that FlowLoom commands can slashload internally to perform common operations with consistent behavior.

## Proposed Structure

```
.claude/commands/flowloom/stdlib/
├── analysis/
│   ├── code-review.md          # Standardized code review process
│   ├── dependency-check.md     # Analyze dependencies
│   └── security-scan.md        # Security analysis patterns
├── generation/
│   ├── commit-message.md       # Generate semantic commits
│   ├── documentation.md        # Create consistent docs
│   └── test-cases.md          # Generate test scenarios
├── memory/
│   ├── entity-creator.md       # Consistent entity creation
│   ├── relation-builder.md     # Build entity relationships
│   └── session-tracker.md      # Track session work
├── parsing/
│   ├── argument-parser.md      # Parse command arguments
│   ├── error-extractor.md     # Extract error patterns
│   └── diff-analyzer.md        # Analyze git diffs
├── formatting/
│   ├── footer-builder.md       # Build consistent footers
│   ├── table-formatter.md      # Format data as tables
│   └── progress-reporter.md    # Report task progress
└── validation/
    ├── permission-checker.md   # Validate permissions
    ├── path-validator.md       # Validate file paths
    └── config-validator.md     # Validate configurations
```

## Example Usage

### In a FlowLoom command:
```markdown
# Generate commit message using stdlib
slashload flowloom/stdlib/generation/commit-message $DIFF_OUTPUT

# Parse complex arguments
slashload flowloom/stdlib/parsing/argument-parser "$ARGUMENTS"

# Create memory entities consistently
slashload flowloom/stdlib/memory/entity-creator "component" "SessionManager" "Manages work sessions"
```

## Key Benefits

1. **Consistency**: All commands use same patterns
2. **Reusability**: Write once, use everywhere
3. **Maintainability**: Update behavior in one place
4. **Quality**: Well-tested, refined prompts
5. **Abstraction**: Hide complexity from command authors

## Implementation Patterns

### Input/Output Contracts
Each stdlib prompt should define:
- Expected input format
- Output format
- Error handling
- Example usage

### Versioning
- Semantic versioning for stdlib
- Backwards compatibility
- Deprecation notices

### Testing
- Test cases for each stdlib function
- Integration tests with commands
- Performance benchmarks

## Proposed Initial Set

### High Priority
1. `commit-message` - Analyze changes, generate message
2. `entity-creator` - Standardize memory entity creation
3. `argument-parser` - Parse complex command arguments
4. `error-handler` - Consistent error reporting
5. `progress-tracker` - Track multi-step operations

### Medium Priority
1. `code-reviewer` - Standardized review process
2. `test-generator` - Create test cases
3. `doc-builder` - Generate documentation
4. `diff-analyzer` - Understand code changes
5. `session-tracker` - Track session work

### Future Additions
1. `dependency-analyzer` - Understand project deps
2. `security-scanner` - Check for vulnerabilities  
3. `performance-analyzer` - Profile code
4. `refactoring-assistant` - Suggest improvements
5. `migration-helper` - Assist upgrades

## Integration Points

### Command Development
When creating new commands:
```markdown
# Instead of writing custom logic
@task Parse user arguments and extract flags

# Use stdlib
slashload flowloom/stdlib/parsing/argument-parser "$ARGUMENTS"
Let parsed_args = [output from above]
```

### Memory Operations
Standardize all memory operations:
```markdown
# Don't create entities manually
# Use the stdlib for consistency
slashload flowloom/stdlib/memory/entity-creator \
  "type:$ENTITY_TYPE" \
  "name:$ENTITY_NAME" \
  "observations:$OBSERVATIONS"
```

### Error Handling
Consistent error reporting:
```markdown
# When an error occurs
slashload flowloom/stdlib/formatting/error-reporter \
  "error:$ERROR_MESSAGE" \
  "context:$OPERATION" \
  "suggestions:$RECOVERY_STEPS"
```

## Development Guidelines

### Creating Stdlib Prompts
1. Single responsibility
2. Clear input/output
3. Error handling
4. Examples included
5. Performance considered

### Documentation
Each stdlib prompt needs:
- Purpose statement
- Input parameters
- Output format
- Usage examples
- Error scenarios

### Quality Standards
- Peer review required
- Test coverage
- Performance benchmarks
- User feedback incorporated

This stdlib approach would make FlowLoom more powerful and consistent while reducing duplicate prompt engineering effort across commands.