Create detailed implementation plan from high-level architecture
Let input_args = "$ARGUMENTS"

Create a detailed implementation plan based on a selected high-level plan.

## Argument Interpretation
First, analyze the provided arguments: input_args

**When no arguments provided (input_args is empty):**
Offer the user these options:
1. **Use current context** - Generate detailed plan based on current session context and recent activity
2. **Select from recent plans** - Choose from the 10 most recent high-level plans
3. **Specify component** - Enter a component name (e.g., "multi-claude-coordination", "installer-system")
4. **Specify plan file** - Enter a specific plan filename

Present these options clearly and wait for user choice.

**When arguments provided:**
- Plan filename: Use that specific plan
- Component name: Find and use the most recent high-level plan for that component
- "interactive": Force interactive selection mode

## Argument Patterns
- (no arguments) - Offer user choice of context options
- `140_FlowLoom_architecture_greenfield-coordination-system.md` - Use specific plan file
- `multi-claude-coordination` - Use most recent high-level plan for that component
- `interactive` - Force interactive selection from recent plans

## User Interaction Process

When input_args is empty, present this menu:
```
Create Detailed Implementation Plan

Choose your approach:
1. Use current context (based on: [describe current session context])
2. Select from recent high-level plans
3. Specify component name
4. Specify plan filename

What would you like to do? (1-4): 
```

Then proceed based on user selection.

Then create a detailed implementation plan based on the selected high level plan.

Follow FlowLoom's plan file naming convention:
- Use the pattern: `[number]_[component]_implementation_[descriptive-name].md`
- Number it appropriately in the sequence of existing plan files
- Place it in the appropriate component subdirectory of `plans/FlowLoom/`

The detailed plan should follow this structure:

# [TICKET-ID] Implementation Plan

## Overview

This document outlines a detailed implementation plan for [feature/task description]. It provides a structured approach to implementing the required functionality while maintaining code quality and following project standards.

## Purpose

This implementation aims to:

1. **[Primary Goal]** - Brief description of main objective
2. **[Secondary Goal]** - Brief description of secondary objective
3. **[Additional Goals]** - Any other key objectives

## Prerequisites

Before starting the implementation:

- [ ] [Prerequisite 1] - Description of prerequisite
- [ ] [Prerequisite 2] - Description of prerequisite
- [ ] [Prerequisite 3] - Description of prerequisite

## Implementation Plan

The implementation will be structured in phases, with each phase containing discrete tasks that can be implemented and committed independently.

### Phase 1: [Phase Name]

#### Task 1.1: [Task Name]

**Status:** 📝 PLANNED

**Purpose:** Brief description of the task's purpose and how it contributes to the overall implementation.

**Implementation Checklist:**
- [ ] [Subtask 1] - Brief description
- [ ] [Subtask 2] - Brief description
- [ ] [Subtask 3] - Brief description

**Reference Implementation:**
```python
# Include reference implementation if applicable
# This serves as a guide for the actual implementation
# Can include pseudocode or actual code
```

**Implementation Notes:**
<!-- To be filled in after implementation -->
<!-- Include key decisions, any deviations from the plan, and important details about the implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->
<!-- List tests added following Django-tenants requirements: -->
<!-- - All fixtures include tenant parameter -->
<!-- - Proper create → yield → delete pattern -->
<!-- - Tests run with script/test command -->
<!-- - TenantClient used for API tests -->

**Commit:** <!-- To be filled in after implementation - include commit hash and message -->

#### Task 1.2: [Task Name]

**Status:** 📝 PLANNED

**Purpose:** Brief description of the task's purpose.

**Implementation Checklist:**
- [ ] [Subtask 1] - Brief description
- [ ] [Subtask 2] - Brief description
- [ ] [Subtask 3] - Brief description

**Reference Implementation:**
```python
# Include reference implementation if applicable
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->
<!-- List tests added following Django-tenants requirements: -->
<!-- - All fixtures include tenant parameter -->
<!-- - Proper create → yield → delete pattern -->
<!-- - Tests run with script/test command -->
<!-- - TenantClient used for API tests -->

**Commit:** <!-- To be filled in after implementation -->

### Phase 2: [Phase Name]

#### Task 2.1: [Task Name]

**Status:** 📝 PLANNED

**Purpose:** Brief description of the task's purpose.

**Implementation Checklist:**
- [ ] [Subtask 1] - Brief description
- [ ] [Subtask 2] - Brief description
- [ ] [Subtask 3] - Brief description

**Reference Implementation:**
```python
# Include reference implementation if applicable
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

## Implementation Process

Each task should follow this standard process:

1. **Development:**
   - Create new files or modify existing files according to the implementation plan
   - Implement all required functionality and tests
   - Ensure code follows project standards including type annotations and docstrings

2. **Testing:**
   - Write comprehensive unit tests for each component
   - Run tests using `script/test <path_to_test_file>` to verify functionality
   - Aim for high test coverage for new components

3. **Linting and Commit:**
   - Stage changes: `git add <changed_files>`
   - Run linting: `script/run_pre_commit_no_mypy <changed_files>`
   - Fix any linting issues identified
   - Commit with descriptive message following the project conventions:
     ```
     git commit -m "feat(TICKET-ID): Implement <component_name>
     
     <Brief description of what was implemented and why>
     
     🤖 Generated with [Claude Code](https://claude.ai/code)
     
     Co-Authored-By: Claude <noreply@anthropic.com>"
     ```

4. **Documentation and Knowledge Management:**
   - Update the implementation plan with notes on what was implemented
   - Mark the task as completed using standardized completion markers:
     - **✅ COMPLETE** - For fully completed tasks
     - **✅ IMPLEMENTATION COMPLETE** - For completed implementation work  
     - **🔄 IN PROGRESS** - For actively worked tasks
     - Change **Status:** from **📝 PLANNED** to **✅ COMPLETE**
   - Update the memory tool with details about:
     - New components and their relationships
     - Implementation patterns to remember
     - Key decisions made during implementation
   - Add commit hash and message to the implementation plan for reference

This process ensures consistent code quality, comprehensive documentation, and a clean commit history throughout the implementation effort, while also maintaining shared knowledge about the implementation in the memory tool.

## Testing Strategy

Each phase should be accompanied by appropriate tests following Django-tenants requirements:

### Django-Tenants Testing Requirements

**CRITICAL:** All database object creation MUST happen in fixtures, never in test functions.

1. **Unit Tests:** Test each component in isolation with comprehensive input variations
   - Create all test data in fixtures with `tenant` parameter
   - Follow create → yield → delete pattern for cleanup
   - Use `script/test <path_to_test_file>` to run tests

2. **Integration Tests:** Ensure components work together correctly
   - Use proper fixture dependencies with tenant context
   - Delete objects in reverse dependency order
   - Test with TenantClient for API interactions

3. **End-to-End Tests:** Verify full functionality operates as expected with real-world scenarios
   - Always run with `script/test atlas_up` for full test suite
   - Never run pytest directly due to schema context requirements

### Required Fixture Pattern

All fixtures creating database objects must follow this pattern:
```python
@pytest.fixture
def test_[model_name](tenant):  # tenant parameter required
    obj = ModelName.objects.create(field="value")
    yield obj
    obj.delete()  # Critical cleanup step
```

See `${FLOWLOOM_WORK_DIR:-.meta-claude}/docs/testing_django_tenants.md` for comprehensive testing guidelines.

## Rollback Plan

In case of issues:

1. Keep the git tag before major changes begin if applicable
2. Implement changes incrementally with separate commits for each task
3. Run full test suite after each task
4. Revert to previous commit if issues are detected and cannot be immediately resolved

## Conclusion

This implementation plan provides a structured approach to delivering [feature/task description]. By following this plan, we can ensure the implementation is robust, maintainable, and meets all requirements.

Create it in the appropriate ${FLOWLOOM_WORK_DIR:-.meta-claude}/plans subdirectory.