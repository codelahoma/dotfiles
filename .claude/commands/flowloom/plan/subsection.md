Let input_args = "$ARGUMENTS"

Create a detailed implementation plan for a subsection of a feature.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the argument pattern:
- If input_args is empty: Prompt to select from recent plans and subsection
- If input_args contains a subsection description: Use that description and find parent plan
- If input_args contains a plan filename: Use that specific plan as parent
- If input_args contains a JIRA ID: Find plans for that ticket

## Argument Patterns
- (no arguments) - Interactive selection from recent plans
- `authentication module` - Create subsection for auth module
- `100_AUP-1234_architecture_auth.md database` - Use specific plan, focus on database subsection
- `AUP-1234 payment processing` - Find plans for ticket, focus on payment processing
- `interactive` - Force interactive selection mode

## Step 1: Determine the Parent Plan

Based on input_args:

If input_args is empty or contains "interactive":
- Prompt to select one of the ten most recent plan documents using `${FLOWLOOM_WORK_DIR:-.meta-claude}/bin/recent_plans`
- Use the selected plan as the parent plan

If input_args contains a specific plan filename:
- Use that plan as the parent plan

If input_args contains a JIRA ID:
- Find the most recent plan for that ticket

If input_args contains a subsection description:
- Check memory for the most recently accessed plan
- If found, use that as the parent plan
- If not found, prompt to select a plan using `${FLOWLOOM_WORK_DIR:-.meta-claude}/bin/recent_plans`

## Step 2: Analyze the Parent Plan and Create Subsection

Examine the parent plan to determine:
1. The JIRA ticket ID
2. If it's a detailed implementation plan (120_*.md)
3. What specific subsection needs further detailed planning

Then, determine the appropriate numbering:

1. Check for existing subsection plans by running:
   ```bash
   ls ${FLOWLOOM_WORK_DIR:-.meta-claude}/plans/[JIRA-ID]/12*.md
   ```

2. Follow the plan file naming convention defined in `${FLOWLOOM_WORK_DIR:-.meta-claude}/docs/plan_file_naming_convention.md`:
   - If there are FEWER than 9 existing subsection plans:
     - Use the pattern: `12[1-9]_[JIRA-ID]_subsection_[descriptive-name].md`
     - Use the next available number (121-129)
   - If there are already 9 subsection plans:
     - Check if a "continued" plan exists (`130_*.md`)
     - If not, create one using `130_[JIRA-ID]_implementation_continued_[descriptive-name].md`
     - Then use the pattern: `13[1-9]_[JIRA-ID]_subsection_[descriptive-name].md`
   - Place it in the appropriate JIRA-ID subdirectory of `${FLOWLOOM_WORK_DIR:-.meta-claude}/plans/`

3. Create a subsection plan with the following structure:

# [TICKET-ID] Subsection Implementation Plan: [Subsection Name]

## Overview

This document outlines a detailed implementation plan for the [subsection name] component of the [parent feature/task]. It provides focused implementation instructions for this specific subsection.

## Parent Plan Reference

This subsection plan extends the following parent plan:
- [Full path to parent plan]

## Purpose

This subsection implementation aims to:

1. **[Primary Goal]** - Brief description of main objective
2. **[Secondary Goal]** - Brief description of secondary objective
3. **[Additional Goals]** - Any other key objectives

## Prerequisites

Before starting this subsection implementation:

- [ ] [Prerequisite 1] - Description of prerequisite
- [ ] [Prerequisite 2] - Description of prerequisite
- [ ] [Prerequisite 3] - Description of prerequisite

## Implementation Plan

The implementation will be structured in logical steps, each with detailed instructions.

### Task 1: [Task Name]

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

### Task 2: [Task Name]

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

## Implementation Process

Each task should follow this standard process:

1. **Development:**
   - Create new files or modify existing files according to the implementation plan
   - Implement all required functionality and tests
   - Ensure code follows project standards including type annotations and docstrings

2. **Testing:**
   - Write comprehensive unit tests for each component following Django-tenants requirements
   - ALL database objects must be created in fixtures with tenant parameter
   - Use create → yield → delete pattern for proper cleanup
   - Run tests using `script/test <path_to_test_file>` to verify functionality
   - NEVER run pytest directly - use script/test for tenant compatibility
   - Use TenantClient for API testing with proper tenant context
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
   - Update the parent plan to reference this subsection plan's completion
   - Add commit hash and message to the implementation plan for reference

## Integration with Parent Plan

This subsection implementation fits into the parent plan as follows:

1. **Dependencies on other components:**
   - [Dependency 1] - Brief description
   - [Dependency 2] - Brief description

2. **Components that depend on this subsection:**
   - [Dependent 1] - Brief description
   - [Dependent 2] - Brief description

3. **Integration points:**
   - [Integration point 1] - Brief description
   - [Integration point 2] - Brief description

## Conclusion

This subsection implementation plan provides detailed guidance for implementing the [subsection name] component of the [parent feature/task]. By following this plan, the subsection will integrate seamlessly with the overall implementation while maintaining code quality and project standards.

4. Store the current plan in memory for future reference.

Create it in the appropriate ${FLOWLOOM_WORK_DIR:-.meta-claude}/plans subdirectory.