Complete task: stage, lint, commit changes, and update documentation
Let input_args = "$ARGUMENTS"

Wrap up your completed task by staging, linting, committing changes, and updating documentation.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the argument pattern:
- If input_args is empty: Complete the current task based on git status
- If input_args contains a JIRA ID: Use that specific ticket ID for commits
- If input_args contains "plan": Also update the specified plan file
- If input_args contains "quick": Skip detailed analysis and proceed directly to commit

## Argument Patterns
- (no arguments) - Complete current task based on git changes
- `AUP-1234` - Complete task using specific JIRA ticket ID
- `plan:filename` - Also update the specified plan file
- `quick` - Skip detailed analysis, proceed to commit
- `AUP-1234 plan:120_implementation.md` - Use ticket ID and update specific plan

Follow these steps to properly finish up your current task:

## Initial Analysis

Start by analyzing your current changes to determine:

1. What you have changed
2. Which changes belong together in logical commits
3. What tests you need to run
4. What plan updates you need to make

```bash
# Check your unstaged and uncommitted changes
git status
git diff --stat
```

## Grouping Changes

Group your related changes for proper commits. Based on your changes, determine:

1. Which files you should commit together
2. A logical grouping of changes if you need multiple commits
3. Appropriate commit messages for each group

## Linting and Tests

Before finalizing commits, complete these steps:

1. Run linting on your changed files
```
script/run_pre_commit_no_mypy <files>
```

2. Run tests to verify your changes
```
script/test <specific_test_files>
```

3. Run the full test suite to check for regressions
```
script/test atlas_up
```

## Commit Process

For each logical group of changes, follow these steps:

1. Stage your appropriate files
```
git add <files>
```

2. Create a well-formatted commit message following the project conventions
```
git commit -m "type(JIRA-ID): descriptive message

Detailed explanation of what was accomplished and why

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

## Update Documentation

After committing, complete these updates:

1. Update your current plan with implementation notes and status changes using standardized completion markers:
   - **✅ COMPLETE** - For fully completed phases/sections
   - **✅ IMPLEMENTATION COMPLETE** - For completed implementation work  
   - **✅ PHASE COMPLETE** - For completed phases
   - **🔄 IN PROGRESS** - For actively worked sections
   - Mark completed tasks with ✅ and [x] checkboxes
   - Add implementation notes to document key decisions
   - Add commit hashes to reference specific changes

2. Update the memory tool with:
   - New components and their relationships
   - Implementation patterns to remember
   - Key decisions made during implementation

## Final Verification

As a final step, complete these checks:

1. Verify all your changes are committed
```
git status
```

2. Confirm your plan is updated with current progress
3. Create a summary of what you accomplished and what's next