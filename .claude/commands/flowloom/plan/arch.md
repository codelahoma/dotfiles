Analyze features and recommend architectural approaches
Let input_args = "$ARGUMENTS"

Analyze the feature described and recommend architectural approaches.

## Step 1: Context Analysis

First, determine the current JIRA ticket from the git branch or arguments, then analyze existing plans:

1. **Extract JIRA ID** from current git branch name or arguments
2. **Check for existing architecture plans** in `${FLOWLOOM_WORK_DIR:-.meta-claude}/plans/[JIRA-ID]/` directory:
   ```bash
   ls ${FLOWLOOM_WORK_DIR:-.meta-claude}/plans/[JIRA-ID]/*00_*.md 2>/dev/null
   ```
3. **Determine context** based on what exists

## Step 2: Argument Interpretation and Context Rules

Based on existing plans and input_args:

**Scenario A: No 100_ plan exists**
- Arguments are REQUIRED to specify what to create architecture for
- If input_args is empty: ERROR - must specify feature/component to analyze
- If input_args provided: Create 100_ architecture plan for specified feature

**Scenario B: 100_ plan exists, no other hundreds multiples (200_, 300_, etc.)**
- If input_args is empty: Ask user to choose:
  - Update existing 100_ plan
  - Start new side task (200_ plan)
- If input_args provided: Ask user to choose:
  - Update existing 100_ plan with new focus from arguments
  - Create new side task (200_ plan) for the specified feature

**Scenario C: Multiple hundreds multiples exist (100_, 200_, maybe 300_, etc.)**
- Always prompt user to choose:
  - Modify existing [list existing X00_ plans with descriptions]
  - Create new side task ([next available multiple]00_ plan)
- If input_args provided, use them for the selected option

## Argument Patterns
- (no arguments with no 100_ plan) - ERROR: Must specify feature to analyze
- (no arguments with existing plans) - Interactive choice of existing vs new
- `"user authentication system"` - Create architecture for specified feature
- `AUP-1234` - Focus on specific JIRA ticket requirements (if different from current branch)
- `microservice` - Include microservice architecture options in analysis
- `update 100_` - Force update of existing 100_ plan
- `new side task` - Force creation of new side task with next available hundred

## IMPORTANT INSTRUCTIONS FOR CLAUDE

1. Your task is ONLY to analyze the feature and suggest possible architectures
2. DO NOT select an architecture or begin implementation
3. DO NOT proceed to any next steps after providing architecture options
4. STOP after presenting the architecture options to the user

## Step 3: Plan File Creation

Create a properly numbered architecture plan file based on the determined context:

1. Follow the plan file naming convention defined in `${FLOWLOOM_WORK_DIR:-.meta-claude}/docs/plan_file_naming_convention.md`
2. Use the numbering determined from context analysis:
   - **100_**: For main work architecture plans
   - **200_**: For first side task architecture plans/reviews  
   - **300_**: For second side task architecture plans/reviews
   - **Next available hundred**: For new side tasks
3. If updating existing plan, modify the existing file
4. If creating new plan:
   - Check if implementation plans (X20_) already exist for this work stream
   - If yes, create as architecture review: `[X00]_[JIRA-ID]_architecture-review_[descriptive-name].md`
   - If no, create as architecture plan: `[X00]_[JIRA-ID]_architecture_[descriptive-name].md`
5. Place it in the appropriate JIRA-ID subdirectory of `${FLOWLOOM_WORK_DIR:-.meta-claude}/plans/`

## Step 4: Interactive Decision Making

Before creating or updating any files, analyze all plans and implement the decision logic:

1. **Check for existing X00_ architecture plans**
2. **Check for existing X10_ and X20_ plans WITHOUT corresponding X00_ plans**
3. **Determine available options**

```
# First, scan for orphaned work streams (X10_/X20_ plans without X00_)
ORPHANED_STREAMS = find plans matching pattern X10_*.md or X20_*.md where no X00_*.md exists for same hundreds digit

IF no existing X00_ plans AND no orphaned streams:
    IF input_args is empty:
        RETURN ERROR: "No architecture plans exist. Please specify what feature/component to analyze."
    ELSE:
        CREATE 100_ plan for input_args

ELIF no existing X00_ plans BUT orphaned streams exist:
    PROMPT: "Found implementation plans without architecture plans. Choose:"
    FOR each orphaned stream:
        ADD OPTION: "Create X00_ architecture review for [brief description of X10_/X20_ plan]"
    IF input_args provided:
        ADD OPTION: "Create new 100_ architecture plan for '{input_args}'"
    ELSE:
        ADD OPTION: "Create new 100_ architecture plan (specify feature)"

ELIF only 100_ plan exists:
    IF orphaned streams exist:
        PROMPT: "Choose:"
        ADD OPTION: "Update existing 100_ plan"
        FOR each orphaned stream:
            ADD OPTION: "Create X00_ architecture review for [brief description]"
        ADD OPTION: "Create new side task ([next_hundred]00_)"
    ELSE:
        IF input_args is empty:
            PROMPT: "Found existing 100_ plan. Choose: (1) Update existing plan, (2) Create new side task (200_)"
        ELSE:
            PROMPT: "Found existing 100_ plan. For '{input_args}': (1) Update existing plan, (2) Create new side task (200_)"

ELIF multiple X00_ plans exist:
    PROMPT: "Choose:"
    FOR each existing X00_ plan:
        ADD OPTION: "Update [X00_ plan description]"
    FOR each orphaned stream:
        ADD OPTION: "Create X00_ architecture review for [brief description]"
    ADD OPTION: "Create new side task ([next_hundred]00_)"
```

## Step 5: Architecture Analysis

Based on the decision made in Step 4, provide architectural analysis:

**If creating new architecture plan:**
Recommend 2-3 architectural approaches:

1. **Architecture Option 1**
   - Overview: Brief description of the architectural pattern
   - Key Components: Main components and their responsibilities
   - Data Flow: How data moves through the system
   - Pros & Cons: Trade-offs of this approach
   - Ideal Use Cases: When this architecture is most appropriate
   - Implementation Complexity: Estimated difficulty (Low/Medium/High)

2. **Architecture Option 2**
   [Similar structure as above]

3. **Architecture Option 3** (if applicable)
   [Similar structure as above]

**If updating existing architecture plan:**
- Analyze the existing plan's architecture
- Identify areas for improvement or extension
- Propose specific updates or additions
- Maintain consistency with existing architectural decisions

**If creating architecture review:**
- Review the existing implementation plan (X20_)
- Analyze the proposed implementation approach
- Identify architectural strengths and potential issues
- Recommend architectural improvements or alternatives
- Assess alignment with project architectural standards

## Step 6: After Presenting Analysis

After presenting the architecture analysis:
1. Ask the user which option they prefer (for new plans) or which updates to make (for existing plans)
2. DO NOT proceed with implementation without explicit user direction
3. Wait for the user to select an option or ask for modifications
4. Only create/update the plan file after user confirmation
