We're going to review our current plan and determine our next steps. This command is for PLAN REVIEW ONLY.

# IMPORTANT: FOLLOW THESE STEPS IN ORDER

## DO NOT START CODING
- This command is for reviewing and analyzing plans ONLY
- Do NOT begin implementing code until explicitly instructed
- Focus solely on understanding the plan and suggesting next steps
- Wait for separate implementation commands before writing any code

## Step 1: Immediately Show Plan List First
Run the ${FLOWLOOM_WORK_DIR:-.meta-claude}/bin/recent_plans utility script to display a numbered list of the 10 most recently modified plan files.

```bash
${FLOWLOOM_WORK_DIR:-.meta-claude}/bin/recent_plans
```

IMMEDIATELY present this list to the USER WITHOUT performing any other searches or explorations first.

When presenting the list to the user:
- Show the numbered list from the recent_plans script output DIRECTLY TO THE USER
- For each plan, extract and display the plan number from the filename (100_, 110_, 120_, etc.)
- Include the selection number (1-10), plan number from filename, title/description, and modification date
- Present the output directly to the user so THEY can see the list and make selections
- DO NOT analyze any plan files until the user selects a specific number

Example format:
"Here are the 10 most recently modified plan files:

1. **Plan 100** - Plan Title (modified date)
2. **Plan 110** - Plan Title (modified date) 
3. **Plan 120** - Plan Title (modified date)
...

Please respond with a number (1-10) to view and review that specific plan, or "cancel" to end the plan review."

Extract the plan number by looking for the pattern at the start of the filename (e.g., "100_", "110_", "220_") and include it in the display.

**CRITICAL**: The user needs to SEE the plan list output directly, not just have it processed by an agent.

## Step 2: Only After I Select a Plan
If I respond with a number, THEN proceed with:
1. Examine the identified plan thoroughly
2. Check the current feature branch to determine our progress
3. Use the memory tool to find related information:
   - Check what features are currently in development
   - Search for components and requirements mentioned in the plan
   - Look for related work recorded in previous sessions
4. Summarize current status and logical next steps

## Step 3: Update the Existing Plan
When making changes based on our discussion:
1. **ALWAYS update the existing plan file** that we're currently following
2. DO NOT create new plan files unless explicitly instructed
3. Use Edit to modify the selected plan with standardized completion markers:
   - **Status updates** using completion indicators:
     - **✅ COMPLETE** - For fully completed phases/sections
     - **✅ IMPLEMENTATION COMPLETE** - For completed implementation work  
     - **✅ PHASE COMPLETE** - For completed phases
     - **🔄 IN PROGRESS** - For actively worked sections
     - **📝 PLANNED** - For future planned work
     - **⚠️ BLOCKED** - For blocked or dependent work
   - Change checklist items from [ ] to [x] for completed tasks
   - Add implementation details and notes
   - Update PR status in tracking tables
   - Revise steps based on our latest decisions
4. Make sure to preserve the overall structure and format of the plan
5. NEVER begin implementing code as part of this command - only update the plan

If I respond to the list with "cancel", you are done.

This approach ensures I can directly select which plan I want to review, after which you'll use all available tools including memory to provide comprehensive context.