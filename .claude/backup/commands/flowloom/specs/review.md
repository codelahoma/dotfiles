Let input_args = "$ARGUMENTS"

We're going to review product specifications and determine our next steps. This command is for SPECIFICATION REVIEW ONLY.

# IMPORTANT: FOLLOW THESE STEPS IN ORDER

## DO NOT START CODING
- This command is for reviewing and analyzing product specifications ONLY
- Do NOT begin implementing code until explicitly instructed
- Focus solely on understanding the specification and suggesting next steps
- Wait for separate implementation commands before writing any code

## Step 1: Immediately Show Specification List First
Run a search to find specification files (those with "specification" or "spec" in the name or numbered 010_) in the ${FLOWLOOM_WORK_DIR:-.meta-claude}/plans directory.

```bash
find ${FLOWLOOM_WORK_DIR:-.meta-claude}/plans -name "*.md" | grep -E "(specification|spec|010_)" | head -10 | while read -r file; do
  echo "$(stat -f "%Sm" -t "%Y-%m-%d %H:%M:%S" "$file") $file"
done | sort -r
```

IMMEDIATELY present this list to me WITHOUT performing any other searches or explorations first.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Show all specifications
- If input_args is a number: Jump directly to that specification from the list
- If input_args is a project name: Filter to specifications for that project

## Argument Patterns
- (no arguments) - Show all available specifications
- `3` - Select specification #3 from the list
- `FlowLoom` - Show only FlowLoom specifications
- `AUP-1250` - Show only AUP-1250 specifications

When presenting the list to the user:
- Show the numbered list of specification files found
- For each spec, extract and display the plan number from the filename (010_, etc.)
- Include the selection number (1-10), plan number from filename, title/description, and modification date
- Inform them they can respond with a number to view that specification, project name to filter, or "cancel" to end
- DO NOT analyze any specification files until I select a specific number

Example format:
"Here are the available product specifications:

1. **Plan 010** - FlowLoom Product Requirements (modified date)
2. **Plan 010** - AUP-1250 OAuth Integration Spec (modified date) 
3. **Plan 010** - Project Requirements Specification (modified date)
...

Please respond with a number (1-10) to view and review that specific specification, a project name to filter, or "cancel" to end the specification review."

Extract the plan number by looking for the pattern at the start of the filename (e.g., "010_") and include it in the display.

## Step 2: Only After I Select a Specification
If I respond with a number, THEN proceed with:
1. Examine the identified specification thoroughly
2. Check the current feature branch to determine project context
3. Use the memory tool to find related information:
   - Check what features are currently in development
   - Search for components and requirements mentioned in the specification
   - Look for related work recorded in previous sessions
4. Analyze the specification for:
   - Completeness of requirements
   - Feasibility of proposed features
   - Dependencies and constraints
   - Success metrics and acceptance criteria
5. Summarize current status and logical next steps

## Step 3: Update the Existing Specification
When making changes based on our discussion:
1. **ALWAYS update the existing specification file** that we're currently reviewing
2. DO NOT create new specification files unless explicitly instructed
3. Use Edit to modify the selected specification with standardized completion markers:
   - **Status updates** using completion indicators:
     - **✅ COMPLETE** - For fully specified sections
     - **✅ REQUIREMENTS COMPLETE** - For completed requirements analysis  
     - **✅ VALIDATION COMPLETE** - For validated requirements
     - **🔄 IN REVIEW** - For actively reviewed sections
     - **📝 DRAFT** - For draft requirements
     - **⚠️ NEEDS CLARIFICATION** - For unclear or incomplete requirements
   - Change requirement items from [ ] to [x] for validated requirements
   - Add stakeholder feedback and validation notes
   - Update acceptance criteria based on latest decisions
   - Revise requirements based on technical feasibility analysis
4. Make sure to preserve the overall structure and format of the specification
5. NEVER begin implementing code as part of this command - only update the specification

If I respond to the list with "cancel", you are done.

This approach ensures I can directly select which specification I want to review, after which you'll use all available tools including memory to provide comprehensive context.