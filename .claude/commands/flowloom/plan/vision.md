Let input_args = "$ARGUMENTS"

You are helping the user review and work with FlowLoom vision plans (000-009 series). These plans contain the foundational vision, strategy, and long-term direction for FlowLoom.

## Argument Interpretation

Based on the provided arguments: input_args

**Argument Patterns:**
- (no arguments) - Show recent vision plans and allow selection
- `list` - Show all vision plans  
- `create [title]` - Create a new vision plan with specified title
- `[plan_number]` - Directly review a specific vision plan (e.g., `001`, `005`)
- `[search_term]` - Search for vision plans containing the term

## Vision Plan Categories (000-009)

According to FlowLoom's hierarchical numbering:
- **000-009**: Vision and strategy (project vision, mission, long-term goals)

## Step 1: Process Arguments and Take Action

If input_args is empty or "list":
1. **Show recent vision plans** using the recent_plans utility with vision filter:

@bash
${FLOWLOOM_WORK_DIR:-.meta-claude}/bin/recent_plans --vision-only

Show the user this list of vision plans. For each plan, extract and display the plan number from the filename and include it in a clear format.

If input_args contains a plan number (like "001" or "5"):
1. **Find and review the specific vision plan**
2. Read the plan file thoroughly
3. Check current status and implementation progress
4. Provide summary and suggest next steps

If input_args is "create" followed by a title:
1. **Create a new vision plan** in the 000-009 range
2. Find the next available number in the vision sequence
3. Use FlowLoom's standard plan template
4. Include the specified title in the plan

If input_args contains other text:
1. **Search for vision plans** containing the search term
2. Display matching plans with context
3. Allow user to select one for detailed review

## Step 2: Vision Plan Management

When working with vision plans:

### Reading Vision Plans
- Focus on long-term strategic direction
- Note alignment with current implementation
- Identify any gaps between vision and reality
- Check for consistency across vision documents

### Creating Vision Plans  
- Use next available number in 000-009 range
- Follow FlowLoom plan naming convention: `00X_FlowLoom_vision_descriptive-title.md`
- Include sections for:
  - Strategic Vision
  - Long-term Goals
  - Success Metrics
  - Implementation Approach

### Updating Vision Plans
- Mark sections as complete when achieved
- Update metrics and milestones
- Ensure vision remains current and aspirational
- Maintain consistency with other foundation plans

## Step 3: Provide Context and Recommendations

After showing vision plans or reviewing a specific plan:

1. **Summarize the current vision status**
2. **Note any inconsistencies** between vision and current development
3. **Suggest next steps** for vision refinement or implementation
4. **Highlight connections** to other foundation plans (010-059)

Remember: Vision plans are the strategic foundation for all FlowLoom development. They should be aspirational but achievable, and regularly updated to reflect the project's evolution.