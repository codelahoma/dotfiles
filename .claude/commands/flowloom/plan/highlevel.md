Generate comprehensive high-level plan for feature implementation
Let input_args = "$ARGUMENTS"

Generate a comprehensive high-level plan for implementing a feature using the specified architecture.

## Argument Interpretation
First, analyze the provided arguments: input_args

**When no arguments provided (input_args is empty):**
Offer the user these options:
1. **Use current context** - Generate plan based on current session context and recent FlowLoom activity
2. **Specify feature/component** - Enter what you want to plan (e.g., "memory coordination", "installer system")
3. **Specify architecture approach** - Enter preferred architecture (e.g., "microservices", "event-driven")
4. **Interactive planning** - Guided conversation to determine what to plan

Present these options clearly and wait for user choice.

**When arguments provided:**
- Feature description: Generate plan for that feature
- Architecture choice: Use that specific architecture approach
- Component name: Focus on that FlowLoom component

## Argument Patterns
- (no arguments) - Offer user choice of context options
- `memory coordination system` - Generate plan for that feature
- `event-driven architecture` - Use that architecture approach
- `multi-claude-coordination` - Focus on that FlowLoom component
- `installer-system microservices` - Combine component with architecture

## User Interaction Process

When input_args is empty, present this menu:
```
Generate High-Level Implementation Plan

Choose your approach:
1. Use current context (based on: [describe current session context])
2. Specify feature/component to plan
3. Specify architecture approach
4. Interactive planning session

What would you like to do? (1-4): 
```

Then proceed based on user selection.

## Planning Process

Think deeply and generate a comprehensive high-level plan for implementing the feature using the architecture specified in input_args or selected through user interaction.

Follow the plan file naming convention defined in `${FLOWLOOM_WORK_DIR:-.meta-claude}/docs/plan_file_naming_convention.md`:
- Use the pattern: `110_[JIRA-ID]_high_level_[descriptive-name].md` 
- Place it in the appropriate JIRA-ID subdirectory of `${FLOWLOOM_WORK_DIR:-.meta-claude}/plans/`

The plan should include:
1. **Feature Summary**: Brief overview of what's being built
2. **Architecture Overview**: Summary of the chosen architectural approach
3. **Implementation Phases**: Major milestones with estimated complexity
4. **Key Components**: Components to be built or modified
5. **Dependencies**: External systems or libraries needed
6. **Potential Challenges**: Anticipated technical difficulties
7. **Testing Strategy**: Approach to validation

If no architecture was specified in input_args, suggest an appropriate architecture based on the feature context.
