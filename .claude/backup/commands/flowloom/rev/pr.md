Let input_args = "$ARGUMENTS"

Create a full PR description for your current branch, copying the title and body to the clipboard.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the argument pattern:
- If input_args is empty: Use current branch and auto-detect ticket number
- If input_args contains a ticket number: Use that specific ticket
- If input_args contains "brief": Create a shorter, more concise PR description
- If input_args contains "detailed": Include extra technical details

## Argument Patterns
- (no arguments) - Auto-detect from current branch
- `AUP-1234` - Use specific ticket number
- `brief` - Create concise PR description
- `detailed` - Include comprehensive technical details
- `AUP-1234 detailed` - Use specific ticket with detailed description

Use these steps to generate a comprehensive PR description:

1. Extract the branch name and detect any Jira ticket numbers (or use provided ticket)
2. Search for relevant documentation and plans in ${FLOWLOOM_WORK_DIR:-.meta-claude}/plans, ${FLOWLOOM_WORK_DIR:-.meta-claude}/docs, and ${FLOWLOOM_WORK_DIR:-.meta-claude}/walkthroughs directories related to this ticket
3. Prioritize more recently updated files when gathering information
4. Summarize the implementation based on:
   - The most recent plan file (likely contains current status)
   - Recent git commits
   - Architecture diagrams if available
   - Any walkthrough documents created
5. Structure the PR description to include:
   - Purpose of the changes
   - Key implementation details
   - Technical decisions made and reasoning
   - Testing approach
   - Any follow-up work needed

Format the title as "TICKET-NUMBER: Descriptive title about the changes" if a Jira ticket is detected.

End the body with "[TICKET-NUMBER]" on a line by itself for the GitHub integration.

Run pbcopy twice - first for the title, then for the body, so both are available in clipboard history.
