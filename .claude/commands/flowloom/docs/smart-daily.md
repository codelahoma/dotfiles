Let input_args = "$ARGUMENTS"

You are helping the user generate a comprehensive daily report using MCP-enhanced data gathering and analysis.

## Argument Interpretation  
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Generate today's full daily report
- If input_args is "yesterday": Generate yesterday's report
- If input_args is a date (YYYY-MM-DD): Generate report for that specific date
- If input_args is "summary": Generate condensed version focusing on key highlights
- If input_args is "detailed": Include expanded analysis and context

## Smart Daily Report Generation Process

1. **Git Activity Analysis (MCP-Enhanced)**
   - Use mcp__git__git_log with appropriate date filtering to gather commits
   - Use mcp__git__git_diff to analyze change scope and impact
   - Use mcp__github__list_commits to cross-reference with GitHub activity

2. **File System Changes**
   - Use mcp__filesystem__search_files to identify recently modified files
   - Use mcp__filesystem__get_file_info to gather modification timestamps
   - Use mcp__filesystem__read_multiple_files to analyze changes in key files

3. **Memory and Knowledge Integration**
   - Use mcp__memory__search_nodes to find relevant context for the day's work
   - Use mcp__basic-memory__recent_activity to gather memory insights
   - Use mcp__basic-memory__search_notes to find related discussions and decisions

4. **Database Analytics**
   - Use mcp__sqlite__read_query to gather project metrics and tracking data
   - Query plan progress, task completion, and other stored metrics

5. **External Context (When Relevant)**
   - Use mcp__brave-search__brave_web_search to gather context on technologies or issues encountered
   - Use mcp__github__search_issues to check for related community discussions

6. **Comprehensive Report Assembly**
   Generate a structured daily report including:
   
   **Activity Summary**
   - Git commits and changes made
   - Files modified and their significance
   - Time spent on different areas (inferred from commit patterns)
   
   **Progress Analysis**
   - Goals achieved vs. planned
   - Blockers encountered and resolved
   - Technical decisions made
   
   **Knowledge Insights**
   - Key learnings captured
   - Connections to previous work
   - Updated mental models or understanding
   
   **Forward Planning**
   - Outstanding tasks and next steps
   - Dependencies identified
   - Recommended focus areas

7. **Memory Storage**
   - Use mcp__basic-memory__write_note to store the daily report
   - Use mcp__memory__add_observations to capture key insights in knowledge graph
   - Use mcp__sqlite__write_query to update progress tracking metrics

Show the user a comprehensive daily report that provides both detailed analysis and actionable insights for future work. Include data sources consulted and confidence levels for different insights.