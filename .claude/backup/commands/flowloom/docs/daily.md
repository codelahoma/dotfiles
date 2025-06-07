Generate comprehensive daily development report with insights
Let input_args = "$ARGUMENTS"

Generate a daily report for the Atlas UP AI project.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the argument pattern:
- If input_args is empty: Generate report for today's date
- If input_args contains a single date: Generate report for that specific date
- If input_args contains a date range: Generate reports for that range
- If input_args contains "force": Overwrite existing reports

## Argument Patterns
- (no arguments) - Generate today's daily report
- `yesterday` - Generate report for yesterday
- `2024-01-15` - Generate report for specific date
- `last week` - Generate reports for the past week
- `May 10-15` - Generate reports for date range
- `force` - Overwrite existing reports
- `yesterday force` - Generate yesterday's report, overwriting if exists

First, determine the date(s) to report on:

1. Parse the date specification from input_args
   - If no arguments provided, use today's date
   - If arguments represent a single date (e.g., "yesterday", "2023-05-20"), use that date
   - If arguments represent a date range (e.g., "last week", "May 10-15", "2023-05-01 to 2023-05-07"), use that range
   - Interpret relative dates like "yesterday", "last Monday", "two days ago", etc.

2. Before proceeding, show the interpreted date(s) and ask for confirmation: 
   "Generate daily reports for: [formatted date list]. Proceed? (yes/no)"
   - Only continue if confirmed with "yes" or similar affirmative response

3. For each date in the range, check if a report already exists at `docs/daily_logs/{YYYYMMDD}-daily.md`
   - Skip dates with existing reports unless "force" argument was provided

For each date to process:
1. Examine the git history:
   - Run `git log --since="{date} 00:00:00" --until="{date} 23:59:59" --pretty=format:"%h %s [%an]"`
   - For significant commits, look at file changes with `git show <commit-hash>`

2. Create a report following this template:

# Daily Report - {date in YYYY-MM-DD format}

## Overview
[Provide a brief summary of the day's focus and accomplishments]

## Commits
[List all commits made on this day, grouped by branch, with descriptions]

## Key Changes
[Summarize significant code changes, refactoring, or new features implemented]

## Decisions
[Document important decisions made and their rationale]

## Challenges
[Note any challenges encountered and how they were addressed]

## Next Steps
[Outline planned next steps or outstanding issues]

3. Save each report to `docs/daily_logs/{YYYYMMDD}-daily.md`

If multiple days were processed, create a summary of the entire period.

For the most recent day (or only day if single date), create a concise summary for Slack that includes:
- 1-2 sentence overview of what was accomplished
- Number of commits and key changes
- Significant achievements or milestones
- Any blockers or important issues

3. Save the Slack summary to `docs/daily_logs/{YYYYMMDD}-slack-summary.md`

4. Automatically copy the Slack summary to the clipboard using:
   ```bash
   cat docs/daily_logs/{YYYYMMDD}-slack-summary.md | pbcopy
   ```

Format your response with:
1. Confirmation of which reports were created
2. The Slack summary for the most recent/only day that's been copied to the clipboard