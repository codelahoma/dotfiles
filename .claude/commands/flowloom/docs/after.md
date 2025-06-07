Let input_args = "$ARGUMENTS"

Generate an after-hours report for your work on the Atlas UP AI project.

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the argument pattern:
- If input_args is empty: Generate report for today's after-hours work
- If input_args contains a date (YYYY-MM-DD): Generate report for that specific date
- If input_args contains "slack-only": Generate only the Slack summary, skip the full report
- If input_args contains a time range: Use that specific time range instead of after-hours

## Argument Patterns
- (no arguments) - Generate today's after-hours report
- `2024-01-15` - Generate after-hours report for specific date
- `slack-only` - Generate only Slack summary for today
- `17:00-23:59` - Use specific time range
- `2024-01-15 slack-only` - Slack summary for specific date

First, locate the relevant daily report at `docs/daily_logs/{YYYYMMDD}-daily.md` and identify the last commit covered.

Then examine git history after that commit:
1. Run `git log --since="today 17:00:00" --until="today 23:59:59" --pretty=format:"%h %s [%an]"`
2. For significant commits, look at file changes with `git show <commit-hash>`

Create a report following this template:

# After-Hours Report - {today's date in YYYY-MM-DD format}

## Overview
[Provide a brief summary of work done after the daily report]

## Starting Point
[Reference the last commit covered in the daily report]

## Commits
[List all commits made after hours, with descriptions]

## Key Changes
[Summarize significant code changes made after hours]

## Next Steps
[Outline items to be addressed in the next working day]

Please save this report to `docs/daily_logs/{YYYYMMDD}-after-hours.md`.

Also, create a concise summary for Slack that includes:
- 1-2 sentence overview of what was accomplished after hours
- Context for why after-hours work was needed
- Number of commits and key changes
- Significant achievements or milestones

Format your response with:
1. Confirmation that the report was created
2. The Slack summary (which you can copy to post to Slack)