# FlowLoom App Builder - Status

Check the status of active app building coordination sessions and view available tasks.

## Command Overview

This command provides a comprehensive view of:
- Active app building coordination sessions
- Task progress and assignments
- Worker availability and capabilities
- Overall project status

## Status Display

@bash /coord:status

After showing the coordination status, provide a user-friendly summary:

## Summary Analysis

Based on the coordination status above:

**Active Sessions:** List any app-builder coordination sessions found

**Task Breakdown:**
- **Completed Tasks:** Count and summarize finished work
- **In Progress Tasks:** Show what's currently being worked on
- **Pending Tasks:** List available tasks awaiting workers
- **Blocked Tasks:** Identify any tasks with dependencies or issues

**Worker Status:**
- **Active Workers:** List registered workers and their capabilities
- **Available Capacity:** Identify workers available for new tasks
- **Specialization Coverage:** Show which skill areas are covered

**Project Health:**
- **Progress Assessment:** Overall completion percentage if calculable
- **Risk Factors:** Identify potential blockers or resource gaps
- **Next Actions:** Recommend immediate next steps

## Quick Actions

Based on the current status, suggest relevant actions:

**If no active sessions:**
- Use `/flowloom:app:build [description]` to start a new app project

**If sessions exist but need workers:**
- Use `/flowloom:app:join [capabilities]` to contribute as a worker

**If you're already a worker:**
- Review available tasks and claim one that matches your skills
- Use `/worker:complete [task_id]` to submit finished work

**If coordination is needed:**
- Use `/coord:dispatch [task] [capabilities]` to add new tasks
- Use `/coord:workflow` commands to manage overall progress

## Example Output Format

```
🚀 FlowLoom App Builder Status

📋 Active Sessions: 
   • app-builder-session (E-commerce Platform)

⚡ Task Summary:
   • Completed: 3/10 tasks (30%)
   • In Progress: 2 tasks
   • Pending: 5 tasks

👥 Workers:
   • frontend-specialist (react, typescript)
   • backend-expert (nodejs, api, database)
   • Available slots: 2

🎯 Next Actions:
   • Need testing specialist for QA tasks
   • Frontend components ready for integration
   • Database schema design in progress
```

This provides a quick overview for coordination and decision-making.