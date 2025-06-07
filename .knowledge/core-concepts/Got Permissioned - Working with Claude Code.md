---
title: Got Permissioned - Working with Claude Code
type: note
permalink: core-concepts/got-permissioned-working-with-claude-code
tags:
- '#permissions'
- '#claude-code'
- '#collaboration'
- '#ppa'
- '#terminology'
---

# Got Permissioned - Working with Claude Code

## The Phrase

"Got permissioned" - When Claude Code requests a permission and you approve it, adding it to your settings.local.json for future use.

## Why This Matters

Using the phrase "got permissioned" acknowledges an important reality:
- **You're teaching your Claude instance** - Each permission you grant helps it work better
- **It's a collaborative process** - You and Claude work together to build capabilities
- **Permissions persist** - Once granted, they apply to all future conversations
- **You're in control** - You decide what to permit

## Examples in Practice

### Discovering New Capabilities
"Oh, I just got permissioned on `git worktree` commands - now we can use parallel development!"

### Sharing Knowledge
"I got permissioned on the MCP filesystem server yesterday. Make sure you have the allowedDirectories set correctly."

### Problem Solving
"If you're seeing permission prompts for that operation, you haven't got permissioned yet. Add it to your settings.local.json."

### Learning Together
"I just got permissioned on a new truncation method - let's try using that instead of the bash approach."

## The Permission Journey

1. **First Encounter** - Claude requests a permission
2. **Decision Point** - You evaluate if it's needed and safe
3. **Getting Permissioned** - You approve and it's added
4. **Future Benefit** - All future sessions can use it

## Building Your Permission Set

As you work with FlowLoom:
- Start with the bundled permissions (~150 pre-approved)
- Get permissioned on new tools as you discover needs
- Share useful permissions with the community
- Document why certain permissions are valuable

## Community Aspect

When someone says "I got permissioned on X":
- They're sharing a discovery
- They're helping others know what's possible
- They're building collective knowledge
- They're improving FlowLoom for everyone

## Best Practices

### When Getting Permissioned
1. **Understand what you're approving** - Read the permission carefully
2. **Consider PPA first** - Can you avoid needing it?
3. **Document why** - Note what workflow needed it
4. **Share if useful** - Help others discover capabilities

### Helping Others Get Permissioned
1. **Be specific** - "You need to get permissioned on `Bash(rg:*)`"
2. **Explain benefits** - Why this permission helps
3. **Show examples** - How to use it effectively
4. **Suggest alternatives** - PPA approaches if available

## Integration with FlowLoom

FlowLoom's installer helps you get permissioned efficiently:
- Pre-approved permissions for common operations
- Documented permission requirements
- Clear explanations of what each enables
- Templates for different workflows

## The Philosophy

"Getting permissioned" represents:
- **Growth** - Your Claude instance becomes more capable
- **Trust** - You're building a trusted working relationship
- **Customization** - Your setup reflects your needs
- **Empowerment** - You control your development environment

## Common Permissions to Get Permissioned On

### Essential Development
- `Bash(npm:*)` - Node.js package management
- `Bash(python:*)` - Python script execution
- `Bash(docker:*)` - Container operations

### Advanced Git
- `Bash(git worktree:*)` - Parallel development
- `Bash(git reflog:*)` - Recovery operations
- `Bash(git cherry-pick:*)` - Selective commits

### Productivity Tools
- `Bash(rg:*)` - Ripgrep for fast searching
- `Bash(jq:*)` - JSON processing
- `Bash(gh:*)` - GitHub CLI operations

Remember: Every time you get permissioned, you're not just solving today's problem - you're building a more capable development environment for all future work. Share your discoveries!