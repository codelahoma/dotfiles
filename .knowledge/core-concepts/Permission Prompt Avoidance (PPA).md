---
title: Permission Prompt Avoidance (PPA)
type: note
permalink: core-concepts/permission-prompt-avoidance-ppa
tags:
- '#ppa'
- '#permissions'
- '#architecture'
- '#principles'
- '#flowloom'
- '#ethics'
---

# Permission Prompt Avoidance (PPA)

## Definition

Permission Prompt Avoidance (PPA) is a core FlowLoom architectural principle that designs workflows to operate within pre-approved permissions, avoiding runtime permission prompts that interrupt flow and degrade user experience.

## Core Principle

**Design systems to work within existing permissions rather than requesting new ones.**

## Why PPA Matters

1. **Uninterrupted Flow** - No jarring permission prompts breaking concentration
2. **Better UX** - Smooth, continuous operation
3. **Predictable Behavior** - Commands always work the same way
4. **Security** - Operates within established boundaries
5. **Automation-Friendly** - Scripts run without manual intervention

## PPA Patterns in FlowLoom

### 1. File Truncation Instead of Deletion
```bash
# ❌ Requires rm permission
rm /tmp/file.txt

# ✅ PPA approach - truncate instead
echo -n > /tmp/file.txt
: > /tmp/file.txt
> /tmp/file.txt
```

### 2. Git Commit via File
```bash
# ❌ Complex quoting with -m can trigger prompts
git commit -m "Complex
multi-line
message"

# ✅ PPA approach - use file
echo "message" > .flowloom/.tmp/commit-msg.txt
git commit -F .flowloom/.tmp/commit-msg.txt
echo -n > .flowloom/.tmp/commit-msg.txt
```

### 3. Directory Creation with -p
```bash
# ❌ May fail if parent doesn't exist
mkdir new/deep/directory

# ✅ PPA approach - always use -p
mkdir -p new/deep/directory
```

### 4. File Operations via MCP
```bash
# ❌ Direct file operations may trigger prompts
cat file.txt | grep pattern

# ✅ PPA approach - use pre-approved MCP
mcp__filesystem__read_file
```

### 5. Safe File Writing
```bash
# ❌ Using >> may trigger different permissions
echo "append" >> file.txt

# ✅ PPA approach - read, modify, write whole file
# Use mcp__filesystem__edit_file or read/write pattern
```

## PPA Design Guidelines

### 1. Analyze Permission Requirements Early
Before implementing, ask:
- What permissions will this need?
- Are they in our standard set?
- Can we achieve the goal differently?

### 2. Prefer Pre-Approved Operations
FlowLoom's pre-approved permissions include:
- All git bash commands
- MCP filesystem operations
- Directory creation with -p
- File truncation
- Echo and cat commands

### 3. Avoid These Patterns
- `rm` commands (use truncation)
- `sudo` anything
- Direct system modifications
- Network operations outside MCP
- Interactive commands

### 4. Design Fallbacks
If you must do something that might trigger prompts:
- Provide alternative approach
- Document the permission needed
- Suggest adding to settings.local.json

## Examples of PPA in Action

### Session Cleanup
Instead of deleting session files:
```bash
# PPA approach - truncate logs, keep structure
find .flowloom/sessions -name "*.log" -exec sh -c 'echo -n > "$1"' _ {} \;
```

### Temporary Files
Instead of creating/deleting temp files:
```bash
# PPA approach - reuse same file paths
TEMP_FILE=".flowloom/.tmp/workspace.txt"
echo "data" > "$TEMP_FILE"
# ... use file ...
echo -n > "$TEMP_FILE"  # Clear for next use
```

### Configuration Updates
Instead of modifying system files:
```bash
# PPA approach - work within project
cp .flowloom/templates/config.json ./.config.json
# Modify local copy only
```

## The PPA Mindset

When designing FlowLoom features, always think:
1. "How can I do this without new permissions?"
2. "What would the PPA way be?"
3. "Is there a creative solution within bounds?"

This constraint drives innovation and creates more robust solutions.

## Integration with FlowLoom

PPA is built into FlowLoom's DNA:
- All stdlib functions use PPA patterns
- Commands designed with PPA first
- Installation pre-approves necessary permissions
- Documentation emphasizes PPA approaches

## Benefits Beyond Permissions

PPA often leads to better design:
- More portable code (fewer system dependencies)
- Cleaner interfaces (well-defined boundaries)
- Better testability (predictable behavior)
- Improved security (principle of least privilege)

Remember: **Every permission prompt is a UX failure.** Design to avoid them!

## Ethics Statement

**PPA is about AVOIDANCE, not EVASION.**

FlowLoom is committed to:
- **Respecting Claude Code's security boundaries** - We work within them, not around them
- **Ethical development practices** - No tricks, hacks, or exploits
- **Transparency** - Clear about what we're doing and why
- **Security-first mindset** - The restrictions exist for good reasons

### What PPA Is:
- ✅ Creative problem solving within constraints
- ✅ Designing elegant solutions that don't need extra permissions
- ✅ Respecting the security model
- ✅ Making the secure path the easy path

### What PPA Is NOT:
- ❌ Finding ways to bypass security
- ❌ Exploiting loopholes
- ❌ Hiding operations from the user
- ❌ Working against Claude Code's design

### The Right Mindset:
"How can we achieve our goal while fully respecting the permission system?"
NOT "How can we get around these permissions?"

This ethical approach makes FlowLoom:
- **Trustworthy** - Users know we respect boundaries
- **Secure** - We strengthen rather than weaken security
- **Sustainable** - Our patterns work with, not against, the platform
- **Educational** - We model good security practices

By embracing constraints creatively, we often find better solutions than if we had unlimited permissions. PPA is about innovation through limitation, always with respect for the security boundaries that keep users safe.

## Working with PPA

### When You Need New Permissions

Sometimes, despite PPA best practices, you'll discover operations that need permissions you don't have yet. This is normal and part of the collaborative process with Claude Code.

When this happens:
1. **Evaluate if you really need it** - Can you use PPA patterns instead?
2. **Understand what you're approving** - Read the permission request carefully
3. **Get permissioned** - Approve it if it makes sense for your workflow
4. **Share your discovery** - Let others know: "I just got permissioned on X"

### The "Got Permissioned" Culture

Using the phrase "got permissioned" acknowledges that:
- You're teaching your Claude instance new capabilities
- Each permission makes your environment more powerful
- You're in control of what to allow
- Sharing helps the community

Example: "I just got permissioned on `Bash(gh pr create:*)` - now we can create pull requests directly!"

### Balance Between PPA and Permissions

The ideal FlowLoom workflow:
1. **Start with PPA** - Use creative solutions within existing permissions
2. **Get permissioned when needed** - For operations that truly require it
3. **Document both approaches** - Help others choose the right path
4. **Share your learnings** - What permissions enabled new workflows?

Remember: PPA is the preference, but getting permissioned when necessary is part of building a capable development environment.