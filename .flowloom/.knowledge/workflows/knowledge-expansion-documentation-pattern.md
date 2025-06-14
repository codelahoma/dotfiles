---
type: guide
category: workflows
tags: [knowledge-expansion, documentation-pattern, learning-capture]
status: active
complexity: intermediate
priority: high
frequency: daily
audience: [flowloom-ai]
introduced_in: "FlowLoom 2.0.0"
last_verified: "2025-06-07"
maintainer: FlowLoom Core Team
permalink: knowledge-expansion-documentation-pattern
depends_on: [dual-memory-system-methodology, flowloom-documentation-methodology]
related_to: [session-documentation, methodology-discovery]
---

# Knowledge Expansion Documentation Pattern

## Core Protocol: Capture Learning Immediately

When FlowLoom discovers new insights, patterns, or methodologies, follow this protocol:

### 1. File-First Documentation
- **Create markdown files**: Write directly to `.knowledge` structure
- **Use proper front matter**: Include FlowLoom schema fields
- **Sync to database**: Run `uvx basic-memory --project ./.flowloom/.knowledge sync`

### 2. Session Tracking
- **Log in fl-memory.json**: Add observations for development context
- **Use relative paths**: Avoid permission and guardrail issues
- **Rich observations**: Use `--from-file` pattern for complex content

### 3. Knowledge Integration
- **Cross-reference existing knowledge**: Ensure consistency
- **Update index documents**: Maintain discoverability
- **Establish search patterns**: Make knowledge easily retrievable

## Example Implementation (2025-06-07)

**Discovery**: Configuration files were being misused for historical documentation

**Immediate Action**:
1. Created `dual-memory-system-methodology.md` in `.knowledge/workflows/`
2. Cleaned up CLAUDE.local.md configuration
3. Established this documentation pattern

**Result**: Knowledge preserved, methodology improved, configuration clarified

## Templates for Common Scenarios

### New Methodology Discovery
```yaml
---
type: guide
category: workflows
tags: [methodology, discovery]
status: active
---

# [Methodology Name]

## Discovery Context
- When: [Date/Session]
- Trigger: [What led to discovery]
- Previous approach: [What was wrong]

## New Understanding
[Clear explanation of insight]

## Implementation
[How to apply this knowledge]

## Impact
[How this changes our approach]

## Observations
- [category] Key insights with category tags
```

### Tool/Command Learning
```yaml
---
type: reference
category: tools
tags: [cli, commands]
status: active
---

# [Tool/Command Name]

## Capability
[What it does]

## Usage Pattern
[How to use effectively]

## Integration Points
[How it connects to our workflow]

## Examples
[Concrete usage examples]
```

### Workflow Improvement
```yaml
---
type: guide
category: workflows
tags: [improvement, optimization]
status: active
---

# [Workflow Name] Improvement

## Previous State
[How things worked before]

## Identified Issue
[What was suboptimal]

## Improved Approach
[Better way to do it]

## Validation
[How we verify improvement]
```

## Documentation Triggers

Document immediately when:
- ❌ **Error Corrections**: Wrong assumptions identified
- ✅ **New Capabilities**: Tools/commands/patterns discovered  
- 🔄 **Workflow Improvements**: Better ways to accomplish tasks
- 🧠 **Methodology Insights**: Understanding how FlowLoom should operate
- 🔗 **Integration Discoveries**: How systems connect and interact

## Quality Standards

Every knowledge expansion document should:
- **Be searchable**: Use clear titles and key terms
- **Be actionable**: Include implementation guidance
- **Be connected**: Link to related knowledge via front matter
- **Be timestamped**: Include discovery context
- **Be validated**: Show evidence or testing when possible
- **Follow schema**: Use proper FlowLoom front matter

## Sync Commands

After creating knowledge files:
```bash
# Sync to basic-memory database
uvx basic-memory --project ./.flowloom/.knowledge sync --verbose

# Verify sync worked
uvx basic-memory --project ./.flowloom/.knowledge status
```

## Observations
- [pattern] File-first approach prevents MCP tool permission issues
- [workflow] Immediate documentation prevents knowledge loss
- [methodology] Proper front matter enables sophisticated search
- [integration] Dual system provides redundancy and different access patterns

This pattern ensures FlowLoom's knowledge compounds rather than being lost to context switching.