---
title: 'Multi-Claude Coordination: Last-Joined-Master Pattern'
type: note
permalink: flow-loom/technical/multi-claude-coordination-last-joined-master-pattern
---

# Multi-Claude Coordination: Last-Joined-Master Pattern

## Alternative to MCP Server Approach

Instead of creating a dedicated MCP server for Claude coordination, consider a simpler pattern: **the most recently joined Claude instance automatically becomes the master**.

## Technical Considerations

### Advantages:
- **Simplicity**: No additional MCP server infrastructure required
- **Automatic handoff**: Natural transition when new instances join
- **Stateless**: No persistent coordination state to manage
- **Self-organizing**: System naturally adapts to instance changes

### Implementation Challenges:
- **Detection mechanism**: How does an instance know it's "last joined"?
- **State transfer**: How does the new master inherit context from previous master?
- **Race conditions**: What happens if multiple instances join simultaneously?
- **Persistence**: How to maintain coordination across restarts?

## Potential Implementation Approaches

### File-based coordination:
- Write timestamp to shared file on startup
- Latest timestamp = current master
- Simple but requires filesystem access

### Git-based coordination:
- Use git operations as coordination primitives
- Commit timestamps could indicate "join time"
- Natural for development workflows

### Memory-based coordination:
- Use existing memory systems (basic-memory, memory graph)
- Store coordination metadata in shared knowledge base
- Leverages existing FlowLoom infrastructure

## Questions for Investigation:
1. How hard is it to detect "last joined" in Claude Code?
2. Can we access startup/session metadata?
3. What shared state mechanisms are already available?
4. How to handle graceful degradation when coordination fails?

## Next Steps:
- Research Claude Code session detection capabilities
- Test simple file-based coordination prototype
- Compare complexity vs MCP server approach
- Consider hybrid: simple coordination with MCP fallback