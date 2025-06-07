# FlowLoom Knowledge Base

This directory contains the semantic knowledge graph for the FlowLoom project, managed by the Basic Memory MCP server.

## Overview

The knowledge base captures:
- Design decisions and rationale
- Feature relationships and dependencies
- Implementation patterns and best practices
- Project evolution and history
- Team insights and learnings

## Structure

Knowledge is stored as Markdown files with semantic linking between concepts. The Basic Memory server manages:
- Entity creation and relationships
- Semantic search across knowledge
- Knowledge graph navigation
- Context-aware retrieval

## Usage

The knowledge base is automatically populated during development conversations with Claude. Key patterns:

1. **Design Decisions**: Captured when discussing architecture and implementation choices
2. **Feature Documentation**: Created when implementing new functionality
3. **Problem Solutions**: Recorded when solving technical challenges
4. **Best Practices**: Documented as patterns emerge

## Benefits

- **Version Controlled**: All knowledge is tracked in git
- **Human Readable**: Browse and edit knowledge files directly
- **Team Accessible**: Share collective project understanding
- **AI Enhanced**: Claude can reference and build upon previous knowledge

## Viewing the Knowledge Base

- Browse `.knowledge/` directory for Markdown files
- Use tools like Obsidian for graph visualization
- Search with grep/ripgrep for quick lookups
- Let Claude query the semantic graph for connections