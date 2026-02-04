# FlowDocs Knowledge Base

This directory contains project documentation managed by FlowDocs.

## Directory Structure

- `architecture/` - System design and architecture documentation
  - `decisions/` - Architecture Decision Records (ADRs)
- `guides/` - How-to guides and tutorials
- `troubleshooting/` - Problem/solution documentation
- `features/` - Feature specifications and documentation

## Document Format

Each document uses YAML front matter for metadata:

```yaml
---
title: Document Title
type: note
permalink: folder/document-slug
tags: [tag1, tag2]
---
```

## Indexing with basic-memory

After creating or editing documents, run:

```bash
# One-time sync
basic-memory sync

# Or continuous sync (watches for changes)
basic-memory sync --watch
```

This indexes documents for semantic search via the basic-memory MCP server.

## Creating New Documents

Use the FlowDocs CLI:

```bash
flowdocs new "Document Title" --folder guides --tags tag1,tag2
```

Or create files manually following the format above.
