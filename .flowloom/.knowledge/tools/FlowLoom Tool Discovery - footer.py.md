---
title: FlowLoom Tool Discovery - footer.py
type: discovery
tags:
- flowloom
- tools
- cli
- shebang
created: 2025-01-09
permalink: tools/flow-loom-tool-discovery-footer-py
---

# FlowLoom Tool Discovery - footer.py

## Overview
Discovered that footer.py is a shebang script that can be executed directly.

## Key Learning
- **Location**: `./bin/footer.py`
- **Type**: Shebang script (executable)
- **Usage**: Can be called directly as `./bin/footer.py` instead of `python3 ./bin/footer.py`

## Observations
- [tool] footer.py is a shebang script located at ./bin/footer.py
- [execution] Can be executed directly without python3 prefix
- [functionality] Generates FlowLoom interactive footer with memory status indicators
- [parameters] Supports session context and next steps parameters

## Example Usage
```bash
./bin/footer.py --session "Startup Context Loaded" --next-steps "1. Review\\n2. Continue"
```

## Relations
- part_of [[FlowLoom System]]
- implements [[Interactive Footer Pattern]]
- discovered_during [[Startup Context Loading]]