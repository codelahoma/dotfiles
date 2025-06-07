---
title: Claude Code Model Capabilities - Image Access Differences
type: note
permalink: technical-notes/claude-code-model-capabilities-image-access-differences
---

# Claude Code Model Capabilities - Image Access Differences

**Date**: May 27, 2025  
**Context**: Discovered during opus-chat session while attempting forensics command

## Key Finding

**Opus models in Claude Code CLI cannot access image content**, while Sonnet models can. This is an important capability difference when working with:

- Screenshots of errors or commands
- Visual documentation
- Image-based debugging
- UI/UX feedback

## Implications

1. **For Forensics Work**: When documenting permission blocks or errors, Opus users must provide text descriptions rather than screenshots

2. **For Model Selection**: Choose Sonnet when work involves:
   - Reviewing screenshots
   - Debugging visual issues
   - Working with image-based documentation

3. **For Documentation**: Always provide text alternatives when working with Opus to ensure information is accessible

## Workaround

When using Opus and needing to share visual information:
- Copy/paste text content instead of screenshots
- Describe visual elements verbally
- Use Sonnet for image-heavy workflows

This limitation is specific to the Claude Code CLI environment and may differ from web-based Claude interfaces.