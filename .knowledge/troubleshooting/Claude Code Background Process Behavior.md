---
title: Claude Code Background Process Behavior
type: note
permalink: troubleshooting/claude-code-background-process-behavior
---

# Claude Code Background Process Behavior

## Test Results: Background Processes Are Allowed

### Successful Background Operations
- ✅ **`sleep 5 &`** - Simple background process works
- ✅ **`bundle exec jekyll serve --detach`** - Jekyll server successfully runs in background
- ✅ **Process detachment** - Server detached with PID 48636

### Jekyll Background Server Results
```bash
bundle exec jekyll serve --detach --port 4000
```

**Output:**
```
Server detached with pid '48636'
Run `pkill -f jekyll' or `kill -9 48636' to stop the server.
Server address: http://127.0.0.1:4000/flowloom/
```

### Key Findings
1. **Background processes are permitted** in Claude Code
2. **Jekyll serves successfully** in detached mode
3. **Baseurl configuration matters** - site available at `/flowloom/` not root `/`
4. **Process management commands work** - `jobs`, `pkill`, `kill` available

### Jekyll Baseurl Issue
- Site configured with `baseurl: "/flowloom"` in `_config.yml`
- Accessing `localhost:4000` shows "Not Found"
- **Correct URL:** `http://localhost:4000/flowloom/`

### Security Assessment
Contrary to initial assumption, Claude Code **does allow background processes**. This enables:
- Local development servers (Jekyll, React, etc.)
- Background build processes
- Long-running development tools

### Usage Recommendations
1. **Always check baseurl** in Jekyll `_config.yml` 
2. **Use full URL path** when baseurl is configured
3. **Monitor background processes** with `jobs` command
4. **Clean up processes** when done: `pkill -f jekyll`

### Process Management
```bash
# Check running background jobs
jobs

# Kill Jekyll specifically
pkill -f jekyll

# Kill by PID
kill -9 48636
```

This discovery significantly improves local development capabilities within Claude Code sessions.