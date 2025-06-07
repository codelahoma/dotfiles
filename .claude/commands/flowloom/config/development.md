# FlowLoom Development Configuration

This file provides FlowLoom development configuration for working within the FlowLoom repository itself.

## Auto-Detection Command System

**CRITICAL**: Before responding to any user input, first check if the input matches a slash command pattern. If it does, automatically execute the corresponding slashload command instead of responding normally.

### Patterns to Auto-Detect:
- `flowloom:category:command [args]` → `slashload flowloom/category/command [args]` (explicit namespace)
- `category:command [args]` → `slashload flowloom/category/command [args]` (default to FlowLoom namespace)
- `project:category:command [args]` → `slashload flowloom/category/command [args]` (legacy support)
- `category/command [args]` → `slashload flowloom/category/command [args]` (slash format)
- `mode_name` (single word matching known modes) → `slashload flowloom/mode/mode_name`
- `fl[pattern]` (fuzzy matching) → `slashload flowloom/system/fuzzy-match [pattern]` (e.g., "flmemstat", "flplanrev")

### Known Categories and Commands:
- **Categories**: app, config, coord, dev, docker, docs, files, git, impl, memory, mode, plan, rev, session, shell, specs, system, website, worker
- **Modes**: config, workflow, pair, documentation, code_review, files, specification, story, select, session
- **Examples**: "plan:high" → `slashload flowloom/plan/highlevel`, "coord:init" → `slashload flowloom/coord/init`, "worker:status" → `slashload flowloom/worker/status`

## Development-Specific Customizations

Development-specific overrides and extensions to the base FlowLoom configuration go here.

## Git Sync Shortcut
When the user types just "sync", execute the `/project:git:sync` slash command which performs:
1. **Configuration & Command Sync**: Back up CLAUDE.md, CLAUDE.local.md, .mcp.json, settings, and sync .claude commands
2. **Git Repository Sync**: Check for uncommitted changes, auto-commit with generated messages, pull and push
3. **Optional Message Refinement**: Offer to amend commit messages after auto-commit

This provides comprehensive synchronization of configuration, commands, and git repository in a single streamlined workflow.

## Git Operations 
**Use standard bash git commands for all git operations** - MCP git server has been removed due to corruption issues:

### Standard Git Commands (Use These):
- `git status` - Repository status
- `git add` - Stage files
- `git commit` - Create commits  
- `git diff` - Show differences
- `git log` - View commit history
- `git show` - Show commit details
- `git branch` - Branch management
- `git checkout` - Switch branches
- `git reset` - Reset changes
- `git pull` - Pull from remote
- `git push` - Push to remote
- `git fetch` - Fetch from remote
- `git merge` - Merge branches
- `git rebase` - Rebase operations

**Note: MCP git server was removed after causing repository corruption during recovery operations.**

## MCP Server Usage Preferences

When multiple tools provide similar functionality, prefer MCP servers in the following order:

### File Operations
1. **Prefer `mcp__filesystem__*` over built-in file tools** for:
   - `mcp__filesystem__read_file` instead of `Read`
   - `mcp__filesystem__read_multiple_files` for batch reading
   - `mcp__filesystem__write_file` instead of `Write`
   - `mcp__filesystem__edit_file` instead of `Edit` or `MultiEdit`
   - `mcp__filesystem__list_directory` instead of `LS`
   - `mcp__filesystem__search_files` instead of `Glob`/`Grep` for simple searches
   - `mcp__filesystem__create_directory` for directory creation

2. **Use built-in tools when**:
   - Complex grep patterns are needed (use `Grep` tool)
   - Line-specific editing is required (use `Edit` tool)
   - Notebook editing is needed (use `NotebookEdit`)

### Web Operations
1. **Prefer `mcp__puppeteer__*` over `WebFetch`** for:
   - Web page scraping and content extraction
   - Handling complex JavaScript-rendered pages
   - Dealing with redirects or authentication
   - Taking screenshots or generating PDFs

2. **Use built-in `WebSearch` for**:
   - General web searches
   - Finding current information
   - Current events and articles

### Git Operations
**Use standard bash git commands for all git operations** - MCP git server removed due to corruption issues.

### GitHub/GitLab Operations
1. **Always use `mcp__github__*` for GitHub API operations**:
   - Creating/updating issues and PRs
   - Repository searches and file access
   - Managing branches and commits

### Memory and Knowledge Management

#### Memory Read/Write Strategy
**CRITICAL**: For all memory.json operations, prefer Memory Monitor for reads and MCP memory server for writes:

**Read Operations (use Memory Monitor CLI)**:
- **Query memory data**: Use `uv run flowmon query "SQL query"` for structured queries
- **Monitor changes**: Use `uv run flowmon watch` for real-time monitoring  
- **Search entities**: Use `uv run flowmon search "pattern"` for entity searches
- **Memory stats**: Use `uv run flowmon stats` for memory analytics
- **Export data**: Use `uv run flowmon export` for data extraction

**Write Operations (use MCP memory server)**:
- **Create entities**: Use `mcp__memory__create_entities`
- **Add observations**: Use `mcp__memory__add_observations`
- **Create relations**: Use `mcp__memory__create_relations`
- **Delete operations**: Use `mcp__memory__delete_*` functions

**Performance Benefits**: Memory Monitor provides SQL interface, real-time monitoring, and analytics without MCP context limits.

#### Dual Memory System Strategy
FlowLoom uses two complementary memory systems for comprehensive knowledge management:

1. **Use Memory Monitor + MCP memory (memory.json) for**:
   - **Structured Data**: Entities, relationships, and observations
   - **Project Context**: Components, status, dependencies
   - **Fact-Based Knowledge**: "What exists" and "How things connect"
   - **Examples**:
     - Entity: "FlowLoom Docker Component" with observations about implementation status
     - Relation: "Docker Component" depends_on "MCP Architecture"
   - **File**: Persists to `memory.json` (version-controlled)

2. **Use `mcp__basic-memory__*` (.knowledge/) for**:
   - **Narrative Knowledge**: Long-form documentation and explanations
   - **Design Decisions**: Architecture choices, rationale, alternatives considered
   - **How-To Guides**: Implementation patterns, troubleshooting steps
   - **Team Wisdom**: Lessons learned, best practices discovered
   - **Examples**:
     - Article: "Docker Permissions Design Decision"
     - Guide: "Setting Up Multi-Agent Coordination"
   - **Files**: Persists as Markdown in `.knowledge/` directory

#### Integration Pattern
1. **Create entities** in memory.json for trackable components (via MCP)
2. **Write articles** in basic-memory for detailed explanations  
3. **Cross-reference** by mentioning entity names in articles
4. **Search both** when looking for information:
   - Use Memory Monitor (`uv run flowmon search`) for finding entities/relationships
   - Use `mcp__basic-memory__search_notes` for finding documentation

#### Example Workflow
```
# Working on new feature
1. Create entity: "Authentication System" (memory.json)
2. Add observations as you build
3. When making decisions, write: "Authentication Strategy" (.knowledge/)
4. Entity observations can reference: "See Authentication Strategy doc"
```

### Database Operations
1. **Always use `mcp__sqlite__*` for**:
   - All SQLite database operations
   - Structured data storage and queries

### General Guidelines
- MCP servers provide better integration and consistency
- They often have better error handling and validation
- They respect project boundaries and permissions
- When in doubt, check if an MCP server provides the functionality first

## Claude Interaction Guidelines
- You should tell me your current working directory after each interaction
- You should indicate any active modes (Configuration Mode, Workflow Mode, etc.) you are currently in
- You should include a one-line description of what the current session is working on
- You should show the current session ID (if available) for continuity tracking
- You should show the current git branch
- You should include next steps based on current context and state
- When multiple next steps exist, number them (1, 2, 3, etc.) for easy selection
- Format these five status items with newlines between each (no blank lines) for compact clarity
- Bold all labels (Working directory:, Active mode:, etc.) using **bold** markdown
- Order: working directory, then active mode, then session context, then session ID, then git branch, then next steps
- Include usage hint in next steps label: "Next steps (type 'go' for all, or numbers like '1 3'):"
- When the user types only "go", automatically execute the next step from the footer
- When the user types a number (1, 2, 3, etc.), execute that numbered option from the next steps
- When the user types "go" and there are numbered options, execute all numbered options in sequential order (1, then 2, then 3, etc.)

### Automatic Memory Graph Tracking
**IMPORTANT**: Before displaying the footer, automatically track significant activities in the memory graph using consistent shell ID detection and timestamps:

**Timestamp Format**: Use ISO 8601 UTC format for all memory observations: `YYYY-MM-DDTHH:MM:SSZ`
Example: `2025-05-27T15:30:45Z`

**Shell ID Detection**: Use the centralized shell PID detection script:
```bash
# All FlowLoom components use the same shell PID detection
shell_pid=$(./bin/get_shell_pid.sh)

# Alternative: Export as environment variable
eval "$(./bin/get_shell_pid.sh --export)"
# Then use: $SHELL_PID

# For debugging shell detection:
# ./bin/get_shell_pid.sh --debug
```

**Project Facts Tracking**: Always maintain "FlowLoom Project Configuration" entity with ephemeral project facts:
- GitHub username, repository URLs, developer names
- API keys, account identifiers, service endpoints  
- Project-specific constants, naming conventions
- Team member information, contact details
- Environment-specific configuration values

Update this entity whenever new project facts are discovered or corrected.

1. **For Implementation Work**: When session context indicates implementation, testing, or feature development:
   - Use `mcp__memory__add_observations` to update relevant entities with progress
   - Format observations with timestamp and shell PID: "Shell_ID: $shell_pid - $(date -u +%Y-%m-%dT%H:%M:%S)Z | [activity description]"
   - Create new entities for significant components or features being worked on
   - Update relationships when dependencies or integrations are discovered

2. **For Problem Solving**: When session context indicates troubleshooting, debugging, or issue resolution:
   - Create incident entities for problems encountered
   - Add observations about solutions found and their effectiveness
   - Format observations with timestamp: "Shell_ID: $shell_pid - $(date -u +%Y-%m-%dT%H:%M:%S)Z | Problem: [description] | Solution: [resolution]"
   - Link solutions to related components or systems

3. **For Documentation Work**: When session context indicates writing guides, plans, or documentation:
   - Create entities for new knowledge areas or methodologies discovered
   - Update existing entities with new insights or status changes
   - Cross-reference between structured (memory.json) and narrative (.knowledge/) systems
   - Format observations: "Shell_ID: $shell_pid - $(date -u +%Y-%m-%dT%H:%M:%S)Z | Documentation: [type] | Coverage: [topics]"

4. **For Configuration Changes**: When session context indicates setup, configuration, or system changes:
   - Track changes to FlowLoom components and their configuration states
   - Document decisions made and their rationale
   - Update system architecture relationships
   - Format observations: "Shell_ID: $shell_pid - $(date -u +%Y-%m-%dT%H:%M:%S)Z | Config: [component] | Change: [description]"

**Memory Tracking Triggers:**
- Significant milestones or completions mentioned in session context
- New problems identified or solutions discovered
- Changes to system architecture or configuration
- Important decisions made or insights gained
- Integration points or dependencies identified

**Shell PID Consistency**: All memory operations should use the same shell PID detection method as session management for consistent cross-referencing between session metadata and memory graph entries.

This ensures the knowledge graph automatically captures development activity with proper session correlation without requiring explicit memory commands for routine progress tracking.

**Footer Integration**: Always perform the appropriate memory tracking operations BEFORE displaying the interactive footer with working directory, active mode, session context, git branch, and next steps.

## Knowledge System Reference

When the user refers to "your knowledge" they mean both:
1. **Knowledge Graph** (memory.json) - Structured entities, relationships, and observations
2. **Knowledge Docs** (.knowledge/) - Narrative documentation and explanations

Search and reference both systems when the user asks about "your knowledge" or what you know about topics.

# important-instruction-reminders
Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary for achieving your goal.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.
# important-instruction-reminders
Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary for achieving your goal.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.

## System Reload
slashload system:reload