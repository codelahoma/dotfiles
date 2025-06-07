Enter Configuration Mode for Claude commands and settings management

I can assist you with:

🔧 **Command Development**
- Creating new coordination commands or improving existing ones
- Organizing commands into logical categories
- Testing command functionality and argument handling
- Managing the .claude/commands/ structure

📋 **Configuration Management**
- CLAUDE.md (project-wide instructions)
- CLAUDE.local.md (user-specific settings)
- .claude/settings.json (project settings)
- .mcp.json (MCP server configurations)

🎯 **Current Context**

From now on, I'll primarily focus on working with Claude's slash command system, building effective commands, and managing the complete Claude configuration ecosystem including CLAUDE.md, CLAUDE.local.md, .claude.json, settings files, and MCP configurations.

## Configuration Management Focus Areas

1. Creating well-structured command files
2. Organizing commands into logical categories 
3. Using variables like $ARGUMENTS effectively
4. Testing and refining command functionality
5. Managing CLAUDE.md configuration (project-wide instructions)
6. Managing CLAUDE.local.md configuration (user-specific instructions)
7. Managing .claude.json and settings files
8. Configuring MCP servers at appropriate scopes
9. Ensuring consistency between commands and configuration files
10. Security management for sensitive configuration data

## Key Information About Custom Slash Commands

### Command Structure
- **Active commands** are markdown files in `.claude/commands/` (project root)
- **FlowLoom templates** are in `${FLOWLOOM_WORK_DIR:-.meta-claude}/` (installed FlowLoom repo)
- Subdirectories create command categories
- Example: `.claude/commands/docs/daily.md` → `/project:docs:daily`

### Command Format
- Start with clear instructions for Claude on how to respond
- ALWAYS write commands in the second person, addressing the user directly as "you"
- Use `$ARGUMENTS` placeholder to capture user input
- Structure commands with step-by-step instructions
- Include expected output format
- Use imperative, direct language for clarity
- **IMPORTANT**: When using `@bash` directives, always follow with explicit instructions for Claude to display/process the output, otherwise Claude Code will add caveat warnings

### Processing Command Arguments

**IMPORTANT:** Always follow the argument processing pattern defined in `${FLOWLOOM_WORK_DIR:-.meta-claude}/.claude/templates/argument_processor.md`.

#### Standard Approach:
1. Begin every command file with: `Let input_args = "$ARGUMENTS"`
2. Use `input_args` throughout your command for argument handling
3. Include an "Argument Interpretation" section that explains how to process the arguments
4. Document expected argument patterns without including the command name

#### Example Implementation:
```
Let input_args = "$ARGUMENTS"

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Use default settings
- If input_args is "today": Focus on today's items
- If input_args starts with "file:": Process the specific file

## Argument Patterns
- (no arguments) - Default behavior
- `today` - Process today's items
- `yesterday` - Process yesterday's items
- `file:path/to/file.py` - Process specific file
```

For complete guidance, see `${FLOWLOOM_WORK_DIR:-.meta-claude}/.claude/templates/argument_processor.md`

## Current Command Structure

Commands are organized as follows:
- `docs/` - Documentation commands
- `impl/` - Implementation commands  
- `plan/` - Planning commands
- `rev/` - Review commands
- `mode/` - Mode-switching commands

## Command Creation Process

1. Identify purpose and functionality
2. Choose appropriate category and name
3. Create markdown file with clear instructions
4. Include examples and documentation
5. **For commands with `@bash` directives**: Always add explicit instructions after the bash block telling Claude what to do with the output
6. Test with various inputs
7. Refine based on real-world usage
8. **Update the static cheatsheet** at `${FLOWLOOM_WORK_DIR:-.meta-claude}/docs/commands-cheatsheet.md`

**IMPORTANT:** Whenever you create, modify, or remove commands, you MUST update the static cheatsheet at `${FLOWLOOM_WORK_DIR:-.meta-claude}/docs/commands-cheatsheet.md` to keep it current. This ensures the `/project:commands` command provides accurate, fast command reference.

### Key Directory Distinction for Meta-Development
Since FlowLoom is installed in `${FLOWLOOM_WORK_DIR:-.meta-claude}/` to develop itself:
- **Work on active commands:** Edit files in `.claude/commands/` 
- **Reference FlowLoom templates:** Read from `${FLOWLOOM_WORK_DIR:-.meta-claude}/`
- **Sync commands:** Use tools to copy from `${FLOWLOOM_WORK_DIR:-.meta-claude}/` to `.claude/commands/`

## Configuration File Management

### Documentation Configuration Files

#### CLAUDE.md (Project Configuration)
- Contains project-wide instructions and standards
- Checked into version control
- Should include technical environment, command references, coding standards, testing guidelines
- Keep comprehensive but focused on essential project information

#### CLAUDE.local.md (User Configuration) 
- Contains user-specific preferences and local instructions
- NOT checked into version control
- Should include personal workflow preferences, local command shortcuts, development environment specifics
- Keep concise and focused on user-specific overrides

### Claude Configuration Files

#### .claude.json (Home Level: ~/.claude.json)
- Global Claude configuration for all projects
- User-specific settings and MCP servers
- Private configuration, not shared with teams
- Contains:
  - Global MCP server configurations
  - User preferences and authentication
  - API keys and credentials
  - Theme settings and startup configurations

#### .claude/settings.json (Project Level)
- Project-specific Claude Code settings
- Can be checked into version control
- Shared with team members
- Contains:
  - Tool permissions and allowlists
  - Project-specific environment variables
  - Team-shared MCP configurations
  - Local development preferences

#### .claude/settings.local.json (Project Level, Git-ignored)
- Local overrides for project settings
- NOT checked into version control
- User-specific project configurations
- Overrides both global and project settings

#### .mcp.json (Project Root)
- MCP server configurations for the project
- Typically checked into version control
- Shared with entire development team
- Example structure:
```json
{
  "mcpServers": {
    "shared-server": {
      "command": "/path/to/server",
      "args": [],
      "env": {
        "API_KEY": "value"
      }
    }
  }
}
```

### Configuration Hierarchy and Precedence

**Claude Code Settings (Highest to Lowest Precedence):**
1. Enterprise managed policies (`/Library/Application Support/ClaudeCode/policies.json` on macOS)
2. Command line arguments
3. Local project settings (`.claude/settings.local.json`)
4. Shared project settings (`.claude/settings.json`)
5. User settings (`~/.claude/settings.json`)

**MCP Server Configuration Scopes:**
1. **Local Scope** (User-specific, project-only): Stored in user settings, not shared
2. **Project Scope** (Team-shared): Stored in `.mcp.json`, checked into VCS
3. **User Scope** (Cross-project): Stored in `~/.claude.json`, available across all projects

### Configuration Management Commands
- `claude config list` - View current configuration
- `claude config get <key>` - Get specific configuration value
- `claude config set <key> <value>` - Set configuration value
- `claude mcp add <name> <path>` - Add MCP server
- `claude mcp add <name> -s project <path>` - Add project-scoped MCP server

### Configuration Best Practices
- Keep sensitive information (API keys) in user-level configs, not project configs
- Use project-level `.mcp.json` for team-shared MCP servers
- Document team-shared configurations in CLAUDE.md
- Use `.claude/settings.local.json` for personal overrides that shouldn't be shared
- Ensure commands align with configuration instructions
- Update configurations when project practices change
- Maintain consistency between different configuration levels

When the user asks about commands, configuration files, or how to create/modify them, be exceptionally helpful and detailed in your responses. Offer to help with specific aspects of:

- **Command Creation**: Structuring files, handling arguments, organizing commands effectively
- **Configuration Management**: CLAUDE.md/CLAUDE.local.md editing, .claude.json setup, settings files
- **MCP Configuration**: Server setup, scope management, team sharing
- **Security**: Proper placement of sensitive data, configuration validation
- **Integration**: Ensuring all configuration files work together harmoniously