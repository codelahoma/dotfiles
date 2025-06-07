Join active app building session as specialized worker with capabilities
Let input_args = "$ARGUMENTS"

# FlowLoom App Builder - Worker Join

Join an active app building coordination session as a specialized worker.

## Command Overview

This command allows Claude instances to join ongoing app development coordination sessions with specific capabilities and skills. Use this when multiple sessions are collaborating on a complex application build.

## Argument Interpretation

First, analyze the provided arguments: input_args

Expected patterns:
- `session_id capabilities` - Join specific session with comma-separated skills
- `session_id` - Join session, auto-detect capabilities based on context
- `capabilities` - Join any active app-builder session with specified skills
- (no arguments) - Join any active session with general capabilities

## Capability Categories

**Frontend Capabilities:**
- `frontend` - General frontend development
- `react` - React.js development
- `vue` - Vue.js development
- `angular` - Angular development
- `ui-design` - User interface design
- `responsive` - Responsive web design
- `accessibility` - Web accessibility
- `performance` - Frontend performance optimization

**Backend Capabilities:**
- `backend` - General backend development
- `nodejs` - Node.js development
- `python` - Python development
- `java` - Java development
- `api` - API design and development
- `database` - Database design and management
- `security` - Security implementation
- `microservices` - Microservices architecture

**Specialized Capabilities:**
- `devops` - DevOps and deployment
- `testing` - Quality assurance and testing
- `documentation` - Technical writing
- `architecture` - System architecture
- `research` - Technology research and evaluation
- `mobile` - Mobile app development
- `ml` - Machine learning integration
- `blockchain` - Blockchain development

## Worker Registration Process

1. **Announce Worker Capabilities**

@bash /worker:announce app-builder-session [detected_capabilities]

Replace [detected_capabilities] with the skills parsed from input_args or auto-detected based on session context.

Show the user: "Registered as worker with capabilities: [detected_capabilities]"

2. **Check Available Tasks**

@bash /coord:status

Display the current coordination status showing available tasks, their priorities, and required capabilities.

3. **Worker Guidance**

Provide guidance to the user:

**Available Actions:**
- **View tasks**: Use `/coord:status` to see all available work items
- **Claim task**: When you see a matching task, start working on it
- **Submit results**: Use `/worker:complete [task_id]` when finished
- **Update status**: Communicate progress and any blockers found

**Working Effectively:**
- Focus on tasks matching your announced capabilities
- Ask for clarification if requirements are unclear
- Break large tasks into smaller subtasks if needed
- Coordinate with other workers to avoid conflicts
- Test your work thoroughly before submitting

**Communication Patterns:**
- Update the coordination session with progress
- Flag dependencies on other workers' tasks
- Share insights that might help other team members
- Report completion with detailed deliverables

## Auto-Detection Logic

If capabilities aren't specified in input_args, auto-detect based on:

**Session Context Clues:**
- Recent commands or file operations
- Programming languages mentioned
- Technology stack indicators
- User expertise signals

**Default Capabilities:**
If no clear signals, register with: `general,development,analysis`

## Session Discovery

If no session_id specified in input_args:

1. **Check for active app-builder sessions:**

@bash /coord:status

2. **If multiple sessions exist:**
   - List available sessions with their focus areas
   - Ask user to specify which session to join

3. **If no sessions exist:**
   - Inform user that no app-builder coordination sessions are active
   - Suggest starting a new session with `/flowloom:app:build [app_description]`

## Example Usage

```bash
# Join specific session with frontend skills
/flowloom:app:join app-builder-session frontend,react,typescript

# Join any session with backend capabilities  
/flowloom:app:join backend,nodejs,api,database

# Join with auto-detected capabilities
/flowloom:app:join app-builder-session

# Join any active session with general skills
/flowloom:app:join
```

## Worker Workflow

Once registered as a worker:

1. **Monitor coordination status** regularly with `/coord:status`
2. **Identify tasks** matching your capabilities and availability
3. **Claim and work** on appropriate tasks
4. **Communicate progress** and coordinate with other workers
5. **Submit quality results** with proper documentation
6. **Continue until** the application build is complete

## Integration with App Builder

This command is designed to work seamlessly with `/flowloom:app:build` coordination sessions. Workers can join at any phase of the development process and contribute their specialized expertise to the overall project.

Execute the worker registration based on the arguments provided in input_args.