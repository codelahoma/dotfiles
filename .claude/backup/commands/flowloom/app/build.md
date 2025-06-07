Build complete self-contained HTML applications with autonomous completion and interactive enhancement mode
Let input_args = "$ARGUMENTS"

# FlowLoom App Builder

Build complete self-contained HTML file applications from one-turn prompts using intelligent multi-Claude coordination when beneficial, or single-session execution when appropriate.

## Command Overview

This command takes a comprehensive app prompt and automatically determines whether to:
1. **Single Session**: Execute everything in the current session for simpler apps
2. **Multi-Session**: Coordinate multiple Claude instances for complex apps requiring specialized expertise

## Argument Interpretation

First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args contains "simple" or "basic": Force single-session mode
- If input_args contains "complex" or "enterprise": Force multi-session mode
- If input_args contains "oneturn": Force one-turn autonomous mode (enter One-Turn Mode first)
- If input_args contains "auto" or is empty: Automatically determine based on app complexity
- Otherwise: Treat entire input as the app description

## App Complexity Assessment

Analyze the app prompt to determine complexity:

**Single Session Indicators:**
- Simple interactive apps (calculators, games, utilities)
- Basic data visualization
- Single-page applications with local storage
- Straightforward user interfaces
- Standard web APIs (geolocation, canvas, etc.)

**Multi-Session Indicators:**
- Complex data visualization requiring specialized knowledge
- Advanced interactive features (games, simulations)
- Multiple integrated components (charts + forms + animations)
- Sophisticated algorithms or business logic
- Advanced web APIs or cutting-edge features

## Execution Strategy

### For One-Turn Mode Apps

Execute autonomously with minimal user interaction:
1. **Enter One-Turn Mode**: Activate `/flowloom:mode:oneturn` for optimal autonomous configuration
2. **Record start**: Persist timestamp and create build entity in memory  
3. **Autonomous analysis**: Understand requirements and make all technical decisions independently
4. **Autonomous design**: Choose architecture, styling, and implementation approach
5. **Autonomous implementation**: Build complete self-contained HTML file with all features
6. **Mandatory Puppeteer testing**: Systematically test ALL functionality with automated browser testing
7. **Quality assurance**: Ensure professional polish and user experience standards based on test results
8. **Present completed app**: Show final result with test report and feature description
9. **Wait for feedback**: User can approve immediately or request specific changes
10. **Final completion**: Persist timing data and completion status to memory

### For Single Session Apps

Execute directly in current session:
1. **Record start**: Persist timestamp and create build entity in memory
2. **Analyze** the app requirements thoroughly
3. **Design** the HTML file structure and styling
4. **Implement** as a single self-contained HTML file with embedded CSS and JavaScript
5. **Save** the HTML file to demo-gallery with appropriate category
6. **Test with Puppeteer**: Complete automated testing and validation
7. **Autonomous assessment**: Determine if app meets all requirements and is fully functional
8. **Present to user**: "I've completed building [app name]. It includes [key features]. Please review and let me know if you'd like any enhancements or if it's ready to finalize."
9. **Interactive mode**: 
   - If user says "done/looks good/approved": Record as one-turn completion
   - If user requests changes: Enter enhancement mode, iterate until satisfaction
10. **Final completion**: Persist end time and completion data to memory

### For Multi-Session Apps

Use FlowLoom coordination system:

@bash /coord:init app-builder-session

Then dispatch specialized tasks based on app requirements:

**Common Task Categories:**
- **Research**: Technology evaluation, best practices, security analysis
- **Architecture**: System design, database schema, API design
- **Frontend**: UI/UX design, component implementation, responsive design
- **Backend**: API development, business logic, data processing
- **DevOps**: Deployment, CI/CD, monitoring setup
- **Testing**: Test planning, implementation, validation
- **Documentation**: Technical docs, user guides, deployment instructions

**Task Dispatch Examples:**
```bash
# Research phase
/coord:dispatch "Research modern React patterns and state management for [app_type]" capabilities:frontend,research priority:high

# Architecture phase  
/coord:dispatch "Design scalable backend architecture for [app_requirements]" capabilities:backend,architecture priority:high

# Implementation phase
/coord:dispatch "Implement React frontend with [specific_features]" capabilities:frontend priority:medium

/coord:dispatch "Build Node.js API with [endpoints]" capabilities:backend priority:medium

# Quality assurance
/coord:dispatch "Create comprehensive test suite" capabilities:testing priority:medium

/coord:dispatch "Write deployment and user documentation" capabilities:documentation priority:low
```

**Coordination Workflow:**
1. **Initialize**: Start coordination session with app context and build timing
2. **Plan**: Break down app into specialized tasks
3. **Dispatch**: Assign tasks to appropriate workers based on capabilities
4. **Monitor**: Track progress with `/coord:status` 
5. **Integrate**: Combine results from all workers
6. **Validate**: Ensure all components work together
7. **Autonomous assessment**: Determine if coordinated app is fully functional
8. **Present to user**: Show completed app with all integrated components
9. **Interactive enhancement**: Work with user on any requested improvements
10. **Final delivery**: Package complete application with timing data for gallery

## Multi-Session Worker Guidance

**For workers joining the coordination session:**

1. **Announce capabilities**: `/worker:announce app-builder-session [your-skills]`
   - Example: `frontend,react,typescript` or `backend,nodejs,api,security`

2. **Check available tasks**: `/coord:status` to see what needs work

3. **Work on assigned tasks**: Focus on your area of expertise

4. **Submit results**: `/worker:complete [task_id]` with detailed deliverables

5. **Communicate issues**: Update task status if blockers or dependencies found

## Quality Standards

**Code Requirements:**
- Self-contained HTML file with embedded CSS and JavaScript
- Follow modern HTML5, CSS3, and ES6+ standards
- Include proper error handling and input validation
- Implement responsive design for mobile and desktop
- Use semantic HTML and accessible design patterns
- Include comprehensive comments explaining functionality

**Testing Requirements:**
- Automated Puppeteer testing for all functionality:
  - Navigate to file:// URL of created HTML
  - Test all buttons, inputs, and interactive elements
  - Verify data persistence (if applicable)
  - Test at mobile (375x667) and desktop (1920x1080) viewports
  - Capture screenshots of key states
  - Measure initial load time
  - Validate no console errors
- Generate comprehensive test report with:
  - Feature checklist with pass/fail status
  - Screenshots demonstrating functionality
  - Performance metrics
  - Any identified issues or limitations

**Deliverable Requirements:**
- Single HTML file that opens and runs in any modern browser
- No external dependencies or server requirements
- Clear instructions embedded as comments or help section
- Professional appearance with polished UI/UX

## Execution Mode Decision Matrix

**Use One-Turn Mode when:**
- Want autonomous development with minimal interaction
- App can be built in < 1 hour with clear requirements
- Simple to moderate complexity apps
- Personal projects or prototypes
- Prefer complete autonomous implementation

**Use Single Session when:**
- App can be built in < 2 hours
- Uses familiar, well-established patterns
- Limited scope and clear requirements
- Simple technology stack
- Want some interaction during development

**Use Multi-Session when:**
- App requires > 2 hours of development
- Multiple specialized skills needed
- Complex business logic or integrations
- Enterprise-grade requirements
- Team coordination would improve quality

## Execution Flow

1. **Record start time**: Persist user request timestamp to memory for transparency tracking
2. **Parse and analyze** the app prompt from input_args
3. **Assess complexity** using decision matrix
4. **Choose execution mode** (single vs multi-session)
5. **Execute selected strategy** following appropriate workflow
6. **Test with Puppeteer**: Validate all functionality using automated browser testing
7. **Autonomous completion assessment**: Determine if app is fully functional
8. **Present to user**: Show completed app and ask for approval/feedback
9. **Interactive enhancement mode**: Work with user on any requested changes
10. **Final completion**: Record end time when user confirms satisfaction

## Example Usage

```bash
# Autonomous one-turn development (minimal interaction)
/flowloom:app:build oneturn "Create a calculator with scientific functions"

# Simple app - will use single session  
/flowloom:app:build "Create a basic todo list app with local storage"

# Complex app - will use multi-session coordination  
/flowloom:app:build "Build a data visualization dashboard with interactive charts, real-time updates, and export functionality"

# Force single session for complex prompt
/flowloom:app:build simple "Build a multi-player tic-tac-toe game"

# Force multi-session for simple prompt (for learning/demonstration)
/flowloom:app:build complex "Create a basic calculator app"
```

## Build Process Timing and Memory Persistence

**Critical Timestamps to Track:**
1. **One-Turn Start**: When autonomous build begins
2. **One-Turn End**: When autonomous implementation completes
3. **Enhancement Start**: When user provides feedback/requests changes
4. **Enhancement End**: When user confirms final satisfaction

**Start of build process** (when user makes request):
```bash
# Get shell PID for consistent tracking
shell_pid=$(./bin/get_shell_pid.sh)
one_turn_start=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Create build session entity in memory with all four timestamp slots
app_name=$(echo "$input_args" | head -c 50 | tr ' ' '-' | tr -cd '[:alnum:]-')
build_entity="${app_name} HTML App Build"

# Create entity with initial timestamp
mcp__memory__create_entities '[
  {
    "name": "'"$build_entity"'",
    "entityType": "HTML App Build Session",
    "observations": [
      "Shell_ID: '"$shell_pid"' - '"$one_turn_start"' | ONE_TURN_START: Autonomous build initiated"
    ]
  }
]'

echo "🚀 App build started at: $one_turn_start"
echo "📝 Tracking: $build_entity"
```

**Memory tracking during build**:
```bash
# Add observations for major milestones
# - Initial analysis completion
# - Implementation completion  
# - Testing completion
# - Autonomous assessment completion
```

**Autonomous completion** (when implementation finished):
```bash
# Record one-turn completion timestamp
one_turn_end=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Add one-turn completion to memory
mcp__memory__add_observations '[
  {
    "entityName": "'"$build_entity"'",
    "contents": [
      "Shell_ID: '"$shell_pid"' - '"$one_turn_end"' | ONE_TURN_END: Autonomous implementation completed"
    ]
  }
]'
```

**Interactive enhancement phase**:
```bash
# When user provides feedback (if not immediately approved)
if [[ user requests changes ]]; then
    enhancement_start=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    
    # Record enhancement start
    mcp__memory__add_observations '[
      {
        "entityName": "'"$build_entity"'",
        "contents": [
          "Shell_ID: '"$shell_pid"' - '"$enhancement_start"' | ENHANCEMENT_START: Interactive enhancement mode activated"
        ]
      }
    ]'
fi
```

**Final completion** (when user confirms satisfaction):
```bash
enhancement_end=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Record final completion timestamp
mcp__memory__add_observations '[
  {
    "entityName": "'"$build_entity"'",
    "contents": [
      "Shell_ID: '"$shell_pid"' - '"$enhancement_end"' | ENHANCEMENT_END: User confirmed final satisfaction",
      "Shell_ID: '"$shell_pid"' - '"$enhancement_end"' | BUILD_STATUS: Completed with user approval"
    ]
  }
]'

echo "✅ App build completed at: $enhancement_end"
echo "📊 All four timestamps tracked in memory graph for gallery metadata"
```

**Memory persistence structure**:
- Build session entity with all four critical timestamps:
  1. ONE_TURN_START: When autonomous build begins
  2. ONE_TURN_END: When autonomous implementation completes  
  3. ENHANCEMENT_START: When user feedback triggers enhancement mode
  4. ENHANCEMENT_END: When user confirms final satisfaction
- App details and complexity assessment
- Build status and user approval confirmation
- Enhancement iterations count for transparency
- Searchable Shell_ID and UTC timestamp format for consistency

## Puppeteer Test Template

```javascript
// Example test structure for each app
async function testApp() {
    // Navigate to the HTML file
    await page.goto('file:///path/to/app.html');
    
    // Test initial state
    await page.screenshot({ path: 'initial-state.png' });
    
    // Test each major feature
    // Example: Click buttons, fill forms, verify results
    
    // Test responsive design
    await page.setViewport({ width: 375, height: 667 }); // Mobile
    await page.screenshot({ path: 'mobile-view.png' });
    
    await page.setViewport({ width: 1920, height: 1080 }); // Desktop
    await page.screenshot({ path: 'desktop-view.png' });
    
    // Verify no console errors
    const errors = [];
    page.on('console', msg => {
        if (msg.type() === 'error') errors.push(msg.text());
    });
    
    return {
        success: errors.length === 0,
        errors: errors,
        screenshots: ['initial-state.png', 'mobile-view.png', 'desktop-view.png']
    };
}
```

## Autonomous Completion Criteria

**I will determine an app is "fully functional" when:**
- All core features from the prompt are implemented and working
- The app handles edge cases and provides appropriate user feedback
- Responsive design works correctly on mobile and desktop
- No console errors or broken functionality
- The app provides a professional, polished user experience
- All interactive elements are intuitive and accessible

**Presentation to User:**
After autonomous completion, I will present the app with:
"I've completed building [app description]. The app includes [list key features and capabilities]. It has been tested and is fully functional. Please review the application and let me know:
- If you're satisfied and ready to finalize it
- If you'd like any enhancements or modifications

The app is located at: [file path]"

**Interactive Enhancement Mode:**
- Work collaboratively on any requested changes
- Maintain the same quality standards for all modifications
- Continue until user explicitly confirms satisfaction
- Track all enhancement iterations for transparency

Begin execution based on the app prompt provided in input_args.