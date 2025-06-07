Let input_args = "$ARGUMENTS"

You will create comprehensive documentation based on the specified scope and type, leveraging advanced documentation patterns for technical walkthroughs, multi-document systems, and GitHub integration.

## Command Purpose
This enhanced command creates sophisticated documentation systems ranging from single document updates to comprehensive technical walkthroughs with live code integration, professional diagrams, and systematic cross-referencing.

## Argument Interpretation
First, analyze the provided arguments: input_args

Enhanced argument patterns:
- `file:path.md` - Update single document with GitHub integration
- `system:feature-name` - Create multi-document system with master index
- `diagram:name.mmd` - Create/update diagram with size-controlled SVG
- `technical:scope` - Technical walkthrough with code integration
- `standards` - Update documentation standards and templates
- Time-based: `today`, `yesterday`, `week` - Changes since timeframe
- Commit-based: `abc123f` - Changes from specific commit
- Branch-based: `branch ComponentName` - All branch changes for component

## Argument Patterns
- `file:README.md` - Single document update with enhanced linking
- `system:oauth-integration` - Multi-document system for OAuth feature
- `diagram:oauth-flow` - Create OAuth flow diagram with size control
- `technical:database-changes` - Technical walkthrough of DB modifications
- `standards` - Update documentation templates and standards
- `today` - Document today's changes with GitHub integration
- `week` - Comprehensive week's changes analysis
- `branch AUP-1250` - Complete branch documentation system

@bash git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "main"

Determine the current branch and extract ticket ID for proper organization.

@bash git merge-base HEAD origin/master 2>/dev/null || echo "No merge base found"

Find the branch base commit to understand the full scope of changes in this feature branch.

@bash git log --oneline $(git merge-base HEAD origin/master 2>/dev/null || echo "HEAD~10")..HEAD --no-merges 2>/dev/null || echo "No commits found since branch base"

Review ALL commits added since the branch diverged from origin/master to understand the complete scope of changes.

@bash git diff --name-only $(git merge-base HEAD origin/master 2>/dev/null || echo "HEAD~10")..HEAD 2>/dev/null | head -20 || echo "No changed files found"

Identify all files changed since the branch base to understand the comprehensive scope of modifications.

@bash find plans -name "*.md" -type f 2>/dev/null | head -10 || echo "No plan files found"

Locate all applicable plans that provide context for the changes and implementation approach.

@bash find plans -name "*.md" -exec grep -l "COMPLETE\|IMPLEMENTATION COMPLETE\|✅.*COMPLETE" {} \; 2>/dev/null | head -5 || echo "No completed plans found"

Identify completed plans which should be given highest priority as authoritative sources for what was actually implemented.

Read each plan file to understand the complete context and implementation approach. **CRITICAL**: Give precedence to plans that are (1) marked as completed and (2) more recently modified. Newer completed plans are the authoritative source, while older plans provide historical context. Look for completion markers like "IMPLEMENTATION COMPLETE" and recent modification dates to determine plan priority and relevance.

## Documentation System Types

### 1. Single Document Updates (`file:path.md`)
- **GitHub Integration**: Add permalink references to specific code sections
- **Enhanced Linking**: Cross-references with other documentation
- **Consistent Formatting**: Apply project standards and templates
- **Code Integration**: Live links to implementation details

### 2. Multi-Document Systems (`system:feature-name`)
- **Master Index Creation**: Comprehensive navigation hub with:
  - Overview and scope summary
  - Hierarchical document organization (10-series, 20-series, etc.)
  - Cross-referenced navigation by component, complexity, development phase
  - Key metrics and achievements summary
- **Systematic Coverage**: 
  - Core Architecture documents (10-13 series)
  - Implementation Deep Dives (20-29 series)  
  - Frontend & UX (30-39 series)
  - Testing & Quality (40-49 series)
  - Advanced Features (50-59 series)
  - Infrastructure (60-69 series)
- **GitHub Integration**: Commit permalinks and live code references throughout
- **Professional Standards**: Consistent formatting, navigation, and quality control

### 3. Technical Walkthroughs (`technical:scope`)
- **Live Code Integration**: GitHub permalinks to specific commits and line ranges
- **Progressive Complexity**: Organized from architecture through implementation details
- **Comprehensive Coverage**: Models, APIs, testing, security, performance
- **Professional Diagrams**: Size-controlled SVGs with source management
- **Cross-Component Analysis**: Integration points and relationships

### 4. Diagram Management (`diagram:name.mmd`)
- **Source Control**: Store `.mmd` files alongside compiled `.svg` assets
- **Size Control**: Professional rendering with controlled dimensions
  ```bash
  # Complex diagrams (600px display)
  mmdc -i diagram.mmd -o diagram.svg -w 800 -H 600
  ```
- **HTML Embedding**: `<img src="diagrams/diagram.svg" alt="Description" width="600">`
- **Consistent Standards**: Size guidelines by diagram type and complexity

### 5. Standards Management (`standards`)
- **Template Updates**: Refresh documentation templates and patterns
- **Cross-Reference Maintenance**: Update linking systems and navigation
- **Quality Control**: Consistency checking across document types
- **GitHub Integration Standards**: Permalink patterns and code reference formats

## Implementation Process

### Phase 1: Comprehensive Analysis and Planning

1. **Branch and Scope Analysis**:
   - Extract ticket ID from current branch name
   - Determine branch base using `git merge-base HEAD origin/master`
   - Analyze ALL commits since branch divergence for complete understanding
   - Identify all files changed across the entire feature branch

2. **Plan Review and Context Building**:
   - Read all applicable plans in `plans/[TICKET-ID]/` directory
   - Review plan evolution from initial architecture through detailed implementation
   - Understand original intentions vs. actual implementation
   - Identify key architectural decisions and pivot points

3. **Comprehensive Change Analysis**:
   - Review complete git diff since branch base: `git diff $(git merge-base HEAD origin/master)..HEAD`
   - Analyze file additions, modifications, and deletions across entire branch
   - Understand implementation scale (commits, files changed, insertions/deletions)
   - Map changes to plan phases and implementation milestones

4. **Documentation Type Determination**:
   - Based on input_args pattern and scope of changes
   - Consider complexity and scale of implementation
   - Determine appropriate documentation system type
   - Plan content organization and structure

Display comprehensive analysis results:
```markdown
## Branch Analysis Summary
- **Branch**: [branch-name] 
- **Ticket ID**: [extracted-id]
- **Base Commit**: [merge-base-hash]
- **Total Commits**: [count] commits since branch base
- **Files Changed**: [count] files across [categories]
- **Scale**: +[insertions] -[deletions] lines
- **Plans Found**: [count] plan files providing context
- **Implementation Scope**: [assessment based on scale and complexity]
```

5. **Plan Analysis and Integration**:
   ```bash
   # Find all plans for the ticket, sorted by modification time (newest first)
   find plans -name "*[TICKET-ID]*" -type f -exec ls -lt {} + | head -10
   ```
   
   **Plan Review Priority Order**:
   1. **Completed plans** (marked with "COMPLETE", "IMPLEMENTATION COMPLETE", or similar status)
   2. **Most recently modified plans** (indicating active work and latest thinking)
   3. **Higher numbered plans** (e.g., 220_ over 110_ - more detailed/recent planning)
   4. **Implementation plans over architecture plans** (220_implementation over 100_architecture)
   
   For each plan file found, in priority order:
   - **Check completion status**: Look for completion markers, status updates, final summaries
   - **Note modification date**: Recent modifications indicate current relevance
   - **Read complete plan content**: Understand the intended approach and evolution
   - **Identify plan hierarchy**: Architecture → High-level → Detailed → Subsections (100→110→120→121)
   - **Map planned vs actual**: Connect plan phases to implemented commits and changes
   - **Document deviations**: Note scope changes, additional features, pivot points
   
   **Plan Integration Strategy**:
   - **Completed plans**: Use as authoritative source for what was actually implemented
   - **Recent incomplete plans**: May indicate ongoing work or future phases
   - **Older plans**: Provide historical context but may be superseded
   - **Conflicting information**: Newer and completed plans take precedence

6. **Comprehensive File Analysis**:
   ```bash
   # Get detailed statistics about the branch changes
   git diff --stat $(git merge-base HEAD origin/master)..HEAD
   ```
   
   Analyze the scope and categorize changes:
   - **Database/Models**: Migrations, model changes, schema evolution
   - **API/Services**: Endpoint changes, service layer modifications
   - **Frontend**: Component updates, UI enhancements
   - **Testing**: Test additions, coverage improvements
   - **Configuration**: Settings, deployment, infrastructure changes
   - **Documentation**: README updates, inline documentation

### Phase 2: Structure Creation and Planning

7. **Create Directory Structure**:
   ```bash
   # For multi-document systems
   mkdir -p walkthroughs/[TICKET-ID]/code-walkthrough/diagrams
   
   # For single updates
   # Work with existing file structure
   ```

4. **GitHub Integration Setup**:
   - Determine current commit hash for permalinks
   - Identify repository structure for accurate linking
   - Plan code reference strategy

### Phase 3: Content Generation

5. **Master Index Creation** (for system: type):
   - Comprehensive overview with scope and achievements
   - Hierarchical navigation with multiple access patterns
   - Key metrics and implementation statistics
   - Professional formatting with consistent standards

6. **Technical Documentation** (for technical: type):
   - Live code integration with GitHub permalinks
   - Progressive complexity organization
   - Professional diagrams with size control
   - Cross-component analysis and integration points

7. **Diagram Creation** (for diagram: type):
   ```bash
   # Create Mermaid source
   cat > diagrams/oauth-flow.mmd << 'EOF'
   flowchart TD
       A[User Authentication] --> B[OAuth Flow]
       B --> C[Token Management]
       C --> D[API Integration]
   EOF
   
   # Compile with size control
   mmdc -i diagrams/oauth-flow.mmd -o diagrams/oauth-flow.svg -w 700 -H 500
   ```

### Phase 4: Quality and Integration

8. **Cross-Reference Integration**:
   - Link between related documents
   - Update existing documentation with new references
   - Ensure navigation consistency

9. **GitHub Integration**:
   - Add commit permalinks: `[Description](https://github.com/org/repo/blob/commit/path#L123-L145)`
   - Reference specific implementations with line ranges
   - Maintain link accuracy with commit-based references

10. **Standards Compliance**:
    - Apply consistent formatting across all documents
    - Ensure professional presentation standards
    - Validate diagram sizing and embedding

## Output Organization

### Directory Structure
```
./
├── walkthroughs/
│   └── [TICKET-ID]/
│       ├── code-walkthrough/           # Multi-document systems
│       │   ├── 01-master-index.md     # Navigation hub
│       │   ├── 10-database-evolution.md
│       │   ├── 11-oauth-architecture.md
│       │   └── diagrams/              # Professional diagrams
│       │       ├── oauth-flow.mmd     # Mermaid source
│       │       ├── oauth-flow.svg     # Compiled SVG
│       │       └── README.md          # Diagram guidelines
│       └── single-updates/            # Individual document updates
└── docs/
    ├── commands-cheatsheet.md         # Updated with new patterns
    └── documentation-standards.md     # Enhanced standards
```

### File Naming Conventions
- **Multi-document systems**: `[NN]-[section-name].md` (01-master-index.md)
- **Technical walkthroughs**: `[NN]-[component]-[aspect].md` (10-database-schema-evolution.md)
- **Diagrams**: `[component]-[type].svg` (oauth-flow-architecture.svg)
- **Single updates**: Preserve existing naming while enhancing content

## Advanced Features

### GitHub Integration Patterns
- **Commit Permalinks**: Link to specific commit state for permanent accuracy
- **Line Range References**: Direct links to implementation details
- **Cross-Repository Links**: Handle complex project structures
- **Live Code Examples**: Interactive exploration through GitHub interface

### Professional Diagram Management
- **Size Guidelines**: 400px (simple), 500px (moderate), 600px (complex), 700px (architecture)
- **Source Control**: Both `.mmd` and `.svg` in version control
- **Consistent Styling**: Professional color schemes and layouts
- **Mobile Compatibility**: Responsive sizing considerations

### Multi-Document Navigation
- **Master Index**: Comprehensive navigation hub with multiple access patterns
- **Cross-References**: Consistent linking between related sections  
- **Progressive Complexity**: Organized learning paths for different skill levels
- **Quick Navigation**: Component, complexity, and phase-based organization

## Quality Control

### Standards Enforcement
- Consistent formatting and presentation across all documents
- Professional GitHub integration with accurate permalinks
- Diagram quality and sizing standards
- Cross-reference accuracy and maintenance

### Documentation Testing
- Verify all GitHub links are accessible and accurate
- Test diagram rendering across different viewers
- Validate navigation between documents
- Ensure mobile compatibility and accessibility

Create documentation that matches the scope and sophistication demonstrated in this session's comprehensive technical walkthrough system, with professional GitHub integration, size-controlled diagrams, and systematic cross-referencing.