Let input_args = "$ARGUMENTS"

## Argument Interpretation
First, analyze the provided arguments: input_args

Based on the pattern:
- If input_args is empty: Analyze entire project structure and suggest improvements
- If input_args is "plan": Create a new directory structure plan for a feature or component
- If input_args is "compare": Compare current structure to best practices for the project type
- If input_args is a path: Analyze and suggest improvements for that specific directory
- If input_args starts with "create:": Create the specified directory structure

## Project Structure Analysis

You should help the user understand and improve their project's directory organization.

### Step 1: Current Structure Assessment
Use LS and Glob tools to comprehensively map the current project structure in the specified scope (input_args).

Focus on:
- Directory hierarchy depth and organization
- File distribution across directories
- Naming conventions and patterns
- Logical grouping of related files
- Adherence to project type conventions (Django, React, etc.)

### Step 2: Structure Analysis
Evaluate the current structure against best practices:

**Organization Principles:**
- Related files grouped together
- Clear separation of concerns
- Intuitive navigation paths
- Consistent naming conventions
- Appropriate nesting depth (avoid too deep or too shallow)

**Project-Specific Patterns:**
- Framework conventions (Django apps, React components, etc.)
- Development lifecycle separation (src, tests, docs, config)
- Environment-specific organization
- Feature-based vs. layer-based grouping

### Step 3: Improvement Recommendations
Provide specific, actionable recommendations:

**Directory Structure:**
- New directories to create for better organization
- Directories that could be merged or split
- Better names for existing directories
- Optimal hierarchy for the project type

**File Placement:**
- Files that belong in different directories
- Grouping opportunities for scattered files
- Standard locations for common file types
- Clear boundaries between different concerns

### Step 4: Implementation Guidance
If recommending changes:
- Provide step-by-step migration plan
- Identify dependencies that need updating
- Suggest safe migration order
- Include rollback options

### Step 5: Structure Documentation
Create clear documentation of:
- Recommended directory structure
- Purpose of each major directory
- File placement guidelines
- Naming conventions to follow

### Structure Analysis Framework

**For Django Projects:**
- App organization and boundaries
- Shared utilities and common code
- Template and static file organization
- Configuration and environment files

**For React/Frontend Projects:**
- Component hierarchy and organization
- Asset and style organization
- Feature-based vs. technical grouping
- Build and deployment structure

**For General Projects:**
- Source code organization
- Documentation structure
- Configuration management
- Development tool organization

Focus on creating a structure that will scale well as the project grows and makes it easy for developers to find and organize new files.