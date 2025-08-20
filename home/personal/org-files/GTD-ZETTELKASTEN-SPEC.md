# GTD-Zettelkasten Integration System Specification

## Vision
A unified productivity system that combines David Allen's Getting Things Done methodology with Niklas Luhmann's Zettelkasten knowledge management system, seamlessly integrated into Spacemacs for developers and knowledge workers.

## Core Principles
1. **Capture everything** - No thought or task is lost
2. **Process to zero** - Inboxes are regularly emptied
3. **Context-aware** - Right tasks at the right time
4. **Knowledge compounds** - Ideas build on each other
5. **Frictionless workflow** - Minimal keystrokes, maximum flow

## System Components

### 1. GTD Task Management

#### 1.1 Capture System
- **Quick Capture** - Single keystroke to capture any thought/task
- **Context Capture** - Auto-detect context (in code, email, meeting)
- **Template Capture** - Pre-filled templates for common items
- **Media Capture** - Movies/TV shows with metadata (OMDB integration)

#### 1.2 Task States
- `TODO` - Captured but not processed
- `NEXT` - Next actionable item
- `WAITING` - Blocked on someone/something
- `SOMEDAY` - Future consideration
- `HOLD` - Paused projects
- `MEETING` - Scheduled meetings/calls
- `DONE` - Completed
- `CANCELLED` - No longer relevant

#### 1.3 Organization
- **Inbox** - Unprocessed items
- **Projects** - Multi-step outcomes
- **Next Actions** - Single actionable steps
- **Waiting For** - Delegated or blocked items
- **Someday/Maybe** - Future possibilities
- **Calendar** - Time-specific commitments (including MEETING items)
- **Reference** - Non-actionable information

#### 1.4 Contexts
- **Location** - @home, @office, @errands
- **Tool** - @computer, @phone, @email
- **Energy** - @high-energy, @low-energy
- **Time** - @quick, @deep-work
- **People** - @boss, @team, @family

#### 1.5 Reviews
- **Daily Review** - Plan the day, process inbox
- **Weekly Review** - Full system review and planning
- **Monthly Review** - Higher-level goals alignment
- **Quarterly Review** - Life direction check

### 2. Zettelkasten Knowledge Management

#### 2.1 Note Types
- **Permanent Notes** - Atomic ideas in your own words
- **Literature Notes** - Key concepts from sources
- **Reference Notes** - Quick lookups and definitions
- **Project Notes** - Knowledge specific to GTD projects
- **Daily Notes** - Journal and fleeting thoughts

#### 2.2 Organization
- **Flat structure** - Notes linked by ideas, not hierarchy
- **Unique identifiers** - Each note has permanent ID
- **Tags** - Flexible categorization
- **Links** - Bidirectional connections between ideas

#### 2.3 Workflows
- **Progressive Summarization** - Refine ideas over time
- **Idea Development** - Build arguments through linked notes
- **Knowledge Retrieval** - Find related concepts quickly
- **Serendipitous Discovery** - Stumble upon forgotten connections

### 3. Integration Features

#### 3.1 GTD → Zettelkasten
- **Project Knowledge Base** - Each GTD project can have associated notes
- **Task-Generated Notes** - Create notes from completed tasks
- **Review Insights** - Capture learnings during reviews
- **Reference Filing** - Non-actionable items become permanent notes

#### 3.2 Zettelkasten → GTD
- **Action Extraction** - Convert note ideas into tasks
- **Project Generation** - Develop notes into full projects
- **Research Tasks** - Create tasks to explore ideas further
- **Implementation Tasks** - Turn knowledge into action

### 4. User Interface

#### 4.1 Keybinding Namespace (SPC o o)
The entire GTD-Zettelkasten system lives under the `SPC o o` namespace, providing a consistent and memorable entry point:

- **SPC o o** - Opens GTD menu (if no further key pressed)
- **Capture** (SPC o o c) - Quick entry points
  - `c` - Generic capture
  - `i` - Inbox
  - `p` - Personal items
  - `w` - Work items
  - `m` - Media (movies/shows)
- **Process** (SPC o o p) - Work through items
  - `i` - Process inbox
  - `c` - Clarify item
- **Navigate** (SPC o o n) - Jump to specific lists
  - `i` - Inbox
  - `p` - Projects
  - `n` - Next actions
- **Review** (SPC o o r) - Structured review sessions
  - `d` - Daily review
  - `w` - Weekly review
- **Agenda** (SPC o o a) - Custom views
  - `a` - Standard agenda
  - `g` - GTD view
  - `d` - Daily dashboard
  - `w` - Weekly view
  - `p` - Personal view
  - `W` - Work view
  - `m` - Media dashboard
- **Save** (SPC o o s) - Save all org buffers
- **Zettelkasten** (SPC o o z) - Knowledge operations
  - `n` - Find/create note
  - `i` - Insert link
  - `c` - Capture note
  - `d` - Daily note
  - `b` - Backlinks
- **Integrate** (SPC o o i) - Bridge GTD and Zettelkasten
  - `l` - Link to knowledge
  - `e` - Extract actions
  - `r` - Review project knowledge

#### 4.2 Agenda Views
- **Dashboard** - Today's priorities and calendar
- **GTD View** - Classic GTD lists with MEETING items
- **Context View** - Tasks by current context
- **Project View** - Project status overview
- **Personal/Work** - Separated life areas
- **Knowledge View** - Recent notes and connections

#### 4.3 Capture Templates
- **Quick Task** - Minimal friction capture
- **Project** - Full project with initial tasks
- **Waiting For** - Delegation tracking
- **Meeting** - Calendar entry with MEETING state
- **Note Types** - Permanent, literature, reference
- **Media** - Movies/shows with metadata
- **Meeting Notes** - Capture meeting outcomes and actions

### 5. Automation

#### 5.1 Smart Features
- **Context Detection** - Auto-set based on time/location
- **Template Selection** - Suggest based on context
- **Priority Suggestion** - Based on deadlines and dependencies
- **Link Suggestions** - Recommend related notes
- **Bulk Operations** - Process similar items together
- **Auto-save** - Regular saving of org buffers

#### 5.2 Integrations
- **Calendar Sync** - Two-way calendar integration (MEETING state)
- **Email to Task** - Convert emails to actions
- **Browser Capture** - Web content to notes
- **Code Integration** - TODOs from code comments
- **API Access** - External tool integration

### 6. Data Organization

#### 6.1 Directory Structure
```
~/personal/org-files/
├── gtd/
│   ├── inbox.org
│   ├── projects.org
│   ├── next-actions.org
│   ├── waiting-for.org
│   ├── someday.org
│   ├── calendar.org      # MEETING items here
│   ├── media.org
│   └── archive/
├── knowledge/
│   ├── permanent/
│   ├── literature/
│   ├── references/
│   ├── projects/
│   └── daily/
├── areas/
│   ├── personal.org
│   ├── work.org
│   └── learning.org
└── resources/
    ├── templates/
    └── reviews/
```

#### 6.2 Metadata
- **Creation Date** - When item was captured
- **Modified Date** - Last update
- **Scheduled** - When to do (especially for MEETING)
- **Deadline** - When due
- **Context Tags** - Where/when actionable
- **Energy Level** - Required focus level
- **Time Estimate** - Expected duration
- **Dependencies** - Links to related items

### 7. Workflows

#### 7.1 Daily Workflow
1. **Morning Review** - Check calendar, review MEETING items
2. **Capture** - Throughout the day as needed
3. **Process** - Empty inbox to zero
4. **Execute** - Work from context lists
5. **Save** - SPC o o s to save all org buffers
6. **Evening Review** - Close loops, plan tomorrow

#### 7.2 Weekly Workflow
1. **Get Clear** - Process all inboxes
2. **Get Current** - Update all lists
3. **Get Creative** - Think about new projects
4. **Get Connected** - Link new knowledge
5. **Schedule** - Convert items to MEETING state as needed

#### 7.3 Knowledge Workflow
1. **Capture** - Interesting ideas/quotes
2. **Process** - Create permanent notes
3. **Connect** - Link related concepts
4. **Develop** - Build idea clusters
5. **Apply** - Convert to actionable projects

### 8. Success Metrics

#### 8.1 GTD Health
- Inbox processing time < 2 minutes/item
- All projects have next actions
- Weekly review completion rate > 90%
- Average task age < 2 weeks
- MEETING items properly scheduled

#### 8.2 Knowledge Health
- Notes have 3+ connections average
- Literature notes processed < 1 week
- Monthly note creation > 20
- Orphan notes < 10%

### 9. User Experience Goals

1. **Capture anywhere** in under 3 seconds
2. **Process inbox** without context switching
3. **Find any item** in under 10 seconds
4. **Review flows** guide themselves
5. **Knowledge emerges** through connections
6. **System maintains itself** with minimal overhead
7. **Never lose work** with easy saving (SPC o o s)

### 10. Key Implementation Requirements

1. **Namespace Consistency** - Everything under SPC o o
2. **State Management** - Proper handling of all states including MEETING
3. **File Safety** - Regular saves and backups
4. **Performance** - Quick load times and responsive interface
5. **Error Handling** - Graceful failures with helpful messages
6. **Progressive Enhancement** - Works without all features loaded

This specification defines what the system should do without prescribing implementation details. It focuses on user needs, workflows, and outcomes rather than technical architecture.