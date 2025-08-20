# GTD-Zettelkasten System User Guide

## Welcome to Your Second Brain

Imagine a system where no brilliant idea escapes, no commitment falls through the cracks, and your knowledge compounds daily. This guide will transform you from an overwhelmed information worker into a productivity powerhouse with a continuously growing knowledge base.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Daily Scenarios](#daily-scenarios)
3. [Complete Keybinding Reference](#complete-keybinding-reference)
4. [Capture Templates Guide](#capture-templates-guide)
5. [Processing Workflows](#processing-workflows)
6. [Review Cycles](#review-cycles)
7. [Knowledge Management](#knowledge-management)
8. [Advanced Workflows](#advanced-workflows)
9. [Troubleshooting](#troubleshooting)
10. [Tips and Best Practices](#tips-and-best-practices)

## Getting Started

### Your First Day

**Morning (8:00 AM)**
You sit down with your coffee. Instead of opening email or Slack, you press `SPC o o r d` to start your daily review. The system shows you:
- 3 meetings scheduled for today (MEETING state items)
- 5 next actions from yesterday
- Your calendar for context

You quickly scan through, mentally preparing for the day ahead.

**First Interruption (8:15 AM)**
Your manager Slacks: "Can we review the API design after lunch?"
Without switching contexts, you press `SPC o o c i` and type:
```
Review API design with manager @office :work:
```
Press `C-c C-c` to save. Back to your coffee. Total time: 5 seconds.

**During Coding (10:30 AM)**
While reviewing code, you realize the authentication system needs refactoring. Press `SPC o o c w p` to capture a work project:
```
Refactor authentication system
** TODO Research modern auth patterns
** TODO Design new architecture  
** TODO Implement changes
** TODO Update documentation
```

### Your First Week Milestones

- **Day 1**: Master quick capture (`SPC o o c i`)
- **Day 2**: Process your first inbox to zero (`SPC o o p i`)
- **Day 3**: Create your first project with next actions
- **Day 4**: Use contexts to filter tasks
- **Day 5**: Complete your first weekly review
- **Weekend**: Start building your Zettelkasten

## Daily Scenarios

### Scenario 1: The Morning Scramble

**7:30 AM** - You wake up with three ideas:
1. A solution to yesterday's bug
2. A book recommendation from a dream (yes, really)
3. Need to buy milk

**Actions:**
```
SPC o o c i  → "Debug solution: Check null pointer in auth handler @computer :work:"
SPC o o c i  → "Book: 'Antifragile' by Taleb :personal:learning:"  
SPC o o c i  → "Buy milk @errands :personal:"
```

**Result:** All captured in 20 seconds. Brain free to focus on morning routine.

### Scenario 2: The Meeting Marathon

**9:00 AM** - Back-to-back meetings scheduled

**Before meetings:**
```
SPC o o a g  → View GTD dashboard
```
You see all MEETING items for today with prep notes.

**During meeting:**
```
SPC o o c m  → Meeting template opens
```
Capture decisions and action items in real-time.

**After meeting:**
```
SPC o o p i  → Process captured items
- Convert action items to NEXT
- File reference info to Zettelkasten
- Update project status
```

### Scenario 3: The Deep Work Session

**2:00 PM** - Time for focused coding

**Setup:**
```
SPC o o a W       → Work view only
SPC o o n n       → Filter next actions by @computer
```

You see only relevant tasks. Pick one and dive in.

**During work:**
- Discover interesting pattern: `SPC o o z c` → Capture to Zettelkasten
- Find a bug: `SPC o o c i` → Quick capture
- Need research: `SPC o o c w w` → Create WAITING item

**Wrap up:**
```
SPC o o s  → Save all org buffers
```

### Scenario 4: The Learning Session

**Evening** - Reading a technical book

**Setup:**
```
SPC o o z c  → Create literature note template
```

**While reading:**
- Interesting quote: Highlight and capture
- Actionable idea: `SPC o o i e` → Extract as task
- Connect to existing knowledge: `SPC o o z i` → Insert link

**Result:** Book knowledge integrated into both task and knowledge systems.

## Complete Keybinding Reference

### Master Command: `SPC o o`

All GTD-Zettelkasten commands start with `SPC o o`. Think "**O**rg **O**rganization" or "**O**h **O**h, I need to capture this!"

### Capture Commands (`SPC o o c`)

| Keybinding | Command | Description | Example Use |
|------------|---------|-------------|-------------|
| `SPC o o c c` | Generic capture | Opens capture template selection | When unsure which template to use |
| `SPC o o c i` | Inbox capture | Quick thought/task capture | "Call dentist" |
| `SPC o o c p i` | Personal inbox | Personal item capture | "Plan vacation" |
| `SPC o o c p p` | Personal project | Multi-step personal project | "Renovate kitchen" |
| `SPC o o c p n` | Personal next action | Single personal task | "Buy groceries @errands" |
| `SPC o o c w i` | Work inbox | Work item capture | "Review PR #234" |
| `SPC o o c w p` | Work project | Multi-step work project | "Launch new feature" |
| `SPC o o c w n` | Work next action | Single work task | "Email client update @computer" |
| `SPC o o c w w` | Work waiting | Delegated work items | "Waiting for: API specs from John" |
| `SPC o o c m m` | Movie capture (OMDB) | Auto-fetch movie details | "The Matrix" → Full details |
| `SPC o o c m t` | TV show capture (OMDB) | Auto-fetch show details | "Breaking Bad" → Seasons, cast |
| `SPC o o c m M` | Movie manual | Manual movie entry | When OMDB fails |
| `SPC o o c m T` | TV show manual | Manual show entry | For obscure shows |
| `SPC o o c n` | Next action | Generic next action | Context-aware task |
| `SPC o o c W` | Waiting for | Generic waiting | "Waiting for: Package delivery" |

### Process Commands (`SPC o o p`)

| Keybinding | Command | Description | Workflow |
|------------|---------|-------------|----------|
| `SPC o o p i` | Process inbox | Empty inbox to zero | Start here daily |
| `SPC o o p c` | Clarify item | Decide what something is | During processing |

### Navigate Commands (`SPC o o n`)

| Keybinding | Command | Description | When to Use |
|------------|---------|-------------|-------------|
| `SPC o o n i` | Open inbox | Jump to inbox.org | Check captured items |
| `SPC o o n p` | Open projects | View all projects | Project planning |
| `SPC o o n n` | Next actions | View actionable tasks | Choose what to do |
| `SPC o o n s` | Someday/maybe | Future possibilities | Weekly review |
| `SPC o o n c` | Calendar | Time-specific items | Check appointments |
| `SPC o o n w` | Waiting for | Blocked items | Follow up on delegated |

### Review Commands (`SPC o o r`)

| Keybinding | Command | Description | Frequency |
|------------|---------|-------------|-----------|
| `SPC o o r d` | Daily review | Plan your day | Every morning |
| `SPC o o r w` | Weekly review | Full system review | Weekly (Friday/Sunday) |
| `SPC o o r m` | Monthly review | Higher level check | Monthly |
| `SPC o o r q` | Quarterly review | Life direction | Every 3 months |

### Agenda Commands (`SPC o o a`)

| Keybinding | Command | Description | Shows |
|------------|---------|-------------|-------|
| `SPC o o a a` | Standard agenda | Default org agenda | Calendar + tasks |
| `SPC o o a g` | GTD view | Full GTD dashboard | Inbox, Next, Projects |
| `SPC o o a d` | Daily dashboard | Today focus | Today's items only |
| `SPC o o a w` | Weekly view | Week at a glance | 7-day forecast |
| `SPC o o a p` | Personal view | Personal items only | Filtered by :personal: |
| `SPC o o a W` | Work view | Work items only | Filtered by :work: |
| `SPC o o a m` | Media dashboard | Entertainment queue | Movies, shows to watch |
| `SPC o o a c` | Context view | Current context | Based on time/location |

### Save Command

| Keybinding | Command | Description | When to Use |
|------------|---------|-------------|-------------|
| `SPC o o s` | Save all | Save all org buffers | Before breaks, regularly |

### Zettelkasten Commands (`SPC o o z`)

| Keybinding | Command | Description | Purpose |
|------------|---------|-------------|---------|
| `SPC o o z n` | Find/create note | Search or new note | Access knowledge base |
| `SPC o o z i` | Insert link | Link to another note | Build connections |
| `SPC o o z c` | Capture note | New knowledge entry | Add to Zettelkasten |
| `SPC o o z d` | Daily note | Today's journal | Daily logging |
| `SPC o o z D` | Daily (date) | Specific day's note | Past/future journals |
| `SPC o o z b` | Backlinks | What links here | See connections |
| `SPC o o z g` | Graph view | Visual knowledge map | Explore relationships |
| `SPC o o z r` | Find reference | Search references | Quick lookup |
| `SPC o o z l` | Literature note | Book/article notes | Reading capture |
| `SPC o o z p` | Permanent note | Core ideas | Refined thoughts |

### Integration Commands (`SPC o o i`)

| Keybinding | Command | Description | Use Case |
|------------|---------|-------------|----------|
| `SPC o o i l` | Link to roam | Connect task to note | Add knowledge context |
| `SPC o o i e` | Extract actions | Tasks from notes | Turn ideas into TODOs |
| `SPC o o i r` | Review knowledge | Project knowledge base | See related notes |
| `SPC o o i t` | Task from note | Create task from note | Actionable insights |

## Capture Templates Guide

### Quick Capture (Default)
```org
* TODO %?
  :PROPERTIES:
  :CREATED: %U
  :END:
  %i
```
**Use when:** You need to capture fast and think later

### Project Template
```org
* TODO %? [/] :project:
  :PROPERTIES:
  :CREATED: %U
  :CATEGORY: %^{Category|personal|work}
  :DEADLINE: %^{Deadline}t
  :END:
  
** Why this project?
%^{Purpose}

** Success criteria
- [ ] %^{Success metric 1}
- [ ] %^{Success metric 2}

** TODO Define project outcomes
** TODO Break down into tasks
** TODO Identify first next action
```
**Use when:** Starting something that requires multiple steps

### Meeting Template
```org
* MEETING %? :meeting:
  SCHEDULED: %^{When}t
  :PROPERTIES:
  :CREATED: %U
  :ATTENDEES: %^{Who}
  :LOCATION: %^{Where|@office|@zoom|@phone}
  :END:
  
** Agenda
- %^{Agenda item}

** Preparation
- [ ] %^{Prep task}

** Notes
%?

** Action items
- [ ] 

** Decisions made
- 
```
**Use when:** Scheduling or documenting meetings

### Waiting For Template
```org
* WAITING %? :waiting:
  :PROPERTIES:
  :CREATED: %U
  :WAITING_ON: %^{Person or system}
  :DELEGATED: %U
  :FOLLOW_UP: %^{Follow up date}t
  :END:
  
Original request:
%^{What did you ask for?}

Context:
%^{Why do you need this?}
```
**Use when:** You've delegated or are blocked

### Literature Note Template
```org
#+title: %^{Title}
#+author: %^{Author}
#+type: %^{Type|book|article|video|podcast}
#+date: %U
#+tags: %^{Tags}

* Summary
%^{One paragraph summary}

* Key Ideas
** %^{Key idea 1}
   %?

* Quotes
** [%] "%^{Quote}"
   - Context: %^{Quote context}
   - Page: %^{Page number}

* Personal Thoughts
%^{What do I think about this?}

* Action Items
- [ ] %^{What will I do with this knowledge?}

* Related Notes
- [[roam:%^{Related note}]]
```
**Use when:** Processing books, articles, videos

### Permanent Note Template
```org
#+title: %^{Concept}
#+created: %U
#+tags: %^{Tags}
#+id: %(org-id-uuid)

%^{State the idea in one sentence}

* Explanation
%^{Explain in your own words}

* Evidence
- %^{Supporting evidence}

* Counter-arguments
- %^{What could be wrong with this?}

* Connections
- Relates to: [[roam:%^{Related concept}]]
- Contrasts with: [[roam:%^{Contrasting idea}]]
- Leads to: [[roam:%^{Implication}]]

* Applications
- %^{How can this be applied?}
```
**Use when:** Developing original thoughts

## Processing Workflows

### The 2-Minute Rule Workflow

```
SPC o o p i          → Start processing inbox
→ Read item
→ Ask: "Can this be done in 2 minutes?"
  
If YES:              If NO:
→ Do it now         → Is it actionable?
→ Mark DONE           
                      If YES:           If NO:
                      → Next action?    → Reference?
                      → Project?        → Someday?
                      → Waiting?        → Trash?
```

### The Clarify Process

For each inbox item, ask:

1. **What is it?**
   - Task, project, reference, or idea?

2. **Is it actionable?**
   - Yes → Continue to #3
   - No → File as reference or trash

3. **What's the next action?**
   - Physical, visible action
   - Starts with a verb
   - Can be done in one session

4. **What context?**
   - Where can this be done?
   - What tools needed?
   - What energy level?

5. **What's the outcome?**
   - Single action → NEXT
   - Multi-step → Project
   - Blocked → WAITING

### Bulk Processing

When inbox has 20+ items:

```
1. SPC o o p i       → Open inbox
2. Sort by type:
   - Questions → Answer immediately
   - FYIs → Read and archive
   - Requests → Convert to tasks
   
3. Batch similar items:
   - All emails together
   - All code reviews together
   - All meeting requests together
   
4. Use macros for repetitive processing:
   - Record: q a [actions] q
   - Apply: @ a
```

## Review Cycles

### Daily Review (5-10 minutes)

**Morning Ritual:**
```
SPC o o r d          → Open daily review template
□ Check calendar     → What's fixed today?
□ Review NEXT        → What can I accomplish?
□ Check WAITING      → What needs follow-up?
□ Set MIT's          → 3 Most Important Tasks
□ Time block         → When will I do them?
```

**Evening Ritual:**
```
□ Process inbox      → SPC o o p i
□ Check off DONE     → Update task states
□ Quick capture      → Brain dump for tomorrow
□ Note wins          → What went well?
□ Save everything    → SPC o o s
```

### Weekly Review (30-60 minutes)

**Get Clear (20 min):**
```
□ Collect loose items
  - Physical inbox
  - Email inbox
  - Browser tabs
  - Notebook pages
  
□ Process to zero
  SPC o o p i → Until empty
  
□ Empty your head
  SPC o o c i → Brain dump
```

**Get Current (25 min):**
```
□ Review calendar
  - Past week: Extract insights
  - Coming week: Prepare
  
□ Review project list
  SPC o o n p → Each project has NEXT?
  
□ Review WAITING
  SPC o o n w → Follow up needed?
  
□ Review SOMEDAY
  SPC o o n s → Anything activate?
```

**Get Creative (15 min):**
```
□ What's working?
□ What's not?
□ New projects?
□ Bold ideas?
```

### Monthly Review (60-90 minutes)

Extends weekly review with:

```
□ Review goals        → On track?
□ Analyze metrics     → Tasks completed, projects advanced
□ Update areas        → Life categories current?
□ Prune someday       → Still relevant?
□ Archive completed   → Clean up files
□ System maintenance  → What needs tweaking?
```

### Quarterly Review (2-3 hours)

Life-level perspective:

```
□ Vision check        → Still the right direction?
□ Role review         → All roles have projects?
□ Goal setting        → Next quarter's focus
□ System overhaul     → Major improvements?
□ Knowledge review    → Zettelkasten insights
□ Celebrate wins      → Acknowledge progress
```

## Knowledge Management

### Building Your Zettelkasten

**Week 1: Capture Everything**
- Every interesting thought → `SPC o o z c`
- Don't judge, just capture
- Aim for 5 notes daily

**Week 2: Start Connecting**
- Review week 1 notes
- Find connections → `SPC o o z i`
- Create hub notes for themes

**Week 3: Develop Ideas**
- Upgrade fleeting → permanent notes
- Combine related concepts
- Challenge your thinking

**Week 4: Apply Knowledge**
- Extract projects → `SPC o o i e`
- Create implementation plans
- Share insights

### Note Development Pipeline

```
Fleeting Note (shower thought)
    ↓ [SPC o o z c]
Literature Note (from reading)  
    ↓ [Process and connect]
Permanent Note (refined idea)
    ↓ [SPC o o i e]
Project/Action (applied knowledge)
```

### Connection Strategies

**Direct Linking:**
- While writing, think: "This relates to..."
- Insert link: `SPC o o z i`
- Add context why they're related

**Hub Notes:**
- Create index for major topics
- List all related notes
- Write overview synthesis

**Trail Guides:**
- Create "start here" notes
- Map learning paths
- Suggest reading order

**Contrast Notes:**
- X vs Y comparisons
- Opposing viewpoints
- Dialectical synthesis

## Advanced Workflows

### The Code-Review-Capture Flow

While reviewing code:
```
1. See improvement → SPC o o c i 
   "Refactor auth.js - extract validation logic @computer :work:"
   
2. Find bug → SPC o o c w n
   "Fix: User.save() doesn't handle null email @computer :work:bug:"
   
3. Question for author → SPC o o c w w
   "WAITING Ask Jane: Why cache invalidation here? :work:"
   
4. Learning moment → SPC o o z c
   "Pattern: Use Optional for null handling in Java"
```

### The Research Project Flow

Starting a new technology investigation:

```
1. Create project → SPC o o c w p
   "Research GraphQL for API v2"
   
2. Create knowledge hub → SPC o o z n
   "GraphQL Research Hub"
   
3. As you research:
   - Article notes → SPC o o z l
   - Key concepts → SPC o o z p  
   - Questions → SPC o o c i
   - Experiments → SPC o o c w n
   
4. Weekly synthesis → SPC o o z n
   "GraphQL Week 1 Insights"
   
5. Decision time → SPC o o i e
   Extract implementation tasks
```

### The Meeting-Heavy Day Protocol

**Night before:**
```
SPC o o a d          → Tomorrow's view
For each MEETING:
- Add agenda items
- Prep materials
- Set reminders
```

**Between meetings:**
```
SPC o o c i          → Quick capture outcomes
Tag with :meeting-followup:
```

**End of day:**
```
SPC o o n i          → Process all :meeting-followup:
Convert to:
- Actions → NEXT
- Delegated → WAITING  
- Info → Zettelkasten
```

### The Learning Sprint Method

**Setup (Sunday):**
```
1. Choose topic
2. Create project → SPC o o c p p
   "Learn Rust in 30 days"
3. Create hub note → SPC o o z n
   "Rust Learning Hub"
4. Schedule daily time blocks
```

**Daily (30-60 min):**
```
Morning:
- SPC o o n p → Today's learning goal
- Start timer

During:
- SPC o o z l → Literature notes
- SPC o o c i → Practice tasks

Evening:
- SPC o o z d → Daily reflection
- SPC o o z i → Connect to existing
```

**Weekly:**
```
- SPC o o z n → Week synthesis
- SPC o o i e → Extract projects
- Share learnings
```

## Troubleshooting

### Common Issues and Solutions

**"I forgot to capture something important"**
- Train the reflex: Task appears → `SPC o o c i`
- Keep capture open during meetings
- Review at day's end: "What did I commit to?"

**"My inbox is overwhelming (50+ items)"**
```
1. Declare bankruptcy:
   - Archive all to "inbox-overflow-DATE.org"
   - Start fresh
   
2. Speed process:
   - Sort by source (email, meetings, etc)
   - Bulk convert similar items
   - Use "2-second rule": Trash if not important
```

**"I never do my reviews"**
- Start smaller: 5-min daily only
- Calendar block: Friday 4pm
- Reward yourself: Review → Treat
- Track streak: "X days reviewed"

**"Tasks feel stale"**
- Add energy tags: :high-energy: :low-energy:
- Review contexts: Still relevant?
- Prune aggressively: When in doubt, SOMEDAY
- Focus on outcomes, not tasks

**"Can't find anything"**
```
Better capture:
- Use consistent naming
- Tag thoroughly
- Add context in PROPERTIES

Better search:
- SPC o o z n → Full text search
- Learn org-agenda filters
- Create saved searches
```

**"System feels heavy"**
- You're over-engineering
- Simplify templates
- Reduce required fields
- Remember: Capture fast, process later

### Error Messages

**"Invalid function: org-element-with-disabled-cache"**
- Restart Emacs
- Check org version: `M-x org-version`
- Ensure GTD system loaded after org

**"File not found: inbox.org"**
- Run setup: `M-x codelahoma-gtd-initialize`
- Check directory: `~/personal/org-files/gtd/`
- Create manually if needed

**"Capture template not found"**
- Verify templates loaded: `C-h v org-capture-templates`
- Reload config: `M-x load-file ~/.spacemacs.d/codelahoma-org.el`

## Tips and Best Practices

### Capture Excellence

**Make it Frictionless:**
- Learn `SPC o o c i` muscle memory
- Don't perfect during capture
- Capture first, think later
- Voice capture → Transcribe later

**Be Specific:**
```
Bad:  "Email John"
Good: "Email John re: Q3 budget review @computer :work:"
Best: "Email John about $50k overage in cloud costs by Friday @computer :work:deadline:"
```

**Context is King:**
- Where can this be done?
- What mindset needed?
- What tools required?
- Who else involved?

### Processing Mastery

**Batch by Energy:**
- High energy → Complex decisions
- Medium → Standard processing  
- Low → Quick filing, deleting

**Use Templates:**
- Meeting outcomes → Standard format
- Code reviews → Checklist
- Learning notes → Question prompts

**Time Box:**
- Set timer: 25 minutes processing
- Stop when timer ends
- Prevents perfection paralysis

### Review Wisdom

**Make it Sacred:**
- Same time, same place
- Phone off, door closed
- Favorite beverage
- Celebration after

**Use Checklists:**
- Print review templates
- Check physically
- Satisfaction of completion

**Capture Insights:**
- What patterns do you see?
- What's working well?
- What needs adjustment?

### Zettelkasten Growth

**One Idea Per Note:**
- Atomic thoughts
- Self-contained
- Combinable

**Your Words Only:**
- No copy-paste
- Rephrase to understand
- Add your perspective

**Link Generously:**
- When in doubt, link
- Explain the connection
- Create trails

**Regular Gardening:**
- Weekly: Review recent notes
- Monthly: Find new connections
- Quarterly: Major synthesis

### System Evolution

**Start Simple:**
- Week 1-2: Capture only
- Week 3-4: Add processing
- Month 2: Add reviews
- Month 3: Full system

**Customize Gradually:**
- Use defaults first
- Note pain points
- Adjust one thing at a time
- Test for a week

**Regular Maintenance:**
- Monthly: Archive old items
- Quarterly: Review templates
- Yearly: Major overhaul

### Integration Magic

**Cross-Pollination:**
- Project research → Permanent notes
- Book insights → New projects
- Daily notes → Pattern recognition
- Meeting notes → Action items

**Energy Management:**
- Tag tasks by energy required
- Match tasks to energy available
- Batch similar energy tasks
- Protect high-energy time

**Context Switching:**
- Group by tool needed
- Batch by location
- Theme your days
- Minimize transitions

### The Path to Mastery

**Month 1: Foundation**
- Capture habit solid
- Processing routine
- Daily review consistent

**Month 3: Flow**
- Weekly review automatic
- Contexts intuitive
- Knowledge growing

**Month 6: Integration**
- GTD feeds Zettelkasten
- Knowledge drives action
- System self-maintaining

**Year 1: Transformation**
- External brain functional
- Stress dramatically reduced
- Creative output increased
- Knowledge compounds daily

## Remember

This system is your servant, not your master. Start where you are, use what works, adjust what doesn't. The goal isn't perfection—it's progress.

Your future self will thank you for every captured thought, every processed task, every connected idea. Begin today. Press `SPC o o c i` and capture your first thought.

Welcome to your new superpower. 🚀