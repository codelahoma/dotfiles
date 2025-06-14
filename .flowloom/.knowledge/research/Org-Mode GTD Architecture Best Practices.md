---
title: Org-Mode GTD Architecture Best Practices
type: note
permalink: research/org-mode-gtd-architecture-best-practices
tags:
- '#org-mode'
- '#gtd'
- '#architecture'
- '#workflow'
- '#research'
---

# Org-Mode GTD Architecture Best Practices

## File Structure Patterns

### Pattern 1: Core GTD Files
```
~/org/
├── gtd.org         # Active projects and tasks
├── inbox.org       # Capture/refile inbox
├── someday.org     # Someday/maybe items
├── tickler.org     # Date-triggered reminders
├── archive.org     # Completed/cancelled items
└── reference/      # Reference materials
```

### Pattern 2: Context-Based Organization
```
~/org/
├── inbox.org       # Universal inbox
├── work/
│   ├── projects.org
│   ├── tasks.org
│   └── meetings.org
├── personal/
│   ├── projects.org
│   ├── tasks.org
│   └── routines.org
└── archive/
```

### Pattern 3: Action-Focused Structure
```
~/org/
├── actions.org     # Next actions
├── projects.org    # Project definitions
├── waiting.org     # Delegated/waiting items
├── someday.org     # Future possibilities
└── calendar.org    # Time-specific items
```

## Task States and Keywords

### Recommended TODO Keywords
```elisp
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

;; With quick selection keys
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
```

### Keyword Meanings
- **TODO**: Something that needs to be done
- **NEXT**: The next action for a project (actionable now)
- **STARTED**: Currently in progress
- **WAITING**: Blocked, waiting on someone else
- **HOLD**: Paused by choice
- **DONE**: Completed successfully
- **CANCELLED**: No longer needed

## Capture and Refile Architecture

### Refile Configuration
```elisp
;; Include all agenda files and current buffer
(setq org-refile-targets '((nil :maxlevel . 9)
                          (org-agenda-files :maxlevel . 9)))

;; Use file path for refile completion
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

;; Allow creating new nodes
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Use IDO for target completion
(setq org-refile-use-outline-path 'file)
(setq org-completion-use-ido t)

;; Save all after refile
(advice-add 'org-refile :after 'org-save-all-org-buffers)
```

### Capture Workflow
1. **Quick Capture** → inbox.org
2. **Process Inbox** → Refile to appropriate file/project
3. **Add Context** → Tags, deadlines, effort estimates
4. **Archive When Done** → Move to archive.org

## Agenda Architecture

### Core Agenda Files Setup
```elisp
(setq org-agenda-files '("~/org/gtd.org"
                        "~/org/inbox.org"
                        "~/org/tickler.org"))

;; Exclude someday.org from daily agenda
(setq org-agenda-file-regexp "\\`[^.].*\\.org\\|.todo\\'")
```

### Custom Agenda Commands
```elisp
(setq org-agenda-custom-commands
      '(("d" "Daily Dashboard"
         ((agenda "" ((org-agenda-span 1)
                     (org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Actions")))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting On")))))
        
        ("w" "Weekly Review"
         ((agenda "" ((org-agenda-span 7)))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled TODOs")))
          (tags "PROJECT"
                ((org-agenda-overriding-header "Active Projects")))))
        
        ("p" "Projects" tags "+PROJECT+TODO=\"TODO\"|+PROJECT+TODO=\"NEXT\"|+PROJECT+TODO=\"STARTED\"")
        
        ("c" . "Context Views")
        ("co" "@office" tags-todo "@office")
        ("ch" "@home" tags-todo "@home")
        ("ce" "@errands" tags-todo "@errands")))
```

## Tag System for GTD

### Context Tags
```elisp
(setq org-tag-alist '((:startgroup)
                      ("@office" . ?o)
                      ("@home" . ?h)
                      ("@errands" . ?e)
                      ("@computer" . ?c)
                      ("@phone" . ?p)
                      (:endgroup)
                      ("PROJECT" . ?P)
                      ("URGENT" . ?u)
                      ("ROUTINE" . ?r)))
```

### Project Identification
- Use "PROJECT" tag or property
- Projects contain multiple TODOs
- Must have at least one NEXT action
- Archive when all tasks complete

## Review Workflows

### Daily Review Process
```org
* Daily Review [0/4]
  - [ ] Process inbox to zero
  - [ ] Review calendar for today/tomorrow  
  - [ ] Check NEXT actions
  - [ ] Quick scan of WAITING items
```

### Weekly Review Template
```org
* Weekly Review [0/7]
  - [ ] Process all inboxes
  - [ ] Review previous calendar week
  - [ ] Review upcoming calendar week
  - [ ] Review NEXT actions list
  - [ ] Review project list
  - [ ] Review WAITING list
  - [ ] Review someday/maybe list
```

### Implementation in Org
```elisp
;; Auto-generate review templates
(defun my/create-weekly-review ()
  "Create a new weekly review entry."
  (interactive)
  (find-file "~/org/reviews.org")
  (goto-char (point-max))
  (insert (format-time-string "* Weekly Review %Y-%m-%d\n"))
  (insert "** Inbox Processing\n")
  (insert "** Calendar Review\n")
  (insert "** Projects Status\n")
  (insert "** Next Actions\n"))
```

## Properties and Metadata

### Useful Properties for GTD
```org
* TODO Task Title
  :PROPERTIES:
  :EFFORT:   2:00
  :CONTEXT:  @office
  :ENERGY:   high
  :CATEGORY: ProjectName
  :CREATED:  [2024-01-15 Mon]
  :END:
```

### Property Inheritance
```elisp
;; Inherit certain properties from parent
(setq org-use-property-inheritance t)
(setq org-inherit-properties '("CATEGORY" "CONTEXT"))
```

## Archiving Strategy

### Archive Configuration
```elisp
;; Archive to separate file
(setq org-archive-location "~/org/archive.org::* From %s")

;; Save archive file after archiving
(setq org-archive-save-context-info '(time file ltags itags todo category olpath))

;; Archive subtrees, not just tasks
(setq org-archive-subtree-add-inherited-tags t)
```

### When to Archive
- Completed projects (all tasks DONE)
- Cancelled projects/tasks
- Old reference material
- Completed periodic reviews

## Performance Optimization

### Large File Handling
```elisp
;; Prevent slowdown with large files
(setq org-agenda-inhibit-startup t)
(setq org-agenda-use-tag-inheritance nil)

;; Optimize agenda generation
(setq org-agenda-span 'day)
```

### File Organization Tips
- Keep active files under 1000 lines
- Archive aggressively
- Use separate files for different contexts
- Limit agenda files to active items only

## Integration Points

### Clock Time Tracking
```elisp
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
```

### Effort Estimates
```elisp
(setq org-global-properties
      '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")))
```

### Column View for Projects
```elisp
(setq org-columns-default-format "%50ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %TAGS")
```

## Core Principles

1. **Capture Quickly**: Minimize friction for capturing ideas
2. **Process Regularly**: Empty inbox daily
3. **One System**: All tasks go through the same workflow
4. **Context Matters**: Use tags/properties for where/when
5. **Review Consistently**: Daily and weekly reviews are essential
6. **Archive Completed**: Keep active lists clean
7. **Start Simple**: Begin with basic setup, evolve over time

## Common Pitfalls to Avoid

1. **Over-engineering**: Start simple, add complexity only when needed
2. **Too many files**: Makes refiling and finding tasks harder
3. **Complex keywords**: Stick to standard GTD states
4. **Ignoring reviews**: System breaks down without regular maintenance
5. **Not archiving**: Slows down agenda and clutters views
6. **Too many custom agendas**: Focus on a few key views

## Evolution Path

1. **Phase 1**: Basic capture and task list
2. **Phase 2**: Add contexts and projects
3. **Phase 3**: Implement reviews and archiving
4. **Phase 4**: Add time tracking and effort estimates
5. **Phase 5**: Integrate with calendar and email
6. **Phase 6**: Advanced automation and workflows