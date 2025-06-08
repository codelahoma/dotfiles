---
type: documentation
category: architecture
tags: [org-mode, org-roam, gtd, workflow, productivity, spacemacs]
status: active
complexity: advanced
priority: high
frequency: daily
audience: [flowloom-ai, developers]
introduced_in: "FlowLoom 2.0.0"
last_verified: "2025-06-07"
maintainer: Rod Knowlton
permalink: unified-org-roam-gtd-system
depends_on: [spacemacs, org-mode, org-roam]
related_to: [dotspacemacs-configuration, org-mode-setup]
file_paths: ["display.md", "home/spacemacs-config/tools/org-mode.org"]
---

# Unified Org-Roam + Org-Mode GTD System Design

## Design Philosophy
**"Org-Roam as the Knowledge Base, Org-Agenda as the Action Engine"**

- **Org-Roam**: Handle all knowledge, notes, references, projects, and long-term thinking
- **Org-Agenda**: Handle all actions, scheduling, and daily workflow
- **Unified Integration**: Seamless linking between knowledge and actions

## Current State Analysis

### Existing Org Configuration
**Location**: `/home/spacemacs-config/tools/org-mode.org`

**Current Features**:
- ✅ **Org-Roam enabled** (org-enable-roam-support t, org-enable-roam-ui t)
- ✅ **Rich visual system** with elaborate color schemes (21 schemes) and bullet sets (9 sets)  
- ✅ **Modern features** enabled (notifications, transclusion, sticky headers)
- ✅ **Integration support** (GitHub, Jira, Trello, reveal.js)
- ✅ **Advanced packages** (org-roam-bibtex, org-noter, org-noter-pdftools)

**Visual Customizations to Preserve**:
- `org-heading-colors-schemes` - 21 beautiful color schemes (Arctic, Ocean, Galaxy, etc.)
- `org-superstar-bullet-sets` - 9 different bullet styles (minimal, ornate, stars, etc.)
- Custom font faces with proper typography hierarchy
- Interactive functions: `switch-org-colors()`, `switch-org-bullets()`, `preview-org-colors()`

### Missing: Core GTD Workflow
- ❌ No org-agenda configuration
- ❌ No capture templates  
- ❌ No GTD-specific TODO keywords
- ❌ No organized file structure
- ❌ No unified roam + agenda workflow

## Proposed File Structure
```
~/org/
├── inbox.org              # Quick capture entry point
├── work/
│   ├── projects.org       # Work project definitions  
│   ├── gtd.org           # Work-specific GTD workflow
│   └── someday.org       # Work someday/maybe items
├── personal/
│   ├── projects.org       # Personal project definitions
│   ├── gtd.org           # Personal GTD workflow  
│   └── someday.org       # Personal someday/maybe items
├── archive.org            # All completed items archive
└── roam/                  # Unified org-roam knowledge base
    ├── daily/             # Daily notes (work + personal)
    ├── work/              # Work-specific roam notes
    ├── personal/          # Personal-specific roam notes
    ├── areas/             # Life areas (both contexts)
    └── resources/         # Reference materials (shared)
```

## GTD Workflow Implementation

### 1. TODO Keywords with Context Tags
```emacs-lisp
(setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; Use tags for context separation
(setq org-tag-alist '(("@work" . ?w)
                      ("@personal" . ?p)
                      ("@home" . ?h)
                      ("@office" . ?o)
                      ("@phone" . ?c)
                      ("@computer" . ?m)))
```

### 2. Context-Aware Capture Templates
```emacs-lisp
(setq org-capture-templates
      '(;; Universal inbox
        ("i" "Inbox" entry (file "~/org/inbox.org")
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
        
        ;; Work captures
        ("w" "Work Templates")
        ("wt" "Work Task" entry (file "~/org/work/gtd.org")
         "* TODO %? :@work:\n  %U\n  %a\n  %i" :empty-lines 1)
        ("wp" "Work Project" entry (file "~/org/work/projects.org")
         "* TODO %? [/] :@work:\n  DEADLINE: %^{Deadline}t\n  %U\n  %a\n  %i")
        ("wn" "Work Note" plain (function org-roam-node-insert)
         "%?" :empty-lines 1 :unnarrowed t)
        
        ;; Personal captures  
        ("p" "Personal Templates")
        ("pt" "Personal Task" entry (file "~/org/personal/gtd.org")
         "* TODO %? :@personal:\n  %U\n  %a\n  %i" :empty-lines 1)
        ("pp" "Personal Project" entry (file "~/org/personal/projects.org")
         "* TODO %? [/] :@personal:\n  DEADLINE: %^{Deadline}t\n  %U\n  %a\n  %i")
        ("pn" "Personal Note" plain (function org-roam-node-insert)
         "%?" :empty-lines 1 :unnarrowed t)))
```

### 3. Context-Switching Agenda Views
```emacs-lisp
(setq org-agenda-custom-commands
      '(;; Work-only views
        ("w" "Work Dashboard"
         ((agenda "" ((org-agenda-span 7)
                      (org-agenda-tag-filter-preset '("+@work"))))
          (todo "NEXT" ((org-agenda-overriding-header "Work Next Actions")
                        (org-agenda-tag-filter-preset '("+@work"))))
          (todo "WAITING" ((org-agenda-overriding-header "Work Waiting For")
                           (org-agenda-tag-filter-preset '("+@work"))))
          (tags-todo "@work+project+LEVEL=2" 
                     ((org-agenda-overriding-header "Active Work Projects")))))
        
        ;; Personal-only views
        ("p" "Personal Dashboard"
         ((agenda "" ((org-agenda-span 7)
                      (org-agenda-tag-filter-preset '("+@personal"))))
          (todo "NEXT" ((org-agenda-overriding-header "Personal Next Actions")
                        (org-agenda-tag-filter-preset '("+@personal"))))
          (todo "WAITING" ((org-agenda-overriding-header "Personal Waiting For")
                           (org-agenda-tag-filter-preset '("+@personal"))))
          (tags-todo "@personal+project+LEVEL=2"
                     ((org-agenda-overriding-header "Active Personal Projects")))))
        
        ;; Unified view (everything)
        ("u" "Unified Dashboard"
         ((agenda "" ((org-agenda-span 7)))
          (todo "NEXT" ((org-agenda-overriding-header "All Next Actions")))
          (todo "WAITING" ((org-agenda-overriding-header "All Waiting For")))
          (tags-todo "project+LEVEL=2"
                     ((org-agenda-overriding-header "All Active Projects")))))))
```

## Context Switching Functions
```emacs-lisp
(defun org-work-mode ()
  "Switch to work-focused org environment."
  (interactive)
  (setq org-agenda-files '("~/org/work/" "~/org/inbox.org"))
  (message "Switched to Work Mode"))

(defun org-personal-mode ()
  "Switch to personal-focused org environment."
  (interactive)
  (setq org-agenda-files '("~/org/personal/" "~/org/inbox.org"))
  (message "Switched to Personal Mode"))

(defun org-unified-mode ()
  "Switch to unified org environment (everything)."
  (interactive)
  (setq org-agenda-files '("~/org/"))
  (message "Switched to Unified Mode"))
```

## Spacemacs Keybindings (SPC o keyspace)
```emacs-lisp
;; Context switching
(spacemacs/declare-prefix "oom" "mode")
(spacemacs/set-leader-keys
  "oomw" 'org-work-mode
  "oomp" 'org-personal-mode  
  "oomu" 'org-unified-mode)

;; Agenda dashboards  
(spacemacs/declare-prefix "ooa" "agenda")
(spacemacs/set-leader-keys
  "ooaw" (lambda () (interactive) (org-agenda nil "w"))  ; Work dashboard
  "ooap" (lambda () (interactive) (org-agenda nil "p"))  ; Personal dashboard  
  "ooau" (lambda () (interactive) (org-agenda nil "u"))  ; Unified dashboard
  "ooaa" 'org-agenda)                                    ; Standard agenda

;; Capture templates
(spacemacs/declare-prefix "ooc" "capture")
(spacemacs/set-leader-keys
  "ooci" (lambda () (interactive) (org-capture nil "i"))   ; Inbox
  "oocw" (lambda () (interactive) (org-capture nil "wt"))  ; Work task
  "oocp" (lambda () (interactive) (org-capture nil "pt"))  ; Personal task
  "oocn" (lambda () (interactive) (org-capture nil "wn"))  ; Work note
  "oocN" (lambda () (interactive) (org-capture nil "pn"))  ; Personal note
  "oocc" 'org-capture)                                     ; Full capture menu

;; Quick navigation
(spacemacs/declare-prefix "oog" "goto")
(spacemacs/set-leader-keys
  "oogi" (lambda () (interactive) (find-file "~/org/inbox.org"))
  "oogw" (lambda () (interactive) (find-file "~/org/work/gtd.org"))
  "oogp" (lambda () (interactive) (find-file "~/org/personal/gtd.org"))
  "oogr" 'org-roam-node-find)

;; Org-roam integration
(spacemacs/declare-prefix "oor" "roam")
(spacemacs/set-leader-keys
  "oorf" 'org-roam-node-find
  "oori" 'org-roam-node-insert
  "oort" 'org-roam-dailies-goto-today
  "oory" 'org-roam-dailies-goto-yesterday
  "oord" 'org-roam-dailies-goto-date
  "oorc" 'org-roam-dailies-capture-today
  "oorb" 'org-roam-buffer-toggle)
```

## Implementation Strategy

### Phase 1: Preserve Visual System
- Comment out but keep all existing visual customizations
- Preserve org-heading-colors-schemes (21 schemes)
- Preserve org-superstar-bullet-sets (9 sets)
- Keep custom font faces and typography
- Maintain interactive switching functions

### Phase 2: Add Core GTD
- Simple TODO keywords (NEXT/TODO/WAITING | DONE/CANCELLED)
- Practical capture templates (inbox, project, roam-note)
- Focused agenda views (dashboard, next actions, projects)
- File organization structure

### Phase 3: Roam-GTD Integration
- org-roam-gtd-capture: Capture with auto-linking
- org-roam-project-link: Link agenda items to roam notes
- org-roam-actionable-review: Process #actionable roam notes

## Benefits

### For Daily Workflow
- **Single capture point**: Inbox for everything, sort later
- **Clear next actions**: Always know what to do next  
- **Project tracking**: See progress and outcomes clearly
- **Knowledge connection**: Context from roam notes enhances action clarity

### For Knowledge Management
- **Persistent learning**: All research/notes in permanent roam structure
- **Project context**: Rich background for each project in roam
- **Idea development**: Thoughts mature in roam before becoming actions
- **Reference system**: Easy access to past decisions and resources

### For Long-term Productivity
- **Reduced cognitive load**: System handles organization automatically
- **Better decisions**: More context available for each action
- **Learning capture**: Knowledge compounds over time in roam
- **Visual motivation**: Beautiful interface encourages daily use

## Workflow Example: Remote Developer Day

**Morning Startup**: `SPC o o m w` (work mode) → `SPC o o a w` (work dashboard)
**Quick Planning**: `SPC o o r t` (today's roam note) for session planning
**Deep Work**: Seamless capture with `SPC o o c n` for insights
**Meeting Notes**: Daily note sections for standup, architecture reviews
**Action Capture**: `SPC o o c w` for immediate task creation
**Context Switching**: `SPC o o m p` for personal mode when needed
**End of Day**: Dashboard review and daily note completion

## Observations
- [integration] Seamlessly blends knowledge management with action management
- [visual] Preserves extensive visual customization work
- [workflow] Context switching reduces cognitive overhead
- [capture] Multiple capture paths accommodate different thinking modes
- [knowledge] Persistent roam notes provide rich project context
- [productivity] Dashboard views maintain focus and clarity