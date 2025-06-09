# Org-GTD Phase 2: Capture Templates and Agenda Views Implementation

## Overview

This document outlines a detailed implementation plan for Phase 2 of the Org-GTD system: Capture Templates and Agenda Views. This phase builds upon the core infrastructure from Phase 1 to create a comprehensive capture workflow and powerful agenda dashboards for effective task management.

## Purpose

This implementation aims to:

1. **Streamline Task Capture** - Create context-aware capture templates that make adding tasks frictionless
2. **Enable Smart Dashboards** - Build custom agenda views that provide focused, actionable task lists
3. **Configure Intelligent Refile** - Set up smart refile targets to quickly organize captured items

## Prerequisites

Before starting the implementation:

- [ ] Phase 1 (Core Infrastructure) is complete and tested
- [ ] GTD directory structure exists and is validated
- [ ] TODO keywords and tags are properly configured
- [ ] Understanding of org-capture and org-agenda systems

## Implementation Plan

### Phase 2.1: Enhanced Capture Templates

#### Task 2.1.1: Basic GTD Capture Templates

**Status:** 📝 PLANNED

**Purpose:** Create the foundational capture templates for inbox, work tasks, personal tasks, and quick notes.

**Implementation Checklist:**
- [ ] Configure org-capture with default target files
- [ ] Create universal inbox template
- [ ] Create work task template with context
- [ ] Create personal task template with context
- [ ] Create quick note template
- [ ] Add capture template selection hints

**Reference Implementation:**
```elisp
;; Configure default capture targets
(setq org-default-notes-file (rk/org-file "inbox.org"))

;; Basic GTD capture templates
(setq org-capture-templates
      '(("i" "Inbox" entry (file+headline org-default-notes-file "Inbox")
         "* TODO %?\n  CAPTURED: %U\n  %i"
         :empty-lines 1)
        
        ("t" "Task")
        ("tw" "Work Task" entry (file+headline (rk/org-file "work/gtd.org") "Tasks")
         "* TODO %? :@work:\n  SCHEDULED: %t\n  CAPTURED: %U\n  %i"
         :empty-lines 1)
        
        ("tp" "Personal Task" entry (file+headline (rk/org-file "personal/gtd.org") "Tasks")
         "* TODO %? :@personal:\n  SCHEDULED: %t\n  CAPTURED: %U\n  %i"
         :empty-lines 1)
        
        ("n" "Quick Note" entry (file+headline org-default-notes-file "Notes")
         "* %? :note:\n  CAPTURED: %U\n  %i"
         :empty-lines 1
         :immediate-finish t)))

;; Add template group descriptions
(defun rk/capture-template-help ()
  "Display capture template descriptions."
  (interactive)
  (message "Capture Templates: [i]nbox [t]ask ([w]ork/[p]ersonal) [n]ote"))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

#### Task 2.1.2: Advanced Capture Templates

**Status:** 📝 PLANNED

**Purpose:** Implement sophisticated capture templates for projects, meetings, interruptions, and energy-based tasks.

**Implementation Checklist:**
- [ ] Create project capture with metadata
- [ ] Create meeting template with attendees
- [ ] Create interruption log template
- [ ] Create energy-context quick capture
- [ ] Create voice note placeholder template
- [ ] Create email task template

**Reference Implementation:**
```elisp
;; Advanced capture templates
(add-to-list 'org-capture-templates
             '("p" "Project")
             t)

(add-to-list 'org-capture-templates
             '("pw" "Work Project" entry (file+headline (rk/org-file "work/projects.org") "Projects")
               "* TODO %^{Project Name} [/] :@work:project:\n  :PROPERTIES:\n  :CATEGORY: %^{Category|development|research|management}\n  :EFFORT: %^{Effort estimate|1d|3d|1w|2w|1m}\n  :END:\n  CAPTURED: %U\n  \n  %^{Project Description}\n  \n** TODO Define project objectives\n** TODO Create project plan\n** TODO Set up project structure\n"
               :empty-lines 1)
             t)

(add-to-list 'org-capture-templates
             '("m" "Meeting" entry (file+headline (rk/org-file "work/gtd.org") "Meetings")
               "* MEETING %^{Meeting Title} :@work:meeting:\n  SCHEDULED: %^T\n  :PROPERTIES:\n  :ATTENDEES: %^{Attendees}\n  :LOCATION: %^{Location|Conference Room|Zoom|Teams}\n  :END:\n  CAPTURED: %U\n  \n  Agenda:\n  %?\n  \n  Notes:\n  \n  Action Items:\n  "
               :empty-lines 1)
             t)

(add-to-list 'org-capture-templates
             '("x" "Interruption" entry (file+datetree (rk/org-file "work/gtd.org"))
               "* INTERRUPTION %U :interruption:\n  Interrupted by: %^{Who}\n  Task interrupted: %a\n  Reason: %^{Reason}\n  Duration: %^{Duration|5m|10m|15m|30m}\n  %?"
               :clock-in t :clock-resume t)
             t)

(add-to-list 'org-capture-templates
             '("e" "Energy-based Capture")
             t)

(add-to-list 'org-capture-templates
             '("el" "Low Energy Task" entry (file+headline org-default-notes-file "Inbox")
               "* TODO %? :@low_energy:@15min:\n  CAPTURED: %U"
               :immediate-finish t)
             t)

(add-to-list 'org-capture-templates
             '("eh" "High Energy Task" entry (file+headline org-default-notes-file "Inbox")
               "* NEXT %? :@high_energy:@deep:\n  CAPTURED: %U"
               :immediate-finish t)
             t)
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

### Phase 2.2: Custom Agenda Commands

#### Task 2.2.1: Core GTD Dashboards

**Status:** 📝 PLANNED

**Purpose:** Create the three primary dashboards (work, personal, unified) that form the foundation of daily task management.

**Implementation Checklist:**
- [ ] Configure agenda file lists for each context
- [ ] Create work dashboard with custom blocks
- [ ] Create personal dashboard with custom blocks
- [ ] Create unified dashboard combining both contexts
- [ ] Add dashboard navigation shortcuts
- [ ] Configure agenda appearance settings

**Reference Implementation:**
```elisp
;; Configure custom agenda commands
(setq org-agenda-custom-commands
      '(("w" "Work Dashboard" 
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-files (list (rk/org-file "work/gtd.org")
                                              (rk/org-file "work/projects.org")
                                              (rk/org-file "inbox.org")))
                      (org-agenda-prefix-format " %i %-15:c%?-12t% s")))
          (todo "NEXT" ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-files (list (rk/org-file "work/gtd.org")
                                                (rk/org-file "work/projects.org")))
                        (org-agenda-prefix-format " %i %-15:c")
                        (org-agenda-todo-ignore-scheduled 'future)))
          (todo "WAITING" ((org-agenda-overriding-header "Waiting For")
                           (org-agenda-files (list (rk/org-file "work/gtd.org")
                                                   (rk/org-file "work/projects.org")))
                           (org-agenda-prefix-format " %i %-15:c %s")))
          (tags "project+TODO=\"TODO\"" ((org-agenda-overriding-header "Active Projects")
                                          (org-agenda-files (list (rk/org-file "work/projects.org")))))))
        
        ("p" "Personal Dashboard"
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-files (list (rk/org-file "personal/gtd.org")
                                              (rk/org-file "personal/projects.org")
                                              (rk/org-file "inbox.org")))
                      (org-agenda-prefix-format " %i %-15:c%?-12t% s")))
          (todo "NEXT" ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-files (list (rk/org-file "personal/gtd.org")
                                                (rk/org-file "personal/projects.org")))
                        (org-agenda-prefix-format " %i %-15:c")
                        (org-agenda-todo-ignore-scheduled 'future)))
          (todo "WAITING" ((org-agenda-overriding-header "Waiting For")
                           (org-agenda-files (list (rk/org-file "personal/gtd.org")
                                                   (rk/org-file "personal/projects.org")))
                           (org-agenda-prefix-format " %i %-15:c %s")))
          (tags "project+TODO=\"TODO\"" ((org-agenda-overriding-header "Active Projects")
                                          (org-agenda-files (list (rk/org-file "personal/projects.org")))))))
        
        ("u" "Unified Dashboard"
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-files (rk/org-gtd-files))
                      (org-agenda-prefix-format " %i %-15:c%?-12t% s")))
          (todo "NEXT" ((org-agenda-overriding-header "All Next Actions")
                        (org-agenda-files (rk/org-gtd-files))
                        (org-agenda-prefix-format " %i %-15:c")
                        (org-agenda-todo-ignore-scheduled 'future)))
          (todo "WAITING" ((org-agenda-overriding-header "All Waiting Items")
                           (org-agenda-files (rk/org-gtd-files))
                           (org-agenda-prefix-format " %i %-15:c %s")))
          (tags "project+TODO=\"TODO\"" ((org-agenda-overriding-header "All Active Projects")
                                          (org-agenda-files (rk/org-gtd-files))))))))

;; Configure agenda appearance
(setq org-agenda-window-setup 'current-window
      org-agenda-restore-windows-after-quit t
      org-agenda-start-with-log-mode nil
      org-agenda-span 'day
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t)
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

#### Task 2.2.2: Specialized Agenda Views

**Status:** 📝 PLANNED

**Purpose:** Create specialized views for energy-based planning, focus mode, project overviews, and maintenance tasks.

**Implementation Checklist:**
- [ ] Create energy-based task views
- [ ] Create focus mode (top 3 tasks)
- [ ] Create project overview dashboard
- [ ] Create stalled items view
- [ ] Create inbox processing view
- [ ] Create review-ready view

**Reference Implementation:**
```elisp
;; Energy-based views
(add-to-list 'org-agenda-custom-commands
             '("e" "Energy-based Views")
             t)

(add-to-list 'org-agenda-custom-commands
             '("el" "Low Energy Tasks"
               ((tags-todo "@low_energy|@routine|@5min|@15min"
                           ((org-agenda-overriding-header "Low Energy / Quick Tasks")
                            (org-agenda-sorting-strategy '(effort-up category-keep))
                            (org-agenda-prefix-format " %i %-15:c [%e] ")))))
             t)

(add-to-list 'org-agenda-custom-commands
             '("eh" "High Energy Tasks"
               ((tags-todo "@high_energy|@creative|@deep"
                           ((org-agenda-overriding-header "High Energy / Deep Work")
                            (org-agenda-sorting-strategy '(priority-down effort-down))
                            (org-agenda-prefix-format " %i %-15:c [%e] ")))))
             t)

;; Focus mode - Top 3 next actions
(add-to-list 'org-agenda-custom-commands
             '("f" "Focus Mode - Top 3"
               ((todo "NEXT"
                      ((org-agenda-overriding-header "🎯 Today's Top 3 Focus Items")
                       (org-agenda-max-entries 3)
                       (org-agenda-files (rk/org-gtd-files))
                       (org-agenda-sorting-strategy '(priority-down effort-up))))))
             t)

;; Project overview
(add-to-list 'org-agenda-custom-commands
             '("P" "Project Overview"
               ((tags "project"
                      ((org-agenda-overriding-header "All Projects")
                       (org-agenda-prefix-format " %i %-20:c [%e] %(org-format-outline-path (org-get-outline-path))")
                       (org-agenda-sorting-strategy '(category-keep priority-down))
                       (org-agenda-show-inherited-tags nil)))))
             t)

;; Stalled items
(add-to-list 'org-agenda-custom-commands
             '("s" "Stalled Items"
               ((todo "TODO"
                      ((org-agenda-overriding-header "Stalled Tasks (no recent activity)")
                       (org-agenda-files (rk/org-gtd-files))
                       (org-agenda-skip-function 'rk/skip-recent-tasks)
                       (org-agenda-prefix-format " %i %-15:c [%t] ")))
                (tags "project+TODO=\"TODO\""
                      ((org-agenda-overriding-header "Stalled Projects (no NEXT actions)")
                       (org-agenda-skip-function 'rk/skip-projects-with-next)))))
             t)

;; Helper functions for specialized views
(defun rk/skip-recent-tasks ()
  "Skip tasks that have been modified in the last 7 days."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (last-mod (org-entry-get nil "LAST_MODIFIED")))
    (if (and last-mod
             (< (time-to-days (time-subtract (current-time)
                                             (org-time-string-to-time last-mod)))
                7))
        subtree-end
      nil)))

(defun rk/skip-projects-with-next ()
  "Skip projects that have NEXT actions."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (re-search-forward "^\\*+ NEXT" subtree-end t)
        subtree-end
      nil)))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

### Phase 2.3: Refile Configuration

#### Task 2.3.1: Intelligent Refile Setup

**Status:** 📝 PLANNED

**Purpose:** Configure smart refile targets and helper functions to quickly organize captured items into their proper locations.

**Implementation Checklist:**
- [ ] Configure refile target paths
- [ ] Set up refile completion settings
- [ ] Create context-aware refile targets
- [ ] Add quick refile functions
- [ ] Configure refile verification
- [ ] Add refile hydra for quick access

**Reference Implementation:**
```elisp
;; Configure refile targets
(setq org-refile-targets
      '((nil :maxlevel . 3)  ; Current buffer
        (org-agenda-files :maxlevel . 3)
        ((rk/org-file "work/projects.org") :level . 1)
        ((rk/org-file "personal/projects.org") :level . 1)
        ((rk/org-file "work/someday.org") :maxlevel . 2)
        ((rk/org-file "personal/someday.org") :maxlevel . 2)))

;; Configure refile behavior
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-history-length 25
      org-refile-use-cache t)

;; Quick refile functions
(defun rk/refile-to-work-gtd ()
  "Quickly refile to work GTD."
  (interactive)
  (let ((org-refile-targets '(((rk/org-file "work/gtd.org") :maxlevel . 2))))
    (org-refile)))

(defun rk/refile-to-personal-gtd ()
  "Quickly refile to personal GTD."
  (interactive)
  (let ((org-refile-targets '(((rk/org-file "personal/gtd.org") :maxlevel . 2))))
    (org-refile)))

(defun rk/refile-to-current-clock ()
  "Refile current entry to currently clocked task."
  (interactive)
  (when (and (org-clocking-p)
             (not (eq (marker-buffer org-clock-marker) (current-buffer))))
    (let ((org-refile-targets '((nil :maxlevel . 5))))
      (org-refile nil nil (list (save-excursion
                                  (org-clock-goto)
                                  (org-get-heading t t t t))
                                (buffer-file-name (marker-buffer org-clock-marker))
                                nil
                                (marker-position org-clock-marker))))))

;; Refile verification
(defun rk/verify-refile-target ()
  "Verify that refile target is not a TODO keyword task."
  (not (member (nth 2 (org-heading-components)) org-todo-keywords-1)))

(setq org-refile-target-verify-function 'rk/verify-refile-target)

;; Create refile hydra for quick access
(with-eval-after-load 'hydra
  (defhydra rk/org-refile-hydra (:hint nil :exit t)
    "
Refile to: _w_ork  _p_ersonal  _c_lock  _W_ork-project  _P_ersonal-project  _i_nteractive
          _s_omeday-work  _S_omeday-personal  _a_rchive  _q_uit
"
    ("w" rk/refile-to-work-gtd)
    ("p" rk/refile-to-personal-gtd)
    ("c" rk/refile-to-current-clock)
    ("W" (let ((org-refile-targets '(((rk/org-file "work/projects.org") :level . 1))))
           (org-refile)))
    ("P" (let ((org-refile-targets '(((rk/org-file "personal/projects.org") :level . 1))))
           (org-refile)))
    ("s" (let ((org-refile-targets '(((rk/org-file "work/someday.org") :maxlevel . 2))))
           (org-refile)))
    ("S" (let ((org-refile-targets '(((rk/org-file "personal/someday.org") :maxlevel . 2))))
           (org-refile)))
    ("a" (org-archive-subtree))
    ("i" org-refile)
    ("q" nil)))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

## Implementation Process

Each task should follow this standard process:

1. **Development:**
   - Add code to `codelahoma-org.org` in the appropriate section
   - Tangle the file to generate `codelahoma-org.el`
   - Reload the configuration or restart Spacemacs

2. **Testing:**
   - Test each capture template thoroughly
   - Verify agenda views display correctly
   - Test refile operations to all targets
   - Validate quick capture workflows
   - Test specialized views and filters

3. **Documentation:**
   - Document all capture templates in user guide
   - Create agenda view cheatsheet
   - Document refile shortcuts
   - Update keybinding reference

4. **Commit:**
   - Stage changes to both .org and .el files
   - Commit with descriptive message:
     ```
     feat: implement GTD capture templates and agenda views
     
     - Add comprehensive capture templates for all contexts
     - Create work/personal/unified dashboard views
     - Implement energy-based and specialized agenda views
     - Configure intelligent refile with quick shortcuts
     ```

## Testing Strategy

### Manual Testing Checklist:

1. **Capture Templates:**
   - [ ] Test all basic capture templates (inbox, work, personal, note)
   - [ ] Test advanced templates (project, meeting, interruption)
   - [ ] Verify template expansion and property prompts
   - [ ] Test immediate-finish templates
   - [ ] Verify capture to correct locations

2. **Agenda Views:**
   - [ ] Test work dashboard shows only work items
   - [ ] Test personal dashboard shows only personal items
   - [ ] Test unified dashboard combines both contexts
   - [ ] Verify energy-based filtering works
   - [ ] Test focus mode limits to 3 items

3. **Refile Operations:**
   - [ ] Test refile to all configured targets
   - [ ] Verify quick refile functions work
   - [ ] Test refile to clocked task
   - [ ] Verify refile cache updates
   - [ ] Test refile hydra interface

4. **Integration:**
   - [ ] Capture → Refile → Agenda workflow
   - [ ] Verify captured items appear in correct agenda views
   - [ ] Test agenda commands from different contexts

## Rollback Plan

In case of issues:

1. Reset org-capture-templates to minimal configuration
2. Clear org-agenda-custom-commands
3. Reset org-refile-targets to defaults
4. Comment out new functions in `codelahoma-org.org`
5. Restart Spacemacs with minimal configuration

## Conclusion

This implementation provides a comprehensive capture and agenda system that makes GTD workflows smooth and efficient. The combination of smart capture templates, focused agenda views, and intelligent refile creates a powerful task management environment that adapts to different contexts and energy levels.