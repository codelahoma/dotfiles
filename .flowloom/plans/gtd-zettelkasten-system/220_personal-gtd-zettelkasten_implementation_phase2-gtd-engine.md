# Personal GTD-Zettelkasten Phase 2 Implementation Plan

## Overview

This document outlines the detailed implementation plan for Phase 2: Custom GTD Engine Core of the Personal GTD-Zettelkasten Hybrid System. This phase builds the core GTD functionality on top of the foundation established in Phase 1, implementing task management, capture templates, and inbox processing workflows.

## Purpose

This implementation aims to:

1. **Implement Task State Management** - Create a flexible state machine for GTD task states
2. **Build Capture System** - Develop intelligent capture templates with context detection
3. **Create Inbox Processing** - Implement workflows for clarifying and organizing
4. **Establish Project Structure** - Define relationships between projects, tasks, and areas

## Prerequisites

Before starting Phase 2 implementation:

- [x] Phase 1 complete with all modules loaded
- [x] Directory structure validated and accessible
- [x] Org-roam configured and working
- [x] Keybinding framework established under `SPC o o`
- [ ] Development workflow tested and functional

## Implementation Plan

### Phase 2: Custom GTD Engine Core (Week 2-3)

#### Task 2.1: Implement Task State Management

**Status:** ✅ COMPLETE

**Purpose:** Create a comprehensive task state system supporting GTD methodology with personal workflow customization.

**Implementation Checklist:**
- [x] Define org-todo-keywords for GTD states
- [x] Create state transition functions
- [x] Implement automatic state tracking
- [x] Add state change logging
- [x] Create quick state change keybindings

**Reference Implementation:**
```elisp
;; In codelahoma-gtd-core.el:

(defcustom codelahoma-gtd-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "ACTIVE(a)" "|" "DONE(d)")
    (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
    (sequence "PROJECT(p)" "|" "COMPLETED(C)"))
  "GTD task state keywords."
  :type 'sexp
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-todo-keyword-faces
  '(("TODO" . (:foreground "#dc752f" :weight bold))
    ("NEXT" . (:foreground "#4f97d7" :weight bold))
    ("ACTIVE" . (:foreground "#f2241f" :weight bold))
    ("DONE" . (:foreground "#86dc2f" :weight bold))
    ("WAITING" . (:foreground "#b1951d" :weight bold))
    ("HOLD" . (:foreground "#a45bad" :weight bold))
    ("CANCELLED" . (:foreground "#9f8766" :weight bold :strike-through t))
    ("PROJECT" . (:foreground "#2d9574" :weight bold :box t))
    ("COMPLETED" . (:foreground "#86dc2f" :weight bold :box t)))
  "Face properties for GTD todo keywords."
  :type 'alist
  :group 'codelahoma-gtd)

(defun codelahoma-gtd-setup-todo-keywords ()
  "Configure org-mode with GTD todo keywords."
  (setq org-todo-keywords codelahoma-gtd-todo-keywords)
  (setq org-todo-keyword-faces codelahoma-gtd-todo-keyword-faces)
  
  ;; State change logging
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  
  ;; Fast todo selection
  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil))

(defun codelahoma-gtd-set-next-action ()
  "Mark current task as NEXT action."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-todo "NEXT")
    (org-priority ?A)
    (when (org-entry-get nil "DELEGATED_TO")
      (org-set-property "DELEGATED_TO" nil))
    (message "Task marked as NEXT action")))

(defun codelahoma-gtd-delegate-task ()
  "Delegate current task and set to WAITING."
  (interactive)
  (when (eq major-mode 'org-mode)
    (let ((delegate-to (read-string "Delegate to: ")))
      (org-todo "WAITING")
      (org-set-property "DELEGATED_TO" delegate-to)
      (org-set-property "DELEGATED_ON" (format-time-string "[%Y-%m-%d %a]"))
      (org-entry-put nil "WAITING_REASON" (format "Delegated to %s" delegate-to))
      (message "Task delegated to %s" delegate-to))))

(defun codelahoma-gtd-convert-to-project ()
  "Convert current task to a project."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-todo "PROJECT")
    (org-set-property "PROJECT_CREATED" (format-time-string "[%Y-%m-%d %a]"))
    ;; Add project planning template
    (org-end-of-subtree)
    (insert "\n** Outcomes\n- [ ] \n\n** Next Actions\n*** NEXT \n\n** Notes\n")
    (message "Converted to project - add outcomes and next actions")))
```

**Tests:**
- Verify all state transitions work correctly
- Test state change logging
- Ensure properties are set appropriately
- Check fast todo selection interface
- Validate project conversion template

**Implementation Notes:**
- Added comprehensive GTD todo states: TODO, NEXT, ACTIVE, WAITING, HOLD, CANCELLED, PROJECT, COMPLETED
- Implemented state transition functions: set-next-action, delegate-task, convert-to-project
- Added custom faces for visual distinction of states
- Integrated with org-mode's built-in logging (drawer-based)
- Created keybindings under `SPC o o t` for task state management
- Added project and area structure definitions for future use
- Created test file test-task-states.el for verification
- All functionality integrated into codelahoma-gtd-core.el

**Commit:** 
```
feat(gtd): Implement Phase 2 Task State Management

- Added GTD-specific todo keywords with custom faces
- Created state transition functions (next action, delegate, project)
- Integrated state change logging with org-mode
- Added keybindings under SPC o o t namespace
- Created test suite for state management
- Updated initialization to set up todo keywords
```

#### Task 2.2: Build Intelligent Capture System

**Status:** ✅ COMPLETE

**Purpose:** Create a sophisticated capture system with smart templates and context detection for rapid thought collection.

**Implementation Checklist:**
- [x] Design capture template structure
- [x] Implement quick capture functions
- [x] Add context detection (time, location, buffer)
- [x] Create template selection logic
- [x] Build capture finalization hooks

**Reference Implementation:**
```elisp
;; In codelahoma-gtd-capture.el:

(defcustom codelahoma-gtd-capture-templates
  '(("i" "Inbox" entry (file "~/personal/org-files/gtd/inbox.org")
     "* %?\n:PROPERTIES:\n:CAPTURED: %U\n:CONTEXT: %a\n:END:\n"
     :empty-lines 1)
    
    ("t" "Task with context" entry (file "~/personal/org-files/gtd/inbox.org")
     "* TODO %?\n:PROPERTIES:\n:CAPTURED: %U\n:CONTEXT: %a\n:END:\n%i"
     :empty-lines 1)
    
    ("p" "Phone call" entry (file "~/personal/org-files/gtd/inbox.org")
     "* PHONE %? :PHONE:\n:PROPERTIES:\n:CAPTURED: %U\n:PHONE_NUMBER: %^{Phone number}\n:END:\n"
     :clock-in t :clock-resume t)
    
    ("m" "Meeting" entry (file "~/personal/org-files/gtd/inbox.org")
     "* MEETING with %? :MEETING:\n:PROPERTIES:\n:CAPTURED: %U\n:ATTENDEES: %^{Attendees}\n:LOCATION: %^{Location|Office|Remote|Other}\n:END:\n\n** Agenda\n- \n\n** Notes\n"
     :clock-in t :clock-resume t)
    
    ("e" "Email to process" entry (file "~/personal/org-files/gtd/inbox.org")
     "* EMAIL %? :EMAIL:\n:PROPERTIES:\n:CAPTURED: %U\n:FROM: %^{From}\n:SUBJECT: %^{Subject}\n:END:\n"
     :empty-lines 1)
    
    ("w" "Weekly Review" entry (file+datetree "~/personal/org-files/gtd/reviews/weekly-reviews.org")
     "* Weekly Review :REVIEW:\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n\n** Completed This Week\n%?\n\n** Challenges\n\n** Next Week Focus\n\n** Notes\n"
     :jump-to-captured t))
  "GTD capture templates."
  :type 'sexp
  :group 'codelahoma-gtd)

(defun codelahoma-gtd-capture-inbox ()
  "Quick capture to inbox."
  (interactive)
  (org-capture nil "i"))

(defun codelahoma-gtd-capture-with-context ()
  "Capture with automatic context detection."
  (interactive)
  (let ((context (codelahoma-gtd-detect-context)))
    (org-capture nil (if (string-match-p "email\\|mail" context) "e" "t"))
    (when context
      (org-set-property "AUTO_CONTEXT" context))))

(defun codelahoma-gtd-detect-context ()
  "Detect current context for intelligent capture."
  (cond
   ;; In email buffer
   ((derived-mode-p 'message-mode 'mail-mode 'mu4e-view-mode)
    "email")
   ;; In code buffer
   ((derived-mode-p 'prog-mode)
    (format "coding:%s" (or (projectile-project-name) "unknown")))
   ;; In web browser
   ((string-match-p "\\*eww\\*\\|\\*w3m\\*" (buffer-name))
    "web-browsing")
   ;; In org-roam note
   ((and (derived-mode-p 'org-mode)
         (org-roam-buffer-p))
    "knowledge-work")
   ;; Default
   (t nil)))

(defun codelahoma-gtd-capture-finalize-hook ()
  "Hook run after capture finalization."
  ;; Add creation timestamp
  (when (and (eq (org-capture-get :type) 'entry)
             (not (org-entry-get nil "CREATED")))
    (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]")))
  
  ;; Auto-tag based on content
  (codelahoma-gtd-auto-tag)
  
  ;; Save all org buffers
  (org-save-all-org-buffers))

(defun codelahoma-gtd-auto-tag ()
  "Automatically add tags based on entry content."
  (let ((title (org-get-heading t t t t)))
    (cond
     ((string-match-p "\\b\\(call\\|phone\\|contact\\)\\b" title)
      (org-set-tags ":PHONE:"))
     ((string-match-p "\\b\\(email\\|mail\\|reply\\)\\b" title)
      (org-set-tags ":EMAIL:"))
     ((string-match-p "\\b\\(meeting\\|discuss\\|conversation\\)\\b" title)
      (org-set-tags ":MEETING:"))
     ((string-match-p "\\b\\(buy\\|purchase\\|order\\)\\b" title)
      (org-set-tags ":ERRAND:")))))

;; Setup capture templates
(defun codelahoma-gtd-setup-capture ()
  "Configure org-capture for GTD."
  (setq org-capture-templates codelahoma-gtd-capture-templates)
  (add-hook 'org-capture-after-finalize-hook 'codelahoma-gtd-capture-finalize-hook))

;; Keybindings for capture
(spacemacs/set-leader-keys
  "o o c i" 'codelahoma-gtd-capture-inbox
  "o o c c" 'codelahoma-gtd-capture-with-context
  "o o c t" (lambda () (interactive) (org-capture nil "t"))
  "o o c p" (lambda () (interactive) (org-capture nil "p"))
  "o o c m" (lambda () (interactive) (org-capture nil "m"))
  "o o c e" (lambda () (interactive) (org-capture nil "e")))
```

**Tests:**
- Test each capture template
- Verify context detection in different modes
- Check auto-tagging functionality
- Ensure properties are set correctly
- Validate capture finalization hooks

**Implementation Notes:**
- Implemented comprehensive capture templates: inbox, task, phone, meeting, email, weekly review
- Added intelligent context detection for email, coding, web browsing, and knowledge work
- Created auto-tagging system based on content keywords (phone, email, meeting, errand)
- Implemented capture finalization hooks for timestamps and auto-save
- Added quick capture commands for specific types
- Updated keybindings with new capture options under `SPC o o c`
- Created test-capture.el for verification
- Ensured reviews directory exists for weekly review captures
- Integrated capture setup into GTD initialization

**Commit:** 
```
feat(gtd): Implement Phase 2 Intelligent Capture System

- Added comprehensive GTD capture templates
- Implemented context detection for smart captures
- Created auto-tagging based on content
- Added capture finalization hooks
- Extended keybindings for all capture types
- Created test suite for capture functionality
- Integrated capture setup into initialization
```

#### Task 2.3: Implement Inbox Processing Workflow

**Status:** ✅ COMPLETE

**Purpose:** Create efficient workflows for processing inbox items according to GTD methodology.

**Implementation Checklist:**
- [x] Build inbox processing interface
- [x] Create clarification functions
- [x] Implement refile operations
- [x] Add bulk processing capabilities
- [x] Create processing statistics

**Reference Implementation:**
```elisp
;; In codelahoma-gtd-process.el:

(defcustom codelahoma-gtd-refile-targets
  '(("~/personal/org-files/gtd/projects.org" :maxlevel . 3)
    ("~/personal/org-files/gtd/next-actions.org" :maxlevel . 2)
    ("~/personal/org-files/gtd/someday.org" :maxlevel . 2)
    ("~/personal/org-files/gtd/waiting-for.org" :maxlevel . 2)
    ("~/personal/org-files/areas/" :maxlevel . 2))
  "Refile targets for GTD processing."
  :type 'alist
  :group 'codelahoma-gtd)

(defun codelahoma-gtd-process-inbox ()
  "Open inbox in processing mode."
  (interactive)
  (find-file (expand-file-name "inbox.org" codelahoma-gtd-directory))
  (goto-char (point-min))
  (org-next-visible-heading 1)
  (codelahoma-gtd-processing-mode 1)
  (message "Processing inbox - use 'h' for help"))

(define-minor-mode codelahoma-gtd-processing-mode
  "Minor mode for GTD inbox processing."
  :lighter " GTD-Process"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "n" 'codelahoma-gtd-process-next-item)
            (define-key map "p" 'codelahoma-gtd-process-previous-item)
            (define-key map "r" 'codelahoma-gtd-refile-current)
            (define-key map "d" 'codelahoma-gtd-delete-current)
            (define-key map "t" 'org-todo)
            (define-key map "s" 'org-schedule)
            (define-key map "." 'org-priority)
            (define-key map ":" 'org-set-tags-command)
            (define-key map "w" 'codelahoma-gtd-delegate-task)
            (define-key map "c" 'codelahoma-gtd-convert-to-project)
            (define-key map "?" 'codelahoma-gtd-process-help)
            (define-key map "q" 'codelahoma-gtd-finish-processing)
            map))

(defun codelahoma-gtd-process-next-item ()
  "Move to next inbox item."
  (interactive)
  (org-next-visible-heading 1)
  (org-narrow-to-subtree)
  (org-show-entry)
  (org-show-children)
  (message "Item %d of %d" 
           (codelahoma-gtd-current-item-number)
           (codelahoma-gtd-total-inbox-items)))

(defun codelahoma-gtd-process-previous-item ()
  "Move to previous inbox item."
  (interactive)
  (widen)
  (org-previous-visible-heading 1)
  (org-narrow-to-subtree)
  (org-show-entry)
  (org-show-children))

(defun codelahoma-gtd-refile-current ()
  "Refile current item with GTD logic."
  (interactive)
  (let* ((heading (org-get-heading t t t t))
         (suggested-target (codelahoma-gtd-suggest-refile-target heading)))
    (when suggested-target
      (message "Suggested: %s" suggested-target))
    (org-refile)))

(defun codelahoma-gtd-suggest-refile-target (heading)
  "Suggest a refile target based on heading content."
  (cond
   ((string-match-p "\\bproject\\b\\|\\bplan\\b\\|\\bproposal\\b" heading)
    "projects.org")
   ((string-match-p "\\bwaiting\\b\\|\\bfollow.?up\\b\\|\\bdelegated\\b" heading)
    "waiting-for.org")
   ((string-match-p "\\bsomeday\\b\\|\\bmaybe\\b\\|\\bidea\\b" heading)
    "someday.org")
   ((org-entry-get nil "Effort")
    "next-actions.org")
   (t nil)))

(defun codelahoma-gtd-delete-current ()
  "Delete current inbox item after confirmation."
  (interactive)
  (when (y-or-n-p "Delete this item? ")
    (widen)
    (org-cut-subtree)
    (message "Item deleted")
    (codelahoma-gtd-process-next-item)))

(defun codelahoma-gtd-bulk-process ()
  "Process multiple inbox items at once."
  (interactive)
  (org-agenda nil "i") ; Custom agenda view for inbox
  (org-agenda-bulk-mark-regexp ".")
  (message "All items marked - choose bulk action"))

(defun codelahoma-gtd-clarify-item ()
  "Interactive clarification for current item."
  (interactive)
  (let* ((actionable (y-or-n-p "Is this actionable? "))
         (effort (when actionable
                  (read-string "Estimated effort (5m, 1h, 2d): " "30m"))))
    (if actionable
        (progn
          (org-todo "TODO")
          (when effort (org-set-property "Effort" effort))
          (when (y-or-n-p "Is this a project? ")
            (codelahoma-gtd-convert-to-project))
          (when (y-or-n-p "Can be done in 2 minutes? ")
            (org-todo "NEXT")
            (org-priority ?A)))
      ;; Not actionable
      (if (y-or-n-p "Reference material? ")
          (org-set-tags ":REFERENCE:")
        (when (y-or-n-p "Someday/maybe? ")
          (org-set-tags ":SOMEDAY:"))))))

(defun codelahoma-gtd-process-statistics ()
  "Show inbox processing statistics."
  (interactive)
  (let* ((total (codelahoma-gtd-total-inbox-items))
         (processed (- total (codelahoma-gtd-current-item-number)))
         (percent (if (> total 0) 
                     (/ (* 100 processed) total) 
                   0)))
    (message "Processed %d of %d items (%.0f%%)" processed total percent)))

(defun codelahoma-gtd-finish-processing ()
  "Finish processing and clean up."
  (interactive)
  (widen)
  (codelahoma-gtd-processing-mode -1)
  (org-save-all-org-buffers)
  (message "Inbox processing complete")
  (when (= 0 (codelahoma-gtd-total-inbox-items))
    (message "Inbox zero achieved! 🎉")))

;; Keybindings for processing
(spacemacs/set-leader-keys
  "o o p i" 'codelahoma-gtd-process-inbox
  "o o p b" 'codelahoma-gtd-bulk-process
  "o o p c" 'codelahoma-gtd-clarify-item
  "o o p s" 'codelahoma-gtd-process-statistics)
```

**Tests:**
- Process sample inbox items
- Test refile suggestions
- Verify processing mode keybindings
- Check bulk operations
- Validate statistics tracking

**Implementation Notes:**
- Created comprehensive processing mode with focused keybindings
- Implemented narrowing to focus on individual items during processing
- Added smart refile suggestions based on content analysis
- Created interactive clarification workflow for GTD decision tree
- Built helper functions for item counting and statistics
- Added processing help system with clear command documentation
- Integrated refile configuration with org-mode's built-in system
- Created test-process.el for verification
- All processing functions available via `SPC o o p` keybindings

**Commit:** 
```
feat(gtd): Implement Phase 2 Inbox Processing Workflow

- Created GTD processing mode with focused keybindings
- Added smart refile suggestions and clarification workflow
- Implemented item navigation with narrowing
- Built processing statistics and help system
- Integrated refile targets configuration
- Created test suite for processing functionality
- Updated keybindings for all processing commands
```

#### Task 2.4: Define Project and Area Structure

**Status:** ✅ COMPLETE

**Purpose:** Establish the organizational structure for projects, areas of responsibility, and their relationships.

**Implementation Checklist:**
- [x] Define project metadata structure
- [x] Create area of responsibility framework
- [x] Implement project-task relationships
- [x] Build project templates
- [x] Add project navigation functions

**Reference Implementation:**
```elisp
;; In codelahoma-gtd-core.el (additions):

(defcustom codelahoma-gtd-project-properties
  '("PROJECT_TYPE" "OUTCOME" "DEADLINE" "STAKEHOLDER" "STATUS")
  "Properties to track for projects."
  :type '(repeat string)
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-areas-of-focus
  '("Personal Development"
    "Health & Fitness"
    "Relationships"
    "Career"
    "Finance"
    "Home & Environment"
    "Hobbies & Recreation"
    "Community & Service")
  "Areas of responsibility for life management."
  :type '(repeat string)
  :group 'codelahoma-gtd)

(defun codelahoma-gtd-new-project ()
  "Create a new GTD project with template."
  (interactive)
  (let* ((title (read-string "Project title: "))
         (area (completing-read "Area of focus: " codelahoma-gtd-areas-of-focus))
         (outcome (read-string "Desired outcome: "))
         (deadline (org-read-date nil nil nil "Deadline (optional): ")))
    (find-file (expand-file-name "projects.org" codelahoma-gtd-directory))
    (goto-char (point-max))
    (insert (format "\n* PROJECT %s :%s:\n" title (codelahoma-gtd-area-to-tag area)))
    (org-set-property "AREA_OF_FOCUS" area)
    (org-set-property "OUTCOME" outcome)
    (when deadline
      (org-deadline nil deadline))
    (org-set-property "PROJECT_CREATED" (format-time-string "[%Y-%m-%d %a]"))
    (org-set-property "STATUS" "Active")
    (insert "\n** Purpose/Outcome\n" outcome "\n\n")
    (insert "** Success Criteria\n- [ ] \n\n")
    (insert "** Next Actions\n*** NEXT \n\n")
    (insert "** Project Notes\n")
    (insert (format "Created: %s\n\n" (format-time-string "%Y-%m-%d")))
    (goto-char (point-min))
    (re-search-forward "\\*\\*\\* NEXT " nil t)
    (message "Project created - add next actions")))

(defun codelahoma-gtd-area-to-tag (area)
  "Convert area name to org tag."
  (upcase (replace-regexp-in-string 
           "[& ]+" "_" 
           (replace-regexp-in-string "[^[:alnum:]& ]" "" area))))

(defun codelahoma-gtd-list-projects (&optional area)
  "List all projects, optionally filtered by area."
  (interactive)
  (let ((org-agenda-files (list (expand-file-name "projects.org" codelahoma-gtd-directory))))
    (org-tags-view nil (concat "PROJECT" (when area (format "+%s" (codelahoma-gtd-area-to-tag area)))))))

(defun codelahoma-gtd-project-status ()
  "Show project status overview."
  (interactive)
  (let ((projects (codelahoma-gtd-get-all-projects))
        (active 0) (stalled 0) (completed 0))
    (dolist (project projects)
      (pcase (plist-get project :status)
        ("Active" (cl-incf active))
        ("Stalled" (cl-incf stalled))
        ("Completed" (cl-incf completed))))
    (message "Projects - Active: %d, Stalled: %d, Completed: %d, Total: %d" 
             active stalled completed (length projects))))

(defun codelahoma-gtd-find-stalled-projects ()
  "Find projects with no NEXT actions."
  (interactive)
  (let ((org-agenda-files (list (expand-file-name "projects.org" codelahoma-gtd-directory))))
    (org-tags-view nil "PROJECT-TODO=\"NEXT\"")))

(defun codelahoma-gtd-archive-completed-projects ()
  "Archive completed projects."
  (interactive)
  (when (y-or-n-p "Archive all completed projects? ")
    (find-file (expand-file-name "projects.org" codelahoma-gtd-directory))
    (org-map-entries
     (lambda ()
       (when (string= (org-get-todo-state) "COMPLETED")
         (org-archive-subtree)
         (setq org-map-continue-from (point))))
     "PROJECT")
    (org-save-all-org-buffers)
    (message "Completed projects archived")))

;; Project templates
(defcustom codelahoma-gtd-project-templates
  '(("software" . ((properties . (("PROJECT_TYPE" . "Software Development")))
                   (sections . ("Purpose/Outcome" "Requirements" "Technical Design" 
                               "Implementation Plan" "Testing Strategy" "Next Actions"))))
    ("learning" . ((properties . (("PROJECT_TYPE" . "Learning")))
                   (sections . ("Learning Objectives" "Resources" "Study Plan" 
                               "Practice Exercises" "Progress Tracking" "Next Actions"))))
    ("home" . ((properties . (("PROJECT_TYPE" . "Home Improvement")))
               (sections . ("Desired Result" "Materials Needed" "Steps" 
                           "Budget" "Timeline" "Next Actions")))))
  "Project templates by type."
  :type 'alist
  :group 'codelahoma-gtd)

(defun codelahoma-gtd-new-project-from-template ()
  "Create a new project from a template."
  (interactive)
  (let* ((template-name (completing-read "Project template: " 
                                        (mapcar #'car codelahoma-gtd-project-templates)))
         (template (alist-get template-name codelahoma-gtd-project-templates nil nil #'string=))
         (title (read-string "Project title: ")))
    (codelahoma-gtd-new-project)
    ;; Apply template properties
    (dolist (prop (alist-get 'properties template))
      (org-set-property (car prop) (cdr prop)))
    ;; Apply template sections
    (goto-char (point-max))
    (dolist (section (alist-get 'sections template))
      (insert (format "\n** %s\n\n" section)))))

;; Keybindings for projects
(spacemacs/set-leader-keys
  "o o j n" 'codelahoma-gtd-new-project
  "o o j t" 'codelahoma-gtd-new-project-from-template
  "o o j l" 'codelahoma-gtd-list-projects
  "o o j s" 'codelahoma-gtd-project-status
  "o o j f" 'codelahoma-gtd-find-stalled-projects
  "o o j a" 'codelahoma-gtd-archive-completed-projects)
```

**Tests:**
- Create projects with different templates
- Verify area tagging works correctly
- Test project listing and filtering
- Check stalled project detection
- Validate archiving functionality

**Implementation Notes:**
- Defined 8 areas of focus for comprehensive life management
- Created project creation workflow with area selection and outcome definition
- Implemented project templates for software, learning, and home improvement
- Added project status tracking (Active, Stalled, Completed)
- Built project navigation functions (list, status overview, find stalled)
- Created automatic area-to-tag conversion for consistent tagging
- Added project archiving for completed projects
- Integrated all project functions under `SPC o o j` keybindings
- Created test-projects.el for verification
- All project management integrated into codelahoma-gtd-core.el

**Commit:** 
```
feat(gtd): Implement Phase 2 Project and Area Structure

- Defined 8 GTD areas of focus for life management
- Created comprehensive project creation workflow
- Added project templates (software, learning, home)
- Implemented project status tracking and overview
- Built project navigation and archiving functions
- Created test suite for project functionality
- Added keybindings under SPC o o j namespace
```

#### Task 2.5: Integration and Testing

**Status:** [ ] TODO

**Purpose:** Integrate all Phase 2 components and ensure smooth workflow between capture, processing, and organization.

**Implementation Checklist:**
- [ ] Update keybinding documentation
- [ ] Create workflow integration tests
- [ ] Build performance benchmarks
- [ ] Add error handling and recovery
- [ ] Create user documentation

**Reference Implementation:**
```elisp
;; In codelahoma-gtd-core.el (additions):

(defun codelahoma-gtd-initialize-phase2 ()
  "Initialize Phase 2 GTD components."
  (codelahoma-gtd-setup-todo-keywords)
  (codelahoma-gtd-setup-capture)
  (codelahoma-gtd-setup-refile-targets)
  (codelahoma-gtd-setup-processing-hooks)
  (message "GTD Phase 2 initialized"))

(defun codelahoma-gtd-setup-refile-targets ()
  "Configure org-refile for GTD workflow."
  (setq org-refile-targets
        (append 
         '((nil :maxlevel . 2))  ; Current buffer
         (mapcar (lambda (target)
                  (cons (expand-file-name (car target) "~/personal/org-files/")
                        (cdr target)))
                codelahoma-gtd-refile-targets)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm))

(defun codelahoma-gtd-setup-processing-hooks ()
  "Set up hooks for processing workflow."
  (add-hook 'org-after-refile-insert-hook 'codelahoma-gtd-after-refile)
  (add-hook 'org-archive-hook 'codelahoma-gtd-after-archive))

(defun codelahoma-gtd-after-refile ()
  "Actions to perform after refiling."
  (org-set-property "REFILED" (format-time-string "[%Y-%m-%d %a %H:%M]"))
  (when (string-match-p "projects\\.org" (buffer-file-name))
    (org-set-property "PROJECT_STATUS" "Active")))

(defun codelahoma-gtd-workflow-status ()
  "Show current GTD system status."
  (interactive)
  (let* ((inbox-count (codelahoma-gtd-count-entries "inbox.org"))
         (next-count (codelahoma-gtd-count-entries "next-actions.org" "NEXT"))
         (waiting-count (codelahoma-gtd-count-entries "waiting-for.org" "WAITING"))
         (project-count (codelahoma-gtd-count-entries "projects.org" "PROJECT"))
         (today-count (length (org-agenda-get-day-entries 
                              (expand-file-name "calendar.org" codelahoma-gtd-directory)
                              (calendar-current-date)))))
    (message "GTD Status - Inbox: %d | Next: %d | Waiting: %d | Projects: %d | Today: %d"
             inbox-count next-count waiting-count project-count today-count)))

(defun codelahoma-gtd-count-entries (file &optional todo-keyword)
  "Count entries in FILE, optionally filtered by TODO-KEYWORD."
  (let ((count 0)
        (path (expand-file-name file codelahoma-gtd-directory)))
    (when (file-exists-p path)
      (with-current-buffer (find-file-noselect path)
        (org-map-entries
         (lambda () (cl-incf count))
         todo-keyword)))
    count))

;; Performance benchmarking
(defun codelahoma-gtd-benchmark-operations ()
  "Benchmark key GTD operations."
  (interactive)
  (let ((results '()))
    ;; Benchmark capture
    (push (cons "Capture" 
                (benchmark-elapse (codelahoma-gtd-capture-inbox)))
          results)
    ;; Benchmark refile
    (push (cons "Refile" 
                (benchmark-elapse 
                 (with-current-buffer (find-file-noselect 
                                      (expand-file-name "inbox.org" codelahoma-gtd-directory))
                   (goto-char (point-min))
                   (org-refile-get-targets))))
          results)
    ;; Benchmark project list
    (push (cons "List Projects" 
                (benchmark-elapse (codelahoma-gtd-list-projects)))
          results)
    ;; Display results
    (with-output-to-temp-buffer "*GTD Benchmarks*"
      (princ "GTD Operation Benchmarks\n")
      (princ "========================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-15s: %.3f seconds\n" (car result) (cdr result)))))))

;; Error recovery
(defun codelahoma-gtd-check-health ()
  "Check GTD system health and fix common issues."
  (interactive)
  (let ((issues '()))
    ;; Check required files exist
    (dolist (file '("inbox.org" "projects.org" "next-actions.org" 
                   "waiting-for.org" "someday.org"))
      (unless (file-exists-p (expand-file-name file codelahoma-gtd-directory))
        (push (format "Missing file: %s" file) issues)))
    ;; Check for corrupted properties
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (while (re-search-forward "^:PROPERTIES:" nil t)
          (unless (re-search-forward "^:END:" nil t)
            (push (format "Unclosed properties drawer in %s" 
                         (file-name-nondirectory file)) 
                  issues)))))
    ;; Report or fix issues
    (if issues
        (progn
          (with-output-to-temp-buffer "*GTD Health Check*"
            (princ "GTD System Issues Found:\n")
            (princ "========================\n\n")
            (dolist (issue issues)
              (princ (format "- %s\n" issue))))
          (when (y-or-n-p "Attempt automatic fixes? ")
            (codelahoma-gtd-auto-fix-issues issues)))
      (message "GTD system health check passed ✓"))))

;; Updated keybindings
(spacemacs/set-leader-keys
  ;; Status and help
  "o o h" 'codelahoma-gtd-help
  "o o ?" 'codelahoma-gtd-workflow-status
  
  ;; Development
  "o o d b" 'codelahoma-gtd-benchmark-operations
  "o o d h" 'codelahoma-gtd-check-health)
```

**Tests:**
- Full workflow test: capture → process → organize
- Performance benchmarks meet targets (<1s capture)
- Error recovery handles common issues
- All keybindings work without conflicts
- Documentation is clear and helpful

## Implementation Process

### Development Workflow

1. **Branch Management:**
   ```bash
   # Ensure on correct branch
   git checkout gtd-system-greenfield
   git pull origin gtd-system-greenfield
   ```

2. **Module Development:**
   - Edit files in `~/.homesick/repos/dotfiles/home/.spacemacs.d/codelahoma-gtd/`
   - Test changes with `SPC o o d r` (reload)
   - Use `SPC o o d v` to validate structure

3. **Testing Protocol:**
   - Unit test each function in isolation
   - Integration test full workflows
   - Performance benchmark key operations
   - User acceptance test with real data

4. **Commit Strategy:**
   ```bash
   # After implementing each task
   git add -A
   git commit -m "feat(gtd): Implement Phase 2 - [specific component]
   
   - [What was implemented]
   - [Key features added]
   - [Any issues resolved]"
   ```

### Testing Checklist

For each task:
- [ ] Function works as designed
- [ ] Error handling is robust
- [ ] Performance meets targets
- [ ] Keybindings are intuitive
- [ ] Documentation is complete

### Integration Points

Phase 2 must integrate with:
1. **Phase 1 Foundation:**
   - Use established directory structure
   - Extend existing keybindings
   - Maintain org-roam compatibility

2. **Future Phases:**
   - Leave hooks for context system (Phase 3)
   - Prepare for review cycles (Phase 4)
   - Consider knowledge bridge needs (Phase 5)

## Rollback Plan

If issues arise:
1. **Immediate Recovery:**
   - Comment out Phase 2 initialization in dotspacemacs.org
   - Revert to Phase 1 functionality
   - Preserve captured data

2. **Data Preservation:**
   - All org files remain valid
   - No destructive operations on existing data
   - Backward compatible file formats

## Success Criteria

Phase 2 is complete when:
- [ ] All task states are implemented and working
- [ ] Capture is fast (<1 second) and intelligent
- [ ] Inbox processing is smooth and efficient
- [ ] Projects are well-organized with templates
- [ ] Integration between components is seamless
- [ ] Performance benchmarks pass all targets
- [ ] No data loss or corruption occurs
- [ ] User can perform full GTD workflow

## Next Phase Preview

Phase 3 will add Context and Personal Workflow:
- Context-based task filtering (@computer, @home, etc.)
- Custom agenda views for daily planning
- Energy level tracking
- Time blocking integration
- Quick access commands

## Development Notes

### Key Design Decisions:
1. **State Management:** Using org-mode's built-in TODO states with GTD-specific keywords
2. **Capture Speed:** Direct file writes instead of database for instant capture
3. **Processing Mode:** Custom minor mode for focused inbox processing
4. **Project Structure:** Hierarchical org headings with consistent properties

### Performance Targets:
- Capture: <1 second from keystroke to saved
- Refile: <2 seconds to show targets
- Process: Handle 50 items in <5 minutes
- Search: Find any item in <3 seconds

### Technical Considerations:
- Maintain compatibility with standard org-mode
- Ensure all operations are idempotent
- Preserve data integrity during operations
- Support incremental adoption

This implementation plan provides a comprehensive guide for building the GTD engine core in Phase 2, establishing the foundation for a powerful personal productivity system.