# Personal GTD-Zettelkasten Phase 3 Implementation Plan

## Overview

This document outlines the detailed implementation plan for Phase 3: Personal Workflow Implementation of the Personal GTD-Zettelkasten Hybrid System. This phase builds personal productivity patterns on top of the GTD engine, implementing contexts, custom agenda views, and workflow optimizations.

## Purpose

This implementation aims to:

1. **Implement Context System** - Create GTD contexts for location and energy-based filtering
2. **Build Custom Agenda Views** - Develop personalized views matching daily patterns
3. **Create Decision Flow Functions** - Implement smart task selection based on context
4. **Add Quick Access Commands** - Enable rapid navigation and status checks
5. **Enhance Auto-Save Features** - Ensure data integrity with smart saving

## Prerequisites

Before starting Phase 3 implementation:

- [x] Phase 2 complete with GTD engine operational
- [x] Task states and capture working smoothly
- [x] Processing workflow tested and functional
- [x] Project structure established
- [ ] Ready to customize for personal patterns

## Implementation Plan

### Phase 3: Personal Workflow Implementation (Week 3-4)

#### Task 3.1: Implement Context System

**Status:** ✅ COMPLETE

**Purpose:** Create a comprehensive context system for GTD that enables filtering tasks by location, energy level, and available tools.

**Implementation Checklist:**
- [x] Define standard GTD contexts (@computer, @home, @office, @errands, @phone, @anywhere)
- [x] Create energy level contexts (@high-energy, @low-energy, @creative)
- [x] Implement context assignment functions
- [x] Build context filtering for agenda views
- [x] Add context-based task suggestions

**Reference Implementation:**
```elisp
;; In codelahoma-gtd-contexts.el (new file):

;;; codelahoma-gtd-contexts.el --- GTD context management -*- lexical-binding: t; -*-

(require 'codelahoma-gtd-config)

;;; Context Definitions

(defcustom codelahoma-gtd-contexts
  '(;; Location contexts
    ("@computer" . (:icon "💻" :color "#4f97d7" :description "At computer with full tools"))
    ("@home" . (:icon "🏠" :color "#2d9574" :description "At home location"))
    ("@office" . (:icon "🏢" :color "#b1951d" :description "At office/work location"))
    ("@errands" . (:icon "🚗" :color "#dc752f" :description "Out and about"))
    ("@phone" . (:icon "📱" :color "#c56ec3" :description "Phone calls"))
    ("@anywhere" . (:icon "🌍" :color "#4f97d7" :description "Can be done anywhere"))
    ;; Energy contexts  
    ("@high-energy" . (:icon "⚡" :color "#f2241f" :description "Requires focus and energy"))
    ("@low-energy" . (:icon "🔋" :color "#86dc2f" :description "Low cognitive load"))
    ("@creative" . (:icon "🎨" :color "#a45bad" :description "Creative thinking needed")))
  "GTD contexts with properties."
  :type 'alist
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-time-contexts
  '(("@morning" . (:start "06:00" :end "12:00"))
    ("@afternoon" . (:start "12:00" :end "18:00"))
    ("@evening" . (:start "18:00" :end "22:00"))
    ("@weekend" . (:days (0 6)))) ; Sunday and Saturday
  "Time-based contexts."
  :type 'alist
  :group 'codelahoma-gtd)

;;; Context Functions

(defun codelahoma-gtd-current-contexts ()
  "Return list of currently active contexts."
  (let ((contexts '())
        (current-time (current-time))
        (current-hour (string-to-number (format-time-string "%H")))
        (current-day (string-to-number (format-time-string "%w"))))
    ;; Always active contexts
    (push "@anywhere" contexts)
    (push "@phone" contexts)
    
    ;; Location contexts (could be enhanced with actual location detection)
    (when (or (getenv "SSH_CLIENT") (getenv "SSH_TTY"))
      (push "@computer" contexts))
    
    ;; Time-based contexts
    (dolist (time-context codelahoma-gtd-time-contexts)
      (let* ((name (car time-context))
             (props (cdr time-context))
             (start-hour (when (plist-get props :start)
                          (string-to-number (substring (plist-get props :start) 0 2))))
             (end-hour (when (plist-get props :end)
                        (string-to-number (substring (plist-get props :end) 0 2))))
             (days (plist-get props :days)))
        (when (or (and start-hour end-hour
                       (>= current-hour start-hour)
                       (< current-hour end-hour))
                  (and days (member current-day days)))
          (push name contexts))))
    
    contexts))

(defun codelahoma-gtd-assign-context ()
  "Assign context to current task."
  (interactive)
  (when (eq major-mode 'org-mode)
    (let* ((contexts (mapcar #'car codelahoma-gtd-contexts))
           (current (org-entry-get nil "CONTEXT"))
           (selected (completing-read 
                     (format "Context%s: " 
                             (if current (format " (current: %s)" current) ""))
                     contexts nil nil nil nil current)))
      (org-set-property "CONTEXT" selected)
      (message "Context set to %s" selected))))

(defun codelahoma-gtd-filter-by-context (context)
  "Filter agenda by CONTEXT."
  (interactive 
   (list (completing-read "Context: " 
                         (mapcar #'car codelahoma-gtd-contexts))))
  (let ((org-agenda-files (list codelahoma-gtd-directory)))
    (org-tags-view nil (format "CONTEXT=\"%s\"" context))))

(defun codelahoma-gtd-suggest-next-task ()
  "Suggest next task based on current context."
  (interactive)
  (let* ((current-contexts (codelahoma-gtd-current-contexts))
         (tasks '()))
    ;; Collect matching tasks
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (let ((context (org-entry-get nil "CONTEXT"))
                 (todo-state (org-get-todo-state)))
             (when (and (member todo-state '("NEXT" "TODO"))
                       (or (not context)
                           (member context current-contexts)))
               (push (list :title (org-get-heading t t t t)
                          :context context
                          :priority (org-get-priority (org-get-heading))
                          :effort (org-entry-get nil "Effort")
                          :file file
                          :point (point))
                     tasks))))
         "TODO|NEXT")))
    
    ;; Sort by priority and context match
    (setq tasks (sort tasks 
                     (lambda (a b)
                       (> (plist-get a :priority)
                          (plist-get b :priority)))))
    
    ;; Present suggestions
    (if tasks
        (codelahoma-gtd-show-task-suggestions tasks)
      (message "No tasks match current contexts"))))

(defun codelahoma-gtd-show-task-suggestions (tasks)
  "Show TASKS suggestions in a buffer."
  (with-output-to-temp-buffer "*GTD Task Suggestions*"
    (princ "Suggested Next Tasks\n")
    (princ "====================\n\n")
    (princ (format "Active contexts: %s\n\n" 
                   (mapconcat #'identity (codelahoma-gtd-current-contexts) ", ")))
    (let ((count 0))
      (dolist (task (seq-take tasks 10))
        (cl-incf count)
        (princ (format "%d. %s\n" count (plist-get task :title)))
        (princ (format "   Context: %s | Priority: %s | Effort: %s\n"
                      (or (plist-get task :context) "none")
                      (char-to-string (plist-get task :priority))
                      (or (plist-get task :effort) "?")))
        (princ (format "   File: %s\n\n" 
                      (file-name-nondirectory (plist-get task :file)))))))
  (switch-to-buffer-other-window "*GTD Task Suggestions*"))

;;; Energy Level Tracking

(defcustom codelahoma-gtd-energy-levels
  '(("high" . (:value 5 :color "#f2241f"))
    ("good" . (:value 4 :color "#4f97d7"))
    ("medium" . (:value 3 :color "#b1951d"))
    ("low" . (:value 2 :color "#2d9574"))
    ("tired" . (:value 1 :color "#9f8766")))
  "Energy level definitions."
  :type 'alist
  :group 'codelahoma-gtd)

(defvar codelahoma-gtd-current-energy "medium"
  "Current energy level.")

(defun codelahoma-gtd-set-energy-level ()
  "Set current energy level."
  (interactive)
  (let ((level (completing-read "Energy level: " 
                               (mapcar #'car codelahoma-gtd-energy-levels)
                               nil t nil nil codelahoma-gtd-current-energy)))
    (setq codelahoma-gtd-current-energy level)
    (message "Energy level set to: %s" level)))

(defun codelahoma-gtd-match-task-to-energy ()
  "Match tasks to current energy level."
  (let ((energy-value (plist-get (alist-get codelahoma-gtd-current-energy 
                                           codelahoma-gtd-energy-levels 
                                           nil nil #'string=)
                                :value)))
    (cond
     ((>= energy-value 4) "@high-energy")
     ((<= energy-value 2) "@low-energy")
     (t "@anywhere"))))

(provide 'codelahoma-gtd-contexts)
;;; codelahoma-gtd-contexts.el ends here
```

**Tests:**
- Verify all contexts are defined properly
- Test context assignment to tasks
- Check context filtering works
- Validate time-based context detection
- Test task suggestions based on context

**Implementation Notes:**
- Created codelahoma-gtd-contexts.el with complete context system
- Implemented 9 contexts: 6 location-based and 3 energy-based
- Added time-based context detection (morning, afternoon, evening, weekend)
- Created energy level tracking with 5 levels (tired to high)
- Built context assignment and filtering functions
- Added smart task suggestions based on current contexts
- Integrated keybindings under `SPC o o x` namespace
- Created test-contexts.el for verification
- Context indicator ready for mode line display

**Commit:**
```
feat(gtd): Implement Phase 3 Context System

- Added comprehensive GTD context definitions
- Created location and energy-based contexts
- Implemented time-based context detection
- Built context assignment and filtering
- Added smart task suggestions by context
- Created energy level tracking system
- Added keybindings under SPC o o x
```

#### Task 3.2: Build Custom Agenda Views

**Status:** ✅ COMPLETE

**Purpose:** Create personalized agenda views that match daily workflow patterns and provide quick access to relevant information.

**Implementation Checklist:**
- [x] Design daily dashboard view
- [x] Create focused work session view
- [x] Build review preparation views
- [x] Implement context-aware agendas
- [x] Add quick navigation between views

**Reference Implementation:**
```elisp
;; In codelahoma-gtd-agenda.el (new file):

;;; codelahoma-gtd-agenda.el --- Custom GTD agenda views -*- lexical-binding: t; -*-

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-contexts)

;;; Custom Agenda Commands

(defcustom codelahoma-gtd-agenda-files nil
  "Files to include in GTD agenda views.
If nil, uses all files in GTD directory."
  :type '(repeat file)
  :group 'codelahoma-gtd)

(defun codelahoma-gtd-agenda-files ()
  "Get list of agenda files for GTD."
  (or codelahoma-gtd-agenda-files
      (directory-files codelahoma-gtd-directory t "\\.org$")))

;;; Daily Dashboard

(defun codelahoma-gtd-daily-dashboard ()
  "Show comprehensive daily dashboard."
  (interactive)
  (let ((org-agenda-custom-commands
         '(("d" "Daily Dashboard"
            ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-start-day "today")
                        (org-deadline-warning-days 7)))
             (todo "NEXT" ((org-agenda-overriding-header "Next Actions")
                          (org-agenda-sorting-strategy '(priority-down effort-up))))
             (todo "WAITING" ((org-agenda-overriding-header "Waiting For")))
             (tags "INBOX" ((org-agenda-overriding-header "Inbox Items"))))))))
    (org-agenda nil "d")))

;;; Focused Work Session

(defun codelahoma-gtd-focus-session ()
  "Start a focused work session with relevant tasks."
  (interactive)
  (let* ((duration (read-string "Session duration (minutes): " "25"))
         (context (completing-read "Focus context: " 
                                  (mapcar #'car codelahoma-gtd-contexts)))
         (energy (codelahoma-gtd-current-energy)))
    ;; Set up focused view
    (delete-other-windows)
    (split-window-horizontally)
    ;; Left: Filtered task list
    (codelahoma-gtd-filter-by-context context)
    ;; Right: Timer and notes
    (other-window 1)
    (switch-to-buffer (get-buffer-create "*Focus Session*"))
    (erase-buffer)
    (insert (format "Focus Session: %s minutes\n" duration))
    (insert (format "Context: %s | Energy: %s\n" context energy))
    (insert "=====================================\n\n")
    (insert "Session Notes:\n\n")
    ;; Start timer
    (run-with-timer (* 60 (string-to-number duration)) nil
                   'codelahoma-gtd-end-focus-session)))

(defun codelahoma-gtd-end-focus-session ()
  "End focus session and prompt for review."
  (message "Focus session complete!")
  (when (y-or-n-p "Review completed tasks? ")
    (codelahoma-gtd-review-completed-today)))

;;; Context-Aware Views

(defun codelahoma-gtd-agenda-by-context ()
  "Show agenda grouped by context."
  (interactive)
  (let ((org-agenda-custom-commands
         '(("c" "By Context"
            ((tags-todo "@computer"
                        ((org-agenda-overriding-header "Computer Tasks")))
             (tags-todo "@home"
                        ((org-agenda-overriding-header "Home Tasks")))
             (tags-todo "@errands"
                        ((org-agenda-overriding-header "Errands")))
             (tags-todo "@phone"
                        ((org-agenda-overriding-header "Phone Calls"))))))))
    (org-agenda nil "c")))

(defun codelahoma-gtd-agenda-by-energy ()
  "Show tasks organized by energy level required."
  (interactive)
  (let ((org-agenda-custom-commands
         '(("e" "By Energy"
            ((tags-todo "@high-energy"
                        ((org-agenda-overriding-header "High Energy Tasks")))
             (tags-todo "@low-energy"
                        ((org-agenda-overriding-header "Low Energy Tasks")))
             (tags-todo "-@high-energy-@low-energy"
                        ((org-agenda-overriding-header "Medium Energy Tasks"))))))))
    (org-agenda nil "e")))

;;; Quick Views

(defun codelahoma-gtd-quick-inbox ()
  "Quick view of inbox items."
  (interactive)
  (find-file (expand-file-name "inbox.org" codelahoma-gtd-directory))
  (goto-char (point-min))
  (org-overview)
  (org-content 2))

(defun codelahoma-gtd-quick-projects ()
  "Quick project overview."
  (interactive)
  (find-file (expand-file-name "projects.org" codelahoma-gtd-directory))
  (goto-char (point-min))
  (org-overview)
  (org-content 2)
  (message "Projects: %d active, %d total" 
           (count-matches "^\\* PROJECT")
           (count-matches "^\\* \\(PROJECT\\|COMPLETED\\)")))

(defun codelahoma-gtd-quick-waiting ()
  "Quick view of waiting items."
  (interactive)
  (let ((org-agenda-files (codelahoma-gtd-agenda-files)))
    (org-tags-view nil "TODO=\"WAITING\"")))

;;; Planning Views

(defun codelahoma-gtd-weekly-planning ()
  "Weekly planning view."
  (interactive)
  (let ((org-agenda-custom-commands
         '(("w" "Weekly Planning"
            ((agenda "" ((org-agenda-span 'week)
                        (org-agenda-start-on-weekday 1)))
             (todo "PROJECT" ((org-agenda-overriding-header "Active Projects")))
             (tags "DEADLINE<=\"<+7d>\""
                   ((org-agenda-overriding-header "Upcoming Deadlines"))))))))
    (org-agenda nil "w")))

(defun codelahoma-gtd-someday-review ()
  "Review someday/maybe items."
  (interactive)
  (find-file (expand-file-name "someday.org" codelahoma-gtd-directory))
  (goto-char (point-min))
  (org-overview)
  (org-content 2)
  (when (y-or-n-p "Activate any someday items? ")
    (org-map-entries
     (lambda ()
       (when (y-or-n-p (format "Activate: %s? " 
                              (org-get-heading t t t t)))
         (org-todo "TODO")
         (org-refile nil nil 
                    (list nil 
                          (expand-file-name "inbox.org" codelahoma-gtd-directory)
                          nil nil))))
     "TODO")))

;;; Agenda Configuration

(defun codelahoma-gtd-configure-agenda ()
  "Configure org-agenda for GTD workflow."
  (setq org-agenda-files (codelahoma-gtd-agenda-files))
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-compact-blocks t)
  
  ;; Custom agenda prefix
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))
  
  ;; Agenda sorting
  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up priority-down category-keep)
          (todo priority-down effort-up category-keep)
          (tags priority-down category-keep)
          (search category-keep))))

(provide 'codelahoma-gtd-agenda)
;;; codelahoma-gtd-agenda.el ends here
```

**Tests:**
- Test daily dashboard displays correctly
- Verify context-based filtering in agendas
- Check focused work session functionality
- Validate planning views show relevant data
- Test quick navigation commands

**Implementation Notes:**
- Created codelahoma-gtd-agenda.el with comprehensive custom views
- Implemented daily dashboard showing agenda, next actions, waiting, and inbox
- Built focused work session with timer and context filtering
- Added context-aware and energy-based agenda views
- Created morning review routine with split-window checklist
- Added weekly planning and someday review functions
- Configured org-agenda with GTD-specific settings
- Integrated all views with keybindings under `SPC o o a`
- Created test-agenda.el for verification

**Commit:**
```
feat(gtd): Implement Phase 3 Custom Agenda Views

- Created comprehensive daily dashboard view
- Built focused work session with timer support
- Added context and energy-based agenda filtering
- Implemented morning review routine
- Created weekly planning and someday review
- Configured org-agenda for GTD workflow
- Added keybindings under SPC o o a
```

#### Task 3.3: Implement Decision Flow Functions

**Status:** ✅ COMPLETE

**Purpose:** Create intelligent decision support for choosing what to work on next based on context, energy, and priorities.

**Implementation Checklist:**
- [x] Build smart task selection algorithm
- [x] Create decision tree for processing
- [x] Implement priority balancing
- [x] Add time estimation helpers
- [x] Create "what's next" suggestions

**Reference Implementation:**
```elisp
;; Additions to codelahoma-gtd-core.el:

;;; Decision Support Functions

(defun codelahoma-gtd-smart-next-action ()
  "Intelligently suggest the next action based on multiple factors."
  (interactive)
  (let* ((current-contexts (codelahoma-gtd-current-contexts))
         (current-energy (codelahoma-gtd-current-energy))
         (available-time (codelahoma-gtd-estimate-available-time))
         (tasks (codelahoma-gtd-score-all-tasks current-contexts 
                                               current-energy 
                                               available-time)))
    (if tasks
        (codelahoma-gtd-present-smart-suggestions tasks)
      (message "No suitable tasks found for current context"))))

(defun codelahoma-gtd-score-all-tasks (contexts energy time)
  "Score all available tasks based on CONTEXTS, ENERGY, and TIME."
  (let ((scored-tasks '()))
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (when (member (org-get-todo-state) '("NEXT" "TODO"))
             (let ((score (codelahoma-gtd-calculate-task-score 
                          contexts energy time)))
               (when (> score 0)
                 (push (list :title (org-get-heading t t t t)
                           :score score
                           :context (org-entry-get nil "CONTEXT")
                           :effort (org-entry-get nil "Effort")
                           :priority (org-get-priority (org-get-heading))
                           :deadline (org-get-deadline-time nil)
                           :file file
                           :point (point))
                       scored-tasks)))))
         "TODO|NEXT")))
    ;; Sort by score
    (sort scored-tasks (lambda (a b) (> (plist-get a :score) 
                                       (plist-get b :score))))))

(defun codelahoma-gtd-calculate-task-score (contexts energy time)
  "Calculate task score based on CONTEXTS, ENERGY, and TIME."
  (let ((score 0)
        (task-context (org-entry-get nil "CONTEXT"))
        (task-effort (org-entry-get nil "Effort"))
        (task-energy (org-entry-get nil "ENERGY_REQUIRED"))
        (deadline (org-get-deadline-time nil))
        (priority (org-get-priority (org-get-heading))))
    
    ;; Context match (40 points max)
    (when (or (not task-context)
              (member task-context contexts))
      (cl-incf score 40))
    
    ;; Priority score (30 points max)
    (cl-incf score (/ (* (- priority ?A) 30) 3))
    
    ;; Deadline urgency (20 points max)
    (when deadline
      (let ((days-until (/ (- deadline (float-time)) 86400)))
        (cond
         ((< days-until 1) (cl-incf score 20))
         ((< days-until 3) (cl-incf score 15))
         ((< days-until 7) (cl-incf score 10))
         ((< days-until 14) (cl-incf score 5)))))
    
    ;; Energy match (10 points max)
    (when (or (not task-energy)
              (codelahoma-gtd-energy-match-p energy task-energy))
      (cl-incf score 10))
    
    ;; Time fit (penalty for tasks too long)
    (when (and task-effort time)
      (let ((effort-minutes (codelahoma-gtd-effort-to-minutes task-effort)))
        (when (> effort-minutes time)
          (cl-decf score 20))))
    
    score))

(defun codelahoma-gtd-estimate-available-time ()
  "Estimate available time until next appointment."
  (let* ((now (current-time))
         (next-appt (codelahoma-gtd-next-appointment-time)))
    (if next-appt
        (/ (- next-appt (float-time now)) 60) ; minutes
      120))) ; Default 2 hours if no appointments

(defun codelahoma-gtd-effort-to-minutes (effort)
  "Convert EFFORT string to minutes."
  (when effort
    (cond
     ((string-match "\\([0-9]+\\)h" effort)
      (* 60 (string-to-number (match-string 1 effort))))
     ((string-match "\\([0-9]+\\)m" effort)
      (string-to-number (match-string 1 effort)))
     ((string-match "\\([0-9]+\\)d" effort)
      (* 480 (string-to-number (match-string 1 effort)))) ; 8 hours per day
     (t 30)))) ; Default 30 minutes

(defun codelahoma-gtd-present-smart-suggestions (tasks)
  "Present TASKS as smart suggestions."
  (let ((buffer (get-buffer-create "*GTD Smart Suggestions*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert "Smart Task Suggestions\n")
      (insert "======================\n\n")
      (insert (format "Context: %s | Energy: %s | Available time: %d min\n\n"
                     (mapconcat #'identity (codelahoma-gtd-current-contexts) ", ")
                     codelahoma-gtd-current-energy
                     (codelahoma-gtd-estimate-available-time)))
      
      (let ((count 0))
        (dolist (task (seq-take tasks 5))
          (cl-incf count)
          (let ((start (point)))
            (insert (format "%d. %s (Score: %d)\n"
                           count
                           (plist-get task :title)
                           (plist-get task :score)))
            (insert (format "   %s | Priority %s | Effort: %s\n"
                           (or (plist-get task :context) "Any context")
                           (char-to-string (plist-get task :priority))
                           (or (plist-get task :effort) "?")))
            (when (plist-get task :deadline)
              (insert (format "   Deadline: %s\n"
                             (format-time-string "%Y-%m-%d" 
                                               (plist-get task :deadline)))))
            (insert "\n")
            ;; Make clickable
            (put-text-property start (1- (point)) 'task-data task)
            (put-text-property start (1- (point)) 'keymap 
                              (let ((map (make-sparse-keymap)))
                                (define-key map [mouse-1] 'codelahoma-gtd-open-task)
                                (define-key map (kbd "RET") 'codelahoma-gtd-open-task)
                                map)))))
      (goto-char (point-min))
      (read-only-mode 1))
    (display-buffer buffer)))

(defun codelahoma-gtd-open-task ()
  "Open task at point."
  (interactive)
  (let ((task-data (get-text-property (point) 'task-data)))
    (when task-data
      (find-file (plist-get task-data :file))
      (goto-char (plist-get task-data :point))
      (org-show-entry))))

;;; Decision Tree Processing

(defun codelahoma-gtd-decision-tree ()
  "Interactive decision tree for processing."
  (interactive)
  (let ((item (org-get-heading t t t t)))
    (message "Processing: %s" item)
    
    ;; Is it actionable?
    (if (y-or-n-p "Is this actionable? ")
        (progn
          ;; Yes - determine action type
          (if (y-or-n-p "Will it take less than 2 minutes? ")
              (progn
                (message "Do it now!")
                (org-todo "ACTIVE"))
            ;; More than 2 minutes
            (if (y-or-n-p "Is this a project (multiple steps)? ")
                (codelahoma-gtd-convert-to-project)
              ;; Single action
              (progn
                (org-todo "TODO")
                (codelahoma-gtd-assign-context)
                (when (y-or-n-p "Can you delegate this? ")
                  (codelahoma-gtd-delegate-task))
                (when (y-or-n-p "Schedule for specific time? ")
                  (org-schedule nil))))))
      ;; Not actionable
      (if (y-or-n-p "Is this reference material? ")
          (progn
            (org-set-tags ":REFERENCE:")
            (org-refile nil nil (list nil 
                                    (expand-file-name "reference.org" 
                                                     codelahoma-gtd-directory)
                                    nil nil)))
        ;; Not reference
        (if (y-or-n-p "Might this be useful someday? ")
            (progn
              (org-set-tags ":SOMEDAY:")
              (org-refile nil nil (list nil
                                      (expand-file-name "someday.org"
                                                       codelahoma-gtd-directory)
                                      nil nil)))
          ;; Trash it
          (when (y-or-n-p "Delete this item? ")
            (org-cut-subtree)))))))

(provide 'codelahoma-gtd-decision)
```

**Tests:**
- Test task scoring algorithm
- Verify smart suggestions are relevant
- Check decision tree flow
- Validate effort estimation
- Test context-energy matching

**Implementation Notes:**
- Added comprehensive decision support to codelahoma-gtd-core.el
- Implemented smart task scoring based on context, energy, time, priority, and deadlines
- Created interactive decision tree following GTD methodology
- Built clickable task suggestion interface with scores
- Added effort-to-minutes conversion supporting hours, minutes, and days
- Implemented overdue task counting for status display
- Integrated with existing context and energy systems
- Added keybindings `SPC o o p d` (decision tree) and `SPC o o p n` (smart next)
- Created test-decision.el for verification

**Commit:**
```
feat(gtd): Implement Phase 3 Decision Flow Functions

- Built smart task scoring algorithm
- Created interactive GTD decision tree
- Added priority and deadline balancing
- Implemented effort time estimation
- Created clickable suggestion interface
- Added overdue task tracking
- Integrated with context system
```

#### Task 3.4: Add Quick Access Commands

**Status:** ✅ COMPLETE

**Purpose:** Create rapid navigation and status commands for efficient GTD workflow management.

**Implementation Checklist:**
- [x] Implement quick status checks
- [x] Create instant navigation commands
- [x] Build quick capture variants
- [x] Add rapid context switching
- [x] Create workflow shortcuts

**Reference Implementation:**
```elisp
;; Additions to codelahoma-ui.el:

;;; Quick Access Commands

(defun codelahoma-gtd-quick-status ()
  "Quick GTD status in minibuffer."
  (interactive)
  (let* ((inbox (codelahoma-gtd-count-entries "inbox.org"))
         (next (codelahoma-gtd-count-entries "next-actions.org" "NEXT"))
         (waiting (codelahoma-gtd-count-entries "waiting-for.org" "WAITING"))
         (today-scheduled (length (org-agenda-get-scheduled)))
         (overdue (codelahoma-gtd-count-overdue)))
    (message "📥 %d | ⚡ %d | ⏳ %d | 📅 %d | ⚠️  %d" 
             inbox next waiting today-scheduled overdue)))

(defun codelahoma-gtd-quick-capture-task ()
  "Ultra-quick task capture."
  (interactive)
  (let ((task (read-string "Task: ")))
    (with-current-buffer (find-file-noselect 
                         (expand-file-name "inbox.org" codelahoma-gtd-directory))
      (goto-char (point-max))
      (insert (format "\n* TODO %s\n:PROPERTIES:\n:CAPTURED: %s\n:END:\n"
                     task (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (save-buffer))
    (message "Captured: %s" task)))

(defun codelahoma-gtd-quick-note ()
  "Quick note capture."
  (interactive)
  (let ((note (read-string "Note: ")))
    (with-current-buffer (find-file-noselect
                         (expand-file-name "inbox.org" codelahoma-gtd-directory))
      (goto-char (point-max))
      (insert (format "\n* %s :NOTE:\n:PROPERTIES:\n:CAPTURED: %s\n:END:\n"
                     note (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (save-buffer))
    (message "Note captured")))

;;; Quick Navigation

(defhydra codelahoma-gtd-quick-nav (:color blue :hint nil)
  "
GTD Quick Navigation
--------------------
_i_: Inbox      _p_: Projects   _n_: Next Actions
_w_: Waiting    _s_: Someday    _c_: Calendar
_a_: Areas      _r_: Reference  _d_: Daily Review

_q_: Quit
"
  ("i" (find-file (expand-file-name "inbox.org" codelahoma-gtd-directory)))
  ("p" (find-file (expand-file-name "projects.org" codelahoma-gtd-directory)))
  ("n" (find-file (expand-file-name "next-actions.org" codelahoma-gtd-directory)))
  ("w" (find-file (expand-file-name "waiting-for.org" codelahoma-gtd-directory)))
  ("s" (find-file (expand-file-name "someday.org" codelahoma-gtd-directory)))
  ("c" (find-file (expand-file-name "calendar.org" codelahoma-gtd-directory)))
  ("a" (dired (expand-file-name "areas" "~/personal/org-files/")))
  ("r" (dired (expand-file-name "resources" "~/personal/org-files/")))
  ("d" codelahoma-gtd-daily-review)
  ("q" nil))

;;; Context Quick Switch

(defhydra codelahoma-gtd-context-switch (:color blue :hint nil)
  "
Switch Context
--------------
_c_: @computer   _h_: @home      _o_: @office
_e_: @errands    _p_: @phone     _a_: @anywhere

Energy: _1_: Low  _2_: Medium  _3_: High

_q_: Quit
"
  ("c" (codelahoma-gtd-set-context "@computer"))
  ("h" (codelahoma-gtd-set-context "@home"))
  ("o" (codelahoma-gtd-set-context "@office"))
  ("e" (codelahoma-gtd-set-context "@errands"))
  ("p" (codelahoma-gtd-set-context "@phone"))
  ("a" (codelahoma-gtd-set-context "@anywhere"))
  ("1" (setq codelahoma-gtd-current-energy "low"))
  ("2" (setq codelahoma-gtd-current-energy "medium"))
  ("3" (setq codelahoma-gtd-current-energy "high"))
  ("q" nil))

(defun codelahoma-gtd-set-context (context)
  "Set current context to CONTEXT."
  (setq codelahoma-gtd-current-context context)
  (message "Context set to %s" context))

;;; Quick Process

(defun codelahoma-gtd-quick-process ()
  "Quick process current item."
  (interactive)
  (when (eq major-mode 'org-mode)
    (let ((decision (read-char-choice 
                    "Process: [n]ext [t]odo [w]aiting [d]elegate [p]roject [s]omeday [r]eference [x]delete: "
                    '(?n ?t ?w ?d ?p ?s ?r ?x))))
      (pcase decision
        (?n (org-todo "NEXT"))
        (?t (org-todo "TODO"))
        (?w (org-todo "WAITING"))
        (?d (codelahoma-gtd-delegate-task))
        (?p (codelahoma-gtd-convert-to-project))
        (?s (progn (org-todo "TODO")
                   (org-set-tags ":SOMEDAY:")
                   (org-refile nil nil (list nil
                                           (expand-file-name "someday.org"
                                                           codelahoma-gtd-directory)
                                           nil nil))))
        (?r (progn (org-todo nil)
                   (org-set-tags ":REFERENCE:")))
        (?x (when (y-or-n-p "Really delete? ")
              (org-cut-subtree)))))))

;;; Quick Review

(defun codelahoma-gtd-quick-daily-review ()
  "Quick daily review checklist."
  (interactive)
  (with-output-to-temp-buffer "*Quick Daily Review*"
    (princ "Daily Review Checklist\n")
    (princ "======================\n\n")
    (princ "[ ] Process inbox to zero\n")
    (princ "[ ] Review calendar for tomorrow\n")
    (princ "[ ] Review NEXT actions\n")
    (princ "[ ] Check WAITING items\n")
    (princ "[ ] Update project status\n")
    (princ "[ ] Plan tomorrow's priorities\n\n")
    (princ "Press 'q' to close this buffer"))
  (switch-to-buffer-other-window "*Quick Daily Review*"))

;;; Quick Keybinding Setup

(defun codelahoma-gtd-setup-quick-keys ()
  "Set up quick access keybindings."
  ;; Global quick keys (outside of SPC o o namespace)
  (global-set-key (kbd "C-c c") 'codelahoma-gtd-quick-capture-task)
  (global-set-key (kbd "C-c n") 'codelahoma-gtd-quick-note)
  (global-set-key (kbd "C-c s") 'codelahoma-gtd-quick-status)
  
  ;; Quick access under SPC o o q
  (spacemacs/declare-prefix "o o q" "quick")
  (spacemacs/set-leader-keys
    "o o q q" 'codelahoma-gtd-quick-nav/body
    "o o q s" 'codelahoma-gtd-quick-status
    "o o q c" 'codelahoma-gtd-context-switch/body
    "o o q p" 'codelahoma-gtd-quick-process
    "o o q n" 'codelahoma-gtd-smart-next-action
    "o o q r" 'codelahoma-gtd-quick-daily-review))
```

**Tests:**
- Test quick capture speed
- Verify navigation hydra works
- Check context switching
- Validate quick process commands
- Test status display accuracy

**Implementation Notes:**
- Created codelahoma-gtd-quick.el with comprehensive quick access commands
- Implemented quick status showing inbox, next actions, waiting, projects, and overdue counts
- Built hydra-based quick navigation menu for rapid file access
- Added ultra-quick capture variants for tasks, notes, waiting items, and calendar events
- Created quick workflow commands like complete-and-next, delegate, defer
- Implemented jump commands for direct project and context navigation
- Added quick reviews for inbox count and stalled projects
- Created quick dashboard showing comprehensive GTD status
- Integrated all commands under `SPC o o q` keybinding namespace
- Created test-quick.el for verification

**Commit:**
```
feat(gtd): Implement Phase 3 Quick Access Commands

- Created comprehensive quick status displays
- Built hydra-based navigation menu
- Added ultra-quick capture variants
- Implemented workflow shortcuts
- Created jump-to navigation commands
- Added quick review functions
- Built minibuffer dashboard
- Integrated under SPC o o q namespace
```

#### Task 3.5: Enhance Auto-Save and Data Integrity

**Status:** ✅ COMPLETE

**Purpose:** Implement robust auto-save mechanisms and data integrity checks to prevent any loss of captured thoughts or task updates.

**Implementation Checklist:**
- [x] Enhance auto-save functionality
- [x] Add backup mechanisms
- [x] Implement data validation
- [x] Create recovery functions
- [x] Add sync status indicators

**Reference Implementation:**
```elisp
;; In codelahoma-gtd-autosave.el (new file):

;;; codelahoma-gtd-autosave.el --- Auto-save and data integrity -*- lexical-binding: t; -*-

(require 'codelahoma-gtd-config)

;;; Auto-save Configuration

(defcustom codelahoma-gtd-auto-save-interval 60
  "Interval in seconds between auto-saves."
  :type 'integer
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-backup-directory
  (expand-file-name "backups" codelahoma-gtd-directory)
  "Directory for GTD backups."
  :type 'directory
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-max-backups 10
  "Maximum number of backups to keep per file."
  :type 'integer
  :group 'codelahoma-gtd)

;;; Auto-save Functions

(defvar codelahoma-gtd-auto-save-timer nil
  "Timer for auto-saving GTD files.")

(defvar codelahoma-gtd-files-changed nil
  "List of GTD files that have been changed.")

(defun codelahoma-gtd-enable-auto-save ()
  "Enable enhanced auto-save for GTD files."
  (interactive)
  ;; Ensure backup directory exists
  (unless (file-exists-p codelahoma-gtd-backup-directory)
    (make-directory codelahoma-gtd-backup-directory t))
  
  ;; Set up auto-save timer
  (when codelahoma-gtd-auto-save-timer
    (cancel-timer codelahoma-gtd-auto-save-timer))
  
  (setq codelahoma-gtd-auto-save-timer
        (run-with-timer codelahoma-gtd-auto-save-interval
                       codelahoma-gtd-auto-save-interval
                       'codelahoma-gtd-auto-save-changed))
  
  ;; Hook into buffer modifications
  (add-hook 'after-change-functions 'codelahoma-gtd-mark-changed)
  (add-hook 'before-save-hook 'codelahoma-gtd-create-backup)
  
  (message "GTD auto-save enabled (every %d seconds)" 
           codelahoma-gtd-auto-save-interval))

(defun codelahoma-gtd-mark-changed (&rest _)
  "Mark current buffer as changed if it's a GTD file."
  (when (and (buffer-file-name)
             (string-prefix-p (expand-file-name codelahoma-gtd-directory)
                             (buffer-file-name)))
    (add-to-list 'codelahoma-gtd-files-changed (buffer-file-name))))

(defun codelahoma-gtd-auto-save-changed ()
  "Auto-save all changed GTD files."
  (let ((saved-count 0))
    (dolist (file codelahoma-gtd-files-changed)
      (when (get-file-buffer file)
        (with-current-buffer (get-file-buffer file)
          (when (buffer-modified-p)
            (save-buffer)
            (cl-incf saved-count)))))
    (when (> saved-count 0)
      (message "Auto-saved %d GTD file(s)" saved-count))
    (setq codelahoma-gtd-files-changed nil)))

;;; Backup Functions

(defun codelahoma-gtd-create-backup ()
  "Create backup of current GTD file before saving."
  (when (and (buffer-file-name)
             (string-prefix-p (expand-file-name codelahoma-gtd-directory)
                             (buffer-file-name)))
    (let* ((file (buffer-file-name))
           (backup-name (codelahoma-gtd-backup-filename file)))
      (when (file-exists-p file)
        (copy-file file backup-name t)
        (codelahoma-gtd-cleanup-old-backups file)))))

(defun codelahoma-gtd-backup-filename (file)
  "Generate backup filename for FILE."
  (let ((base (file-name-nondirectory file)))
    (expand-file-name 
     (format "%s.%s~" base (format-time-string "%Y%m%d-%H%M%S"))
     codelahoma-gtd-backup-directory)))

(defun codelahoma-gtd-cleanup-old-backups (file)
  "Remove old backups of FILE, keeping only recent ones."
  (let* ((base (file-name-nondirectory file))
         (pattern (format "^%s\\..*~$" (regexp-quote base)))
         (backups (directory-files codelahoma-gtd-backup-directory t pattern))
         (sorted (sort backups #'string<)))
    (when (> (length sorted) codelahoma-gtd-max-backups)
      (dolist (old-backup (butlast sorted codelahoma-gtd-max-backups))
        (delete-file old-backup)))))

;;; Data Integrity

(defun codelahoma-gtd-validate-files ()
  "Validate all GTD files for data integrity."
  (interactive)
  (let ((issues '()))
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          ;; Check for corrupted properties
          (goto-char (point-min))
          (while (re-search-forward "^:PROPERTIES:" nil t)
            (let ((start (point)))
              (unless (re-search-forward "^:END:" nil t)
                (push (format "%s: Unclosed properties at line %d"
                             (file-name-nondirectory file)
                             (line-number-at-pos start))
                      issues))))
          
          ;; Check for broken timestamps
          (goto-char (point-min))
          (while (re-search-forward "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" nil t)
            (let ((date-string (match-string 1)))
              (condition-case nil
                  (encode-time 0 0 0 
                              (string-to-number (substring date-string 8 10))
                              (string-to-number (substring date-string 5 7))
                              (string-to-number (substring date-string 0 4)))
                (error
                 (push (format "%s: Invalid date %s at line %d"
                              (file-name-nondirectory file)
                              date-string
                              (line-number-at-pos))
                       issues))))))))
    
    (if issues
        (codelahoma-gtd-show-validation-issues issues)
      (message "All GTD files validated successfully ✓"))))

(defun codelahoma-gtd-show-validation-issues (issues)
  "Show validation ISSUES in a buffer."
  (with-output-to-temp-buffer "*GTD Validation Issues*"
    (princ "GTD File Validation Issues\n")
    (princ "==========================\n\n")
    (dolist (issue issues)
      (princ (format "- %s\n" issue)))
    (princ "\n")
    (princ "Run M-x codelahoma-gtd-fix-issues to attempt automatic fixes"))
  (switch-to-buffer-other-window "*GTD Validation Issues*"))

;;; Recovery Functions

(defun codelahoma-gtd-recover-file ()
  "Recover a GTD file from backup."
  (interactive)
  (let* ((file (completing-read "File to recover: "
                               (mapcar #'file-name-nondirectory
                                      (directory-files codelahoma-gtd-directory t "\\.org$"))))
         (backups (codelahoma-gtd-list-backups file)))
    (if backups
        (let ((backup (completing-read "Backup to restore: " backups)))
          (when (y-or-n-p (format "Replace %s with backup from %s? " file backup))
            (copy-file (expand-file-name backup codelahoma-gtd-backup-directory)
                      (expand-file-name file codelahoma-gtd-directory)
                      t)
            (message "File recovered from backup")))
      (message "No backups found for %s" file))))

(defun codelahoma-gtd-list-backups (file)
  "List available backups for FILE."
  (let ((pattern (format "^%s\\..*~$" (regexp-quote file))))
    (mapcar #'file-name-nondirectory
            (directory-files codelahoma-gtd-backup-directory t pattern))))

;;; Sync Status

(defvar codelahoma-gtd-sync-status 'idle
  "Current sync status: idle, saving, or error.")

(defun codelahoma-gtd-sync-indicator ()
  "Return sync status indicator for mode line."
  (pcase codelahoma-gtd-sync-status
    ('idle "")
    ('saving " 💾")
    ('error " ⚠️")))

(defun codelahoma-gtd-update-sync-status (status)
  "Update sync STATUS."
  (setq codelahoma-gtd-sync-status status)
  (force-mode-line-update))

;; Add to mode line
(add-to-list 'mode-line-format '(:eval (codelahoma-gtd-sync-indicator)) t)

;;; Emergency Save

(defun codelahoma-gtd-emergency-save-all ()
  "Emergency save all GTD buffers."
  (interactive)
  (let ((saved 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (buffer-file-name)
                   (string-prefix-p (expand-file-name codelahoma-gtd-directory)
                                   (buffer-file-name))
                   (buffer-modified-p))
          (save-buffer)
          (cl-incf saved))))
    (message "Emergency saved %d GTD file(s)" saved)))

(provide 'codelahoma-gtd-autosave)
;;; codelahoma-gtd-autosave.el ends here
```

**Tests:**
- Test auto-save triggers correctly
- Verify backup creation and cleanup
- Check data validation catches issues
- Test file recovery process
- Validate sync status indicators

**Implementation Notes:**
- Created codelahoma-gtd-autosave.el with comprehensive auto-save and backup functionality
- Implemented configurable auto-save with 60-second default interval
- Built automatic backup system with versioning and rotation (keeps last 10 backups)
- Added data integrity validation checking for org structure, properties, and timestamps
- Created recovery functions for restoring from backups
- Implemented sync status indicators for mode line
- Added emergency backup functionality for critical situations
- Integrated save-on-kill and save-on-exit hooks
- Created buffer-specific auto-save enablement for GTD files
- Added comprehensive status monitoring and reporting
- Integrated all functions under `SPC o o b` keybinding namespace
- Created test-autosave.el for verification

**Commit:**
```
feat(gtd): Implement Phase 3 Auto-Save and Data Integrity

- Created comprehensive auto-save system
- Built versioned backup with rotation
- Added data integrity validation
- Implemented recovery mechanisms
- Created sync status indicators
- Added emergency backup function
- Integrated save hooks
- Added keybindings under SPC o o b
```

## Implementation Process

### Development Workflow

1. **For Each Task:**
   - Create/update the module file
   - Implement core functionality
   - Add keybindings to UI
   - Create test file
   - Update plan with notes
   - Commit and push

2. **Testing Protocol:**
   - Unit test each function
   - Integration test with Phase 2
   - User workflow testing
   - Performance validation

3. **Quality Standards:**
   - Maintain <1s response times
   - Clear error messages
   - Consistent keybindings
   - Comprehensive documentation

## Success Criteria

Phase 3 is complete when:
- [ ] All contexts are implemented and functional
- [ ] Custom agenda views match personal workflow
- [ ] Decision support helps choose next tasks
- [ ] Quick access commands work instantly
- [ ] Auto-save prevents any data loss
- [ ] Integration with Phase 2 is seamless
- [ ] Personal productivity patterns are supported

## Next Phase Preview

Phase 4 will implement Review Cycles:
- Daily review automation
- Weekly review templates
- Monthly planning tools
- Metrics and insights
- Progress tracking

This phase focuses on making the GTD system truly personal and adapted to individual work patterns.