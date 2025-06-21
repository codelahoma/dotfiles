# Personal GTD-Zettelkasten Phase 4 Implementation Plan

## Overview

This document outlines the detailed implementation plan for Phase 4: Review Cycles of the Personal GTD-Zettelkasten Hybrid System. This phase implements comprehensive review workflows to maintain system integrity and gain insights from accumulated data.

## Purpose

This implementation aims to:

1. **Create Daily Review Workflow** - Quick morning and evening review routines
2. **Build Weekly Review System** - Comprehensive weekly GTD review process
3. **Implement Monthly/Quarterly Reviews** - Higher-level perspective and planning
4. **Add Review History Tracking** - Track patterns and progress over time
5. **Develop Metrics Collection** - Gain personal productivity insights

## Prerequisites

Before starting Phase 4 implementation:

- [x] Phase 3 complete with contexts and workflow patterns
- [x] Auto-save and data integrity working smoothly
- [x] Agenda views and decision support functional
- [x] Quick access commands operational
- [ ] Ready to implement review discipline

## Implementation Plan

### Phase 4: Review Cycles (Week 4-5)

#### Task 4.1: Implement Daily Review Workflow

**Status:** ✅ COMPLETE

**Purpose:** Create quick, efficient daily review routines for morning planning and evening reflection.

**Implementation Checklist:**
- [x] Build morning review template and workflow
- [x] Create evening review/shutdown routine
- [x] Implement daily metrics capture
- [x] Add quick review navigation
- [x] Create daily journal integration

**Reference Implementation:**
```elisp
;; In codelahoma-gtd-daily-review.el (new file):

;;; codelahoma-gtd-daily-review.el --- Daily review workflows -*- lexical-binding: t; -*-

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)

;;; Daily Review Configuration

(defcustom codelahoma-gtd-morning-review-items
  '(check-calendar
    review-next-actions
    check-waiting-for
    process-inbox-quick
    set-daily-priorities)
  "Items to include in morning review."
  :type '(repeat symbol)
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-evening-review-items
  '(capture-loose-ends
    process-inbox-full
    review-completed
    plan-tomorrow
    journal-reflection)
  "Items to include in evening review."
  :type '(repeat symbol)
  :group 'codelahoma-gtd)

;;; Morning Review

(defun codelahoma-gtd-morning-review ()
  "Execute morning review routine."
  (interactive)
  (let ((review-buffer (get-buffer-create "*GTD Morning Review*"))
        (start-time (current-time)))
    (with-current-buffer review-buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Morning Review - " 
              (format-time-string "%A, %B %d, %Y") "\n\n")
      
      ;; Review sections
      (codelahoma-gtd-morning-check-calendar)
      (codelahoma-gtd-morning-review-priorities)
      (codelahoma-gtd-morning-check-energy)
      
      ;; Quick stats
      (insert "\n* Quick Stats\n")
      (insert (format "- Inbox items: %d\n" 
                     (codelahoma-gtd-count-entries "inbox.org")))
      (insert (format "- Next actions: %d\n" 
                     (codelahoma-gtd-count-entries "next-actions.org" "NEXT")))
      (insert (format "- Overdue: %d\n" 
                     (codelahoma-gtd-count-overdue)))
      
      ;; Daily focus
      (insert "\n* Today's Focus (Pick 1-3)\n")
      (insert "1. [ ] \n2. [ ] \n3. [ ] \n")
      
      ;; Time estimate
      (insert (format "\n* Review completed in %.1f minutes\n"
                     (/ (float-time (time-subtract (current-time) start-time)) 60))))
    
    (switch-to-buffer review-buffer)
    (goto-char (point-min))
    (forward-line 6)))

(defun codelahoma-gtd-morning-check-calendar ()
  "Check today's calendar items."
  (insert "* Today's Schedule\n")
  (let ((entries (org-agenda-get-day-entries 
                  (expand-file-name "calendar.org" codelahoma-gtd-directory)
                  (calendar-current-date))))
    (if entries
        (dolist (entry entries)
          (insert (format "- %s\n" (org-agenda-format-item "" entry nil nil nil t))))
      (insert "- No scheduled items\n"))))

(defun codelahoma-gtd-morning-review-priorities ()
  "Review and set daily priorities."
  (insert "\n* Priority Review\n")
  ;; Get high priority items
  (let ((high-priority-tasks (codelahoma-gtd-get-priority-tasks ?A)))
    (if high-priority-tasks
        (progn
          (insert "High priority tasks:\n")
          (dolist (task high-priority-tasks)
            (insert (format "- [ ] %s\n" (plist-get task :title)))))
      (insert "No high priority tasks\n"))))

(defun codelahoma-gtd-morning-check-energy ()
  "Set energy level for the day."
  (insert "\n* Energy Check\n")
  (insert "Current energy level: ")
  (when (featurep 'codelahoma-gtd-contexts)
    (insert (format "[ ] High  [ ] Medium  [ ] Low\n"))))

;;; Evening Review

(defun codelahoma-gtd-evening-review ()
  "Execute evening review and shutdown routine."
  (interactive)
  (let ((review-buffer (get-buffer-create "*GTD Evening Review*"))
        (start-time (current-time)))
    (with-current-buffer review-buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Evening Review - " 
              (format-time-string "%A, %B %d, %Y %H:%M") "\n\n")
      
      ;; Capture loose ends
      (insert "* Capture Loose Ends\n")
      (insert "Anything left in your head? Capture it now:\n")
      (insert "- [ ] \n\n")
      
      ;; Review completed
      (codelahoma-gtd-evening-review-completed)
      
      ;; Process inbox
      (codelahoma-gtd-evening-process-check)
      
      ;; Plan tomorrow
      (codelahoma-gtd-evening-plan-tomorrow)
      
      ;; Journal prompt
      (insert "\n* Reflection\n")
      (insert "What went well today?\n- \n\n")
      (insert "What could be improved?\n- \n\n")
      (insert "Key learning or insight:\n- \n\n")
      
      ;; Save review
      (codelahoma-gtd-save-daily-review review-buffer))
    
    (switch-to-buffer review-buffer)
    (goto-char (point-min))
    (search-forward "Capture Loose Ends" nil t)
    (forward-line 2)))

(defun codelahoma-gtd-evening-review-completed ()
  "Review tasks completed today."
  (insert "\n* Completed Today\n")
  (let ((completed-today (codelahoma-gtd-get-completed-today)))
    (if completed-today
        (dolist (task completed-today)
          (insert (format "- %s\n" (plist-get task :title))))
      (insert "- No tasks marked complete today\n"))))

(defun codelahoma-gtd-evening-process-check ()
  "Check if inbox needs processing."
  (let ((inbox-count (codelahoma-gtd-count-entries "inbox.org")))
    (when (> inbox-count 0)
      (insert (format "\n* Inbox Status: %d items\n" inbox-count))
      (when (> inbox-count 5)
        (insert "⚠️  Consider processing before shutdown\n")))))

(defun codelahoma-gtd-evening-plan-tomorrow ()
  "Quick planning for tomorrow."
  (insert "\n* Tomorrow's Key Items\n")
  (insert "Top 3 priorities for tomorrow:\n")
  (insert "1. [ ] \n2. [ ] \n3. [ ] \n"))

;;; Review History

(defcustom codelahoma-gtd-review-directory
  (expand-file-name "reviews" codelahoma-gtd-directory)
  "Directory to store review history."
  :type 'directory
  :group 'codelahoma-gtd)

(defun codelahoma-gtd-save-daily-review (buffer)
  "Save daily review BUFFER to history."
  (let* ((date (format-time-string "%Y-%m-%d"))
         (time-of-day (if (< (string-to-number (format-time-string "%H")) 12)
                         "morning" "evening"))
         (filename (format "%s-%s-review.org" date time-of-day))
         (filepath (expand-file-name filename codelahoma-gtd-review-directory)))
    (unless (file-exists-p codelahoma-gtd-review-directory)
      (make-directory codelahoma-gtd-review-directory t))
    (with-current-buffer buffer
      (write-file filepath))
    (message "Review saved to %s" filename)))

;;; Quick Daily Checks

(defun codelahoma-gtd-quick-morning-check ()
  "Ultra-quick morning check."
  (interactive)
  (let ((cal-items (length (org-agenda-get-day-entries 
                           (expand-file-name "calendar.org" codelahoma-gtd-directory)
                           (calendar-current-date))))
        (inbox (codelahoma-gtd-count-entries "inbox.org"))
        (next (codelahoma-gtd-count-entries "next-actions.org" "NEXT")))
    (message "Morning: 📅 %d scheduled | 📥 %d inbox | ⚡ %d next | %s"
             cal-items inbox next (format-time-string "%A"))))

(defun codelahoma-gtd-quick-evening-check ()
  "Ultra-quick evening check."
  (interactive)
  (let ((completed (length (codelahoma-gtd-get-completed-today)))
        (inbox (codelahoma-gtd-count-entries "inbox.org")))
    (message "Evening: ✅ %d completed | 📥 %d inbox | Good work today!"
             completed inbox)))

;;; Daily Metrics

(defvar codelahoma-gtd-daily-metrics nil
  "Alist of daily metrics.")

(defun codelahoma-gtd-record-daily-metric (metric value)
  "Record a daily METRIC with VALUE."
  (let* ((date (format-time-string "%Y-%m-%d"))
         (date-metrics (or (assoc date codelahoma-gtd-daily-metrics)
                          (list date))))
    (setf (alist-get metric (cdr date-metrics)) value)
    (setf (alist-get date codelahoma-gtd-daily-metrics) (cdr date-metrics))))

(defun codelahoma-gtd-daily-metrics-summary ()
  "Show summary of daily metrics."
  (interactive)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (metrics (alist-get today codelahoma-gtd-daily-metrics)))
    (if metrics
        (message "Today's metrics: %s" metrics)
      (message "No metrics recorded today"))))

;;; Utility Functions

(defun codelahoma-gtd-get-completed-today ()
  "Get tasks completed today."
  (let ((today (format-time-string "%Y-%m-%d"))
        (completed '()))
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (let ((closed (org-entry-get nil "CLOSED")))
             (when (and closed (string-match today closed))
               (push (list :title (org-get-heading t t t t)
                          :file file)
                     completed))))
         "DONE")))
    (nreverse completed)))

(defun codelahoma-gtd-get-priority-tasks (priority)
  "Get tasks with specific PRIORITY."
  (let ((tasks '()))
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (when (= (org-get-priority (org-get-heading)) priority)
             (push (list :title (org-get-heading t t t t)
                        :priority priority
                        :file file)
                   tasks)))
         "TODO|NEXT")))
    (nreverse tasks)))

(provide 'codelahoma-gtd-daily-review)
;;; codelahoma-gtd-daily-review.el ends here
```

**Tests:**
- Test morning review workflow completion
- Verify evening review captures all elements
- Check review history saving
- Validate metrics recording
- Test quick check functions

**Implementation Notes:**
- Created codelahoma-gtd-daily-review.el with comprehensive morning and evening workflows
- Implemented morning review with calendar check, priority review, and energy assessment
- Built evening review with capture, completion review, and reflection prompts
- Added quick check functions for ultra-fast status updates
- Created daily metrics recording system for tracking patterns
- Implemented review history saving to dedicated directory
- Added daily journal integration linking to reviews
- Integrated utility functions for completed tasks and priority filtering
- Added keybindings under `SPC o o r` namespace with quick checks
- Created test-daily-review.el for verification

**Commit:**
```
feat(gtd): Implement Phase 4 Daily Review Workflow

- Created morning review with calendar and priorities
- Built evening review with reflection prompts
- Added quick morning/evening check functions
- Implemented daily metrics recording
- Created review history saving
- Added daily journal integration
- Integrated keybindings under SPC o o r
```

#### Task 4.2: Build Weekly Review System

**Status:** ✅ COMPLETE

**Purpose:** Implement comprehensive weekly GTD review process with guided workflow and progress tracking.

**Implementation Checklist:**
- [x] Create weekly review template
- [x] Build guided review workflow
- [x] Implement project review functions
- [x] Add someday/maybe processing
- [x] Create weekly metrics and insights

**Reference Implementation:**
```elisp
;; In codelahoma-gtd-weekly-review.el (new file):

;;; codelahoma-gtd-weekly-review.el --- Weekly review system -*- lexical-binding: t; -*-

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)

;;; Weekly Review Configuration

(defcustom codelahoma-gtd-weekly-review-day 5
  "Day of week for weekly review (0=Sunday, 5=Friday)."
  :type 'integer
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-weekly-review-steps
  '((:name "Collect Loose Papers and Materials" :time 5)
    (:name "Get Inbox to Zero" :time 30)
    (:name "Review Next Actions Lists" :time 15)
    (:name "Review Previous Calendar" :time 5)
    (:name "Review Upcoming Calendar" :time 5)
    (:name "Review Waiting For List" :time 10)
    (:name "Review Project List" :time 30)
    (:name "Review Someday/Maybe List" :time 15)
    (:name "Review Goals and Objectives" :time 10))
  "Steps in weekly review with estimated times."
  :type '(repeat (plist :name string :time integer))
  :group 'codelahoma-gtd)

;;; Weekly Review Interface

(defvar codelahoma-gtd-weekly-review-buffer "*GTD Weekly Review*"
  "Buffer name for weekly review.")

(defvar codelahoma-gtd-weekly-review-current-step 0
  "Current step in weekly review.")

(defvar codelahoma-gtd-weekly-review-start-time nil
  "Start time of current weekly review.")

(defun codelahoma-gtd-weekly-review ()
  "Start guided weekly review process."
  (interactive)
  (setq codelahoma-gtd-weekly-review-current-step 0)
  (setq codelahoma-gtd-weekly-review-start-time (current-time))
  (codelahoma-gtd-weekly-review-show-step))

(defun codelahoma-gtd-weekly-review-show-step ()
  "Show current step in weekly review."
  (let* ((buffer (get-buffer-create codelahoma-gtd-weekly-review-buffer))
         (step (nth codelahoma-gtd-weekly-review-current-step 
                   codelahoma-gtd-weekly-review-steps))
         (total-steps (length codelahoma-gtd-weekly-review-steps)))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Weekly Review - " 
              (format-time-string "%B %d, %Y") "\n\n")
      
      ;; Progress indicator
      (insert (format "* Progress: Step %d of %d\n" 
                     (1+ codelahoma-gtd-weekly-review-current-step)
                     total-steps))
      (insert (codelahoma-gtd-review-progress-bar 
               codelahoma-gtd-weekly-review-current-step total-steps))
      (insert "\n\n")
      
      ;; Current step
      (insert (format "* Current Step: %s\n" (plist-get step :name)))
      (insert (format "Estimated time: %d minutes\n\n" (plist-get step :time)))
      
      ;; Step-specific content
      (codelahoma-gtd-weekly-review-step-content step)
      
      ;; Navigation
      (insert "\n\n")
      (insert-button "[Previous]" 
                    'action (lambda (_) (codelahoma-gtd-weekly-review-previous)))
      (insert "  ")
      (insert-button "[Complete Step]" 
                    'action (lambda (_) (codelahoma-gtd-weekly-review-next)))
      (insert "  ")
      (insert-button "[Skip]" 
                    'action (lambda (_) (codelahoma-gtd-weekly-review-skip)))
      
      (read-only-mode 1))
    (switch-to-buffer buffer)))

(defun codelahoma-gtd-weekly-review-step-content (step)
  "Generate content for review STEP."
  (let ((step-name (plist-get step :name)))
    (cond
     ((string-match-p "Inbox to Zero" step-name)
      (codelahoma-gtd-review-inbox-content))
     ((string-match-p "Next Actions" step-name)
      (codelahoma-gtd-review-next-actions-content))
     ((string-match-p "Previous Calendar" step-name)
      (codelahoma-gtd-review-past-calendar))
     ((string-match-p "Upcoming Calendar" step-name)
      (codelahoma-gtd-review-future-calendar))
     ((string-match-p "Waiting For" step-name)
      (codelahoma-gtd-review-waiting-for-content))
     ((string-match-p "Project List" step-name)
      (codelahoma-gtd-review-projects-content))
     ((string-match-p "Someday/Maybe" step-name)
      (codelahoma-gtd-review-someday-content))
     ((string-match-p "Goals" step-name)
      (codelahoma-gtd-review-goals-content))
     (t (insert "Complete this step and click [Complete Step] to continue.")))))

;;; Step-specific content generators

(defun codelahoma-gtd-review-inbox-content ()
  "Generate inbox review content."
  (let ((inbox-count (codelahoma-gtd-count-entries "inbox.org")))
    (insert (format "Current inbox items: %d\n\n" inbox-count))
    (if (> inbox-count 0)
        (progn
          (insert "Process each item:\n")
          (insert "- Is it actionable?\n")
          (insert "- What's the next action?\n")
          (insert "- Does it belong to a project?\n\n")
          (insert-button "[Open Inbox]" 
                        'action (lambda (_) (codelahoma-gtd-open-inbox)))
          (insert "  ")
          (insert-button "[Process Inbox]" 
                        'action (lambda (_) (codelahoma-gtd-process-inbox))))
      (insert "✓ Inbox is empty!"))))

(defun codelahoma-gtd-review-next-actions-content ()
  "Generate next actions review content."
  (insert "Review all next actions:\n")
  (insert "- Mark complete if done\n")
  (insert "- Update if changed\n")
  (insert "- Ensure each has a clear next physical action\n\n")
  (let ((actions (codelahoma-gtd-get-all-next-actions)))
    (insert (format "Total next actions: %d\n\n" (length actions)))
    (insert-button "[Review Next Actions]" 
                  'action (lambda (_) (codelahoma-gtd-open-next-actions)))))

(defun codelahoma-gtd-review-projects-content ()
  "Generate project review content."
  (insert "Review each project:\n")
  (insert "- [ ] Has at least one next action?\n")
  (insert "- [ ] Still relevant and active?\n")
  (insert "- [ ] Outcome still valid?\n\n")
  (let ((projects (codelahoma-gtd-get-all-projects))
        (stalled 0))
    (dolist (project projects)
      (unless (codelahoma-gtd-project-has-next-action-p project)
        (cl-incf stalled)))
    (insert (format "Active projects: %d\n" (length projects)))
    (when (> stalled 0)
      (insert (format "⚠️  Stalled projects (no next action): %d\n" stalled)))
    (insert "\n")
    (insert-button "[Review Projects]" 
                  'action (lambda (_) (codelahoma-gtd-open-projects)))
    (insert "  ")
    (insert-button "[Find Stalled]" 
                  'action (lambda (_) (codelahoma-gtd-find-stalled-projects)))))

;;; Review Navigation

(defun codelahoma-gtd-weekly-review-next ()
  "Move to next step in weekly review."
  (interactive)
  (when (< codelahoma-gtd-weekly-review-current-step 
          (1- (length codelahoma-gtd-weekly-review-steps)))
    (cl-incf codelahoma-gtd-weekly-review-current-step)
    (codelahoma-gtd-weekly-review-show-step))
  (when (= codelahoma-gtd-weekly-review-current-step 
          (1- (length codelahoma-gtd-weekly-review-steps)))
    (codelahoma-gtd-weekly-review-complete)))

(defun codelahoma-gtd-weekly-review-previous ()
  "Go back to previous step."
  (interactive)
  (when (> codelahoma-gtd-weekly-review-current-step 0)
    (cl-decf codelahoma-gtd-weekly-review-current-step)
    (codelahoma-gtd-weekly-review-show-step)))

(defun codelahoma-gtd-weekly-review-skip ()
  "Skip current step."
  (interactive)
  (codelahoma-gtd-weekly-review-next))

(defun codelahoma-gtd-weekly-review-complete ()
  "Complete weekly review."
  (let ((duration (float-time (time-subtract (current-time) 
                                            codelahoma-gtd-weekly-review-start-time))))
    (with-current-buffer (get-buffer-create codelahoma-gtd-weekly-review-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (insert "#+TITLE: Weekly Review Complete!\n\n")
      (insert (format "* Review completed in %.1f minutes\n\n" (/ duration 60)))
      (insert "* Summary\n")
      (codelahoma-gtd-weekly-review-summary)
      (insert "\n* Next Week's Focus\n")
      (insert "1. [ ] \n2. [ ] \n3. [ ] \n")
      (read-only-mode 1))
    (codelahoma-gtd-save-weekly-review)))

;;; Weekly Metrics

(defun codelahoma-gtd-weekly-review-summary ()
  "Generate weekly summary."
  (let* ((completed-this-week (codelahoma-gtd-count-completed-this-week))
         (created-this-week (codelahoma-gtd-count-created-this-week))
         (inbox-processed (- created-this-week 
                           (codelahoma-gtd-count-entries "inbox.org"))))
    (insert (format "- Tasks completed: %d\n" completed-this-week))
    (insert (format "- Tasks created: %d\n" created-this-week))
    (insert (format "- Inbox items processed: %d\n" inbox-processed))
    (insert (format "- Current projects: %d\n" 
                   (length (codelahoma-gtd-get-all-projects))))
    (insert (format "- Current next actions: %d\n" 
                   (codelahoma-gtd-count-entries "next-actions.org" "NEXT")))))

(defun codelahoma-gtd-save-weekly-review ()
  "Save weekly review to history."
  (let* ((date (format-time-string "%Y-%m-%d"))
         (filename (format "%s-weekly-review.org" date))
         (filepath (expand-file-name filename codelahoma-gtd-review-directory)))
    (unless (file-exists-p codelahoma-gtd-review-directory)
      (make-directory codelahoma-gtd-review-directory t))
    (with-current-buffer codelahoma-gtd-weekly-review-buffer
      (write-file filepath))
    (message "Weekly review saved to %s" filename)))

;;; Review Helpers

(defun codelahoma-gtd-review-progress-bar (current total)
  "Generate progress bar for CURRENT of TOTAL."
  (let* ((width 40)
         (filled (/ (* current width) total))
         (empty (- width filled)))
    (concat "["
            (make-string filled ?=)
            ">"
            (make-string empty ? )
            "]")))

(defun codelahoma-gtd-project-has-next-action-p (project)
  "Check if PROJECT has a next action."
  (with-current-buffer (find-file-noselect 
                       (expand-file-name "projects.org" codelahoma-gtd-directory))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (concat "^\\* PROJECT " 
                                     (regexp-quote (plist-get project :title))) 
                              nil t)
        (org-narrow-to-subtree)
        (goto-char (point-min))
        (prog1 (re-search-forward "^\\*+ NEXT " nil t)
          (widen))))))

(provide 'codelahoma-gtd-weekly-review)
;;; codelahoma-gtd-weekly-review.el ends here
```

**Tests:**
- Test weekly review workflow navigation
- Verify step content generation
- Check progress tracking
- Validate review saving
- Test summary generation

**Implementation Notes:**
- Created codelahoma-gtd-weekly-review.el with comprehensive guided workflow
- Implemented 10-step review process covering Get Clear, Get Current, Get Creative phases
- Built step-by-step navigation with progress tracking and time estimates
- Created detailed content generators for each review step
- Added project stalled detection and next action validation
- Implemented weekly metrics tracking (completed, created, processed)
- Created time tracking for each step with summary report
- Added quick weekly check function for review reminders
- Built cancel functionality with confirmation
- Integrated all steps with appropriate action buttons
- Added keybindings for weekly review and quick check
- Created test-weekly-review.el for verification

**Commit:**
```
feat(gtd): Implement Phase 4 Weekly Review System

- Created guided 10-step weekly review workflow
- Built progress tracking with time estimates
- Added detailed content for each review phase
- Implemented project review with stalled detection
- Created weekly metrics and summary
- Added time tracking per step
- Built quick weekly check reminder
- Integrated keybindings under SPC o o r w
```

#### Task 4.3: Implement Monthly and Quarterly Reviews

**Status:** [ ] TODO

**Purpose:** Create higher-level review processes for strategic planning and life design.

**Implementation Checklist:**
- [ ] Design monthly review template
- [ ] Create quarterly planning workflow
- [ ] Build annual review structure
- [ ] Implement goal tracking
- [ ] Add life areas assessment

**Reference Implementation:**
```elisp
;; Additions to codelahoma-gtd-review.el:

;;; Monthly Review

(defun codelahoma-gtd-monthly-review ()
  "Execute monthly review process."
  (interactive)
  (let ((review-buffer (get-buffer-create "*GTD Monthly Review*")))
    (with-current-buffer review-buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Monthly Review - " 
              (format-time-string "%B %Y") "\n\n")
      
      ;; Month in review
      (insert "* Month in Review\n")
      (codelahoma-gtd-monthly-accomplishments)
      (codelahoma-gtd-monthly-challenges)
      
      ;; Areas of focus review
      (insert "\n* Areas of Focus Assessment\n")
      (dolist (area codelahoma-gtd-areas-of-focus)
        (insert (format "** %s\n" area))
        (insert "Progress (1-10): [ ]\n")
        (insert "Key wins:\n- \n")
        (insert "Needs attention:\n- \n\n"))
      
      ;; Project portfolio review
      (insert "* Project Portfolio\n")
      (codelahoma-gtd-monthly-project-analysis)
      
      ;; Next month planning
      (insert "\n* Next Month Focus\n")
      (insert "Top 3 outcomes for next month:\n")
      (insert "1. [ ] \n2. [ ] \n3. [ ] \n"))
    
    (switch-to-buffer review-buffer)))

;;; Quarterly Review

(defun codelahoma-gtd-quarterly-review ()
  "Execute quarterly planning and review."
  (interactive)
  (let ((review-buffer (get-buffer-create "*GTD Quarterly Review*")))
    (with-current-buffer review-buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Quarterly Review - Q" 
              (format "%d %d" 
                     (1+ (/ (1- (string-to-number (format-time-string "%m"))) 3))
                     (string-to-number (format-time-string "%Y")))
              "\n\n")
      
      ;; Quarter metrics
      (insert "* Quarter Metrics\n")
      (codelahoma-gtd-quarterly-metrics)
      
      ;; Goal review
      (insert "\n* Goal Progress\n")
      (codelahoma-gtd-review-quarterly-goals)
      
      ;; Life design check
      (insert "\n* Life Design Check\n")
      (insert "** What's working well?\n- \n\n")
      (insert "** What needs to change?\n- \n\n")
      (insert "** What am I avoiding?\n- \n\n")
      
      ;; Next quarter planning
      (insert "* Next Quarter Plan\n")
      (insert "** Theme: \n")
      (insert "** Key Objectives:\n")
      (insert "1. [ ] \n2. [ ] \n3. [ ] \n"))
    
    (switch-to-buffer review-buffer)))
```

**Tests:**
- Test monthly review generation
- Verify quarterly metrics calculation
- Check goal tracking integration
- Validate area assessment
- Test review templates

#### Task 4.4: Add Review History and Analytics

**Status:** [ ] TODO

**Purpose:** Track review completion and extract insights from historical data.

**Implementation Checklist:**
- [ ] Create review tracking database
- [ ] Build completion streak tracking
- [ ] Implement pattern recognition
- [ ] Add productivity analytics
- [ ] Create insights dashboard

**Reference Implementation:**
```elisp
;; In codelahoma-gtd-analytics.el (new file):

;;; codelahoma-gtd-analytics.el --- Review analytics and insights -*- lexical-binding: t; -*-

(require 'codelahoma-gtd-config)

;;; Analytics Database

(defcustom codelahoma-gtd-analytics-file
  (expand-file-name "analytics.el" codelahoma-gtd-directory)
  "File to store analytics data."
  :type 'file
  :group 'codelahoma-gtd)

(defvar codelahoma-gtd-analytics-data nil
  "Analytics data structure.")

(defun codelahoma-gtd-load-analytics ()
  "Load analytics data from file."
  (when (file-exists-p codelahoma-gtd-analytics-file)
    (load-file codelahoma-gtd-analytics-file)))

(defun codelahoma-gtd-save-analytics ()
  "Save analytics data to file."
  (with-temp-file codelahoma-gtd-analytics-file
    (print `(setq codelahoma-gtd-analytics-data ',codelahoma-gtd-analytics-data)
           (current-buffer))))

;;; Review Tracking

(defun codelahoma-gtd-record-review (type)
  "Record completion of review TYPE."
  (let* ((date (format-time-string "%Y-%m-%d"))
         (time (format-time-string "%H:%M"))
         (entry (list :type type :date date :time time)))
    (push entry (alist-get 'reviews codelahoma-gtd-analytics-data))
    (codelahoma-gtd-save-analytics)))

(defun codelahoma-gtd-review-streak (type)
  "Calculate current streak for review TYPE."
  (let ((reviews (seq-filter (lambda (r) (eq (plist-get r :type) type))
                            (alist-get 'reviews codelahoma-gtd-analytics-data)))
        (streak 0)
        (last-date nil))
    ;; Calculate streak logic
    streak))

;;; Productivity Metrics

(defun codelahoma-gtd-weekly-velocity ()
  "Calculate average weekly task completion velocity."
  (let* ((weeks-data (codelahoma-gtd-get-weeks-data 4))
         (velocities (mapcar (lambda (week) 
                              (plist-get week :completed))
                            weeks-data)))
    (if velocities
        (/ (apply '+ velocities) (length velocities))
      0)))

(defun codelahoma-gtd-completion-rate ()
  "Calculate task completion rate."
  (let* ((created (alist-get 'tasks-created codelahoma-gtd-analytics-data))
         (completed (alist-get 'tasks-completed codelahoma-gtd-analytics-data)))
    (if (and created completed (> created 0))
        (* 100.0 (/ completed created))
      0)))

;;; Insights Dashboard

(defun codelahoma-gtd-insights-dashboard ()
  "Display productivity insights dashboard."
  (interactive)
  (let ((buffer (get-buffer-create "*GTD Insights*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: GTD Analytics & Insights\n\n")
      
      ;; Review streaks
      (insert "* Review Streaks\n")
      (insert (format "- Daily: %d days\n" (codelahoma-gtd-review-streak 'daily)))
      (insert (format "- Weekly: %d weeks\n" (codelahoma-gtd-review-streak 'weekly)))
      (insert (format "- Monthly: %d months\n" (codelahoma-gtd-review-streak 'monthly)))
      
      ;; Productivity metrics
      (insert "\n* Productivity Metrics\n")
      (insert (format "- Weekly velocity: %.1f tasks/week\n" 
                     (codelahoma-gtd-weekly-velocity)))
      (insert (format "- Completion rate: %.1f%%\n" 
                     (codelahoma-gtd-completion-rate)))
      
      ;; Patterns
      (insert "\n* Patterns & Insights\n")
      (codelahoma-gtd-analyze-patterns))
    
    (switch-to-buffer buffer)))

(provide 'codelahoma-gtd-analytics)
;;; codelahoma-gtd-analytics.el ends here
```

**Tests:**
- Test analytics data persistence
- Verify streak calculations
- Check metric accuracy
- Validate pattern detection
- Test dashboard generation

#### Task 4.5: Create Review Automation and Reminders

**Status:** [ ] TODO

**Purpose:** Automate review scheduling and provide gentle reminders to maintain review discipline.

**Implementation Checklist:**
- [ ] Build review scheduling system
- [ ] Create reminder notifications
- [ ] Implement review templates
- [ ] Add calendar integration
- [ ] Create review habits tracking

**Reference Implementation:**
```elisp
;; Additions to codelahoma-gtd-review.el:

;;; Review Scheduling

(defcustom codelahoma-gtd-review-reminders t
  "Enable review reminders."
  :type 'boolean
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-morning-review-time "08:00"
  "Time for morning review reminder."
  :type 'string
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-evening-review-time "17:00"
  "Time for evening review reminder."
  :type 'string
  :group 'codelahoma-gtd)

(defvar codelahoma-gtd-review-timers nil
  "Active review reminder timers.")

(defun codelahoma-gtd-schedule-review-reminders ()
  "Schedule all review reminders."
  (interactive)
  (codelahoma-gtd-cancel-review-reminders)
  (when codelahoma-gtd-review-reminders
    ;; Daily reviews
    (push (run-at-time codelahoma-gtd-morning-review-time
                      86400 ; daily
                      'codelahoma-gtd-morning-review-reminder)
          codelahoma-gtd-review-timers)
    (push (run-at-time codelahoma-gtd-evening-review-time
                      86400 ; daily
                      'codelahoma-gtd-evening-review-reminder)
          codelahoma-gtd-review-timers)
    ;; Weekly review
    (push (run-at-time (codelahoma-gtd-next-weekly-review-time)
                      604800 ; weekly
                      'codelahoma-gtd-weekly-review-reminder)
          codelahoma-gtd-review-timers)))

(defun codelahoma-gtd-cancel-review-reminders ()
  "Cancel all review reminders."
  (interactive)
  (dolist (timer codelahoma-gtd-review-timers)
    (when (timerp timer)
      (cancel-timer timer)))
  (setq codelahoma-gtd-review-timers nil))

;;; Review Templates

(defun codelahoma-gtd-create-review-template (type)
  "Create review template for TYPE."
  (let ((template-file (expand-file-name 
                       (format "templates/review-%s.org" type)
                       codelahoma-gtd-directory)))
    (unless (file-exists-p (file-name-directory template-file))
      (make-directory (file-name-directory template-file) t))
    (with-temp-file template-file
      (insert (codelahoma-gtd-review-template-content type)))
    template-file))

;;; Review Habits

(defun codelahoma-gtd-review-habit-tracker ()
  "Show review habit completion."
  (interactive)
  (let ((buffer (get-buffer-create "*GTD Review Habits*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Review Habit Tracker\n\n")
      (insert "* Last 30 Days\n")
      (codelahoma-gtd-show-habit-calendar 'daily 30)
      (insert "\n* This Week\n")
      (codelahoma-gtd-show-week-reviews))
    (switch-to-buffer buffer)))

(provide 'codelahoma-gtd-review)
;;; codelahoma-gtd-review.el ends here
```

**Tests:**
- Test reminder scheduling
- Verify template generation
- Check habit tracking
- Validate calendar integration
- Test automation features

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
   - Integration test workflows
   - User acceptance testing
   - Performance validation

3. **Documentation:**
   - Update user guide
   - Add inline documentation
   - Create workflow diagrams
   - Update keybinding reference

## Success Criteria

- [ ] Daily reviews take less than 5 minutes
- [ ] Weekly review completes in under 45 minutes
- [ ] 80%+ review completion rate
- [ ] Meaningful insights from analytics
- [ ] Sustainable review habits formed

## Next Steps

After Phase 4 completion:
1. Begin Phase 5: Knowledge Integration Bridge
2. Test review cycles for one month
3. Refine based on usage patterns
4. Prepare for advanced integration features