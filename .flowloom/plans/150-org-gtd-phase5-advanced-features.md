# Org-GTD Phase 5: Advanced Features Implementation

## Overview

This document outlines a detailed implementation plan for Phase 5 of the Org-GTD system: Advanced Features. This phase adds sophisticated workflows including comprehensive review systems, time tracking with pomodoro integration, batch processing capabilities, and modern integrations for mobile and email workflows.

## Purpose

This implementation aims to:

1. **Systematize Reviews** - Create structured weekly and daily review workflows with health checks
2. **Enable Time Tracking** - Integrate org-clock with pomodoro techniques and reporting
3. **Streamline Processing** - Build batch operations for efficient inbox and task management
4. **Modernize Workflow** - Add mobile capture and email integration capabilities

## Prerequisites

Before starting the implementation:

- [ ] Phases 1-4 are complete and functioning properly
- [ ] Basic GTD workflow is established and tested
- [ ] Understanding of org-clock and org-timer systems
- [ ] Mobile capture strategy determined (Dropbox, syncthing, etc.)

## Implementation Plan

### Phase 5.1: Review System

#### Task 5.1.1: Weekly Review Implementation

**Status:** 📝 PLANNED

**Purpose:** Create a comprehensive weekly review system that guides through all aspects of GTD maintenance and planning.

**Implementation Checklist:**
- [ ] Create weekly review template and checklist
- [ ] Build guided review workflow function
- [ ] Add project health check automation
- [ ] Create review statistics and reporting
- [ ] Configure review scheduling and reminders

**Reference Implementation:**
```elisp
(defun rk/weekly-review ()
  "Comprehensive GTD weekly review process."
  (interactive)
  (let ((review-buffer "*GTD Weekly Review*")
        (start-time (current-time)))
    
    ;; Create review buffer
    (switch-to-buffer (get-buffer-create review-buffer))
    (erase-buffer)
    (org-mode)
    (insert "* GTD Weekly Review - " (format-time-string "%Y-%m-%d %a") "\n\n")
    
    ;; Step 1: Clear Inbox
    (insert "** ☐ Clear Inbox\n")
    (insert "   [[file:" (rk/org-file "inbox.org") "][Open Inbox]]\n")
    (insert "   Current items: " 
            (number-to-string (rk/count-inbox-items)) "\n\n")
    
    ;; Step 2: Process Action Lists
    (insert "** ☐ Review Action Lists\n")
    (rk/insert-context-stats)
    (insert "\n")
    
    ;; Step 3: Review Projects
    (insert "** ☐ Review Projects\n")
    (rk/insert-project-health-report)
    (insert "\n")
    
    ;; Step 4: Review Waiting Items
    (insert "** ☐ Review Waiting Items\n")
    (rk/insert-waiting-items-summary)
    (insert "\n")
    
    ;; Step 5: Review Someday/Maybe
    (insert "** ☐ Review Someday/Maybe Lists\n")
    (insert "   [[file:" (rk/org-file "work/someday.org") "][Work Someday]]\n")
    (insert "   [[file:" (rk/org-file "personal/someday.org") "][Personal Someday]]\n\n")
    
    ;; Step 6: Review Calendar
    (insert "** ☐ Review Calendar\n")
    (insert "   - Past week: Completed commitments?\n")
    (insert "   - Next week: Upcoming deadlines?\n")
    (insert "   - Future: Any new commitments?\n\n")
    
    ;; Step 7: Mind Sweep
    (insert "** ☐ Mind Sweep\n")
    (insert "*** Capture any loose thoughts:\n")
    (insert "- \n- \n- \n- \n- \n\n")
    
    ;; Step 8: Process Notes
    (insert "** ☐ Process Notes and Ideas\n\n")
    
    ;; Step 9: Review Goals
    (insert "** ☐ Review Goals and Objectives\n")
    (insert "*** This Week's Priorities:\n")
    (insert "1. \n2. \n3. \n\n")
    
    ;; Make buffer interactive
    (goto-char (point-min))
    (rk/weekly-review-mode)))

(defun rk/count-inbox-items ()
  "Count number of items in inbox."
  (let ((count 0))
    (with-current-buffer (find-file-noselect (rk/org-file "inbox.org"))
      (org-map-entries
       (lambda () (setq count (1+ count)))
       "LEVEL=1"))
    count))

(defun rk/insert-context-stats ()
  "Insert statistics for each context."
  (dolist (context '("@work" "@personal"))
    (let ((next-count 0)
          (todo-count 0)
          (total-time 0))
      (org-map-entries
       (lambda ()
         (let ((state (org-get-todo-state)))
           (cond ((string= state "NEXT") (setq next-count (1+ next-count)))
                 ((string= state "TODO") (setq todo-count (1+ todo-count))))
           (setq total-time (+ total-time (or (org-clock-sum-current-item) 0)))))
       (concat context "/!"))
      (insert (format "   - %s: %d NEXT, %d TODO (%.1f hours)\n"
                      context next-count todo-count (/ total-time 60.0))))))

(defun rk/insert-project-health-report ()
  "Insert project health analysis."
  (let ((projects '()))
    ;; Collect all projects
    (dolist (file (list (rk/org-file "work/projects.org")
                        (rk/org-file "personal/projects.org")))
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (let* ((title (org-get-heading t t t t))
                  (next-actions (rk/count-project-next-actions))
                  (total-tasks (rk/count-project-tasks))
                  (age (rk/project-age))
                  (health (cond
                           ((= next-actions 0) "🔴 Stalled")
                           ((> age 30) "🟡 Aging")
                           (t "🟢 Healthy"))))
             (push (list title health next-actions total-tasks age) projects)))
         "LEVEL=1")))
    
    ;; Display project summary
    (dolist (project (nreverse projects))
      (insert (format "   - %s %s (%d/%d tasks, %d days old)\n"
                      (nth 1 project) ; health
                      (nth 0 project) ; title
                      (nth 2 project) ; next actions
                      (nth 3 project) ; total tasks
                      (nth 4 project)))))) ; age

(defun rk/count-project-next-actions ()
  "Count NEXT actions in current project."
  (let ((count 0))
    (org-map-entries
     (lambda ()
       (when (string= (org-get-todo-state) "NEXT")
         (setq count (1+ count))))
     nil 'tree)
    count))

(defun rk/count-project-tasks ()
  "Count all tasks in current project."
  (let ((count 0))
    (org-map-entries
     (lambda ()
       (when (org-get-todo-state)
         (setq count (1+ count))))
     nil 'tree)
    count))

(defun rk/project-age ()
  "Calculate project age in days."
  (let ((created (org-entry-get nil "CREATED")))
    (if created
        (/ (time-to-seconds 
            (time-subtract (current-time)
                           (org-time-string-to-time created)))
           86400)
      0)))

;; Review mode for interactive checking
(define-minor-mode rk/weekly-review-mode
  "Minor mode for weekly review interaction."
  :lighter " Review"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'rk/check-review-item)
            (define-key map (kbd "C-c C-x") 'rk/finish-review)
            (define-key map (kbd "C-c C-a") 'org-agenda)
            (define-key map (kbd "C-c C-i") 'org-capture)
            map))

(defun rk/check-review-item ()
  "Mark current review item as complete."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "\\*\\* ☐")
      (replace-match "** ☑")
      (forward-line))))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

#### Task 5.1.2: Daily Review and Planning

**Status:** 📝 PLANNED

**Purpose:** Create a quick daily review process for maintaining momentum and planning the next day.

**Implementation Checklist:**
- [ ] Create daily review template
- [ ] Build morning planning workflow
- [ ] Add end-of-day shutdown ritual
- [ ] Configure daily metrics tracking
- [ ] Create quick review functions

**Reference Implementation:**
```elisp
(defun rk/daily-review ()
  "Quick daily GTD review and planning."
  (interactive)
  (let ((review-buffer "*GTD Daily Review*"))
    (switch-to-buffer (get-buffer-create review-buffer))
    (erase-buffer)
    (org-mode)
    (insert "* Daily Review - " (format-time-string "%Y-%m-%d %a %H:%M") "\n\n")
    
    ;; Today's accomplishments
    (insert "** Today's Accomplishments\n")
    (rk/insert-todays-completed-tasks)
    (insert "\n")
    
    ;; Time tracking summary
    (insert "** Time Tracking Summary\n")
    (rk/insert-todays-time-summary)
    (insert "\n")
    
    ;; Tomorrow's priorities
    (insert "** Tomorrow's Priorities\n")
    (insert "*** Must Do (Top 3):\n")
    (insert "1. \n2. \n3. \n\n")
    
    ;; Quick inbox check
    (insert "** Inbox Status: " 
            (number-to-string (rk/count-inbox-items)) " items\n\n")
    
    ;; Energy assessment
    (insert "** Energy and Focus Assessment\n")
    (insert "- Morning energy peak: [  ] Used well [  ] Could improve\n")
    (insert "- Afternoon productivity: [  ] Maintained [  ] Dipped\n")
    (insert "- Overall satisfaction: [  ] High [  ] Medium [  ] Low\n\n")
    
    ;; Notes for tomorrow
    (insert "** Notes for Tomorrow\n")
    (insert "- \n- \n\n")
    
    (goto-char (point-min))))

(defun rk/insert-todays-completed-tasks ()
  "Insert list of tasks completed today."
  (let ((tasks '()))
    (org-map-entries
     (lambda ()
       (let ((closed (org-entry-get nil "CLOSED")))
         (when (and closed
                    (string= (format-time-string "%Y-%m-%d")
                             (format-time-string "%Y-%m-%d" 
                                                 (org-time-string-to-time closed))))
           (push (org-get-heading t t t t) tasks))))
     "/DONE" 'agenda)
    
    (if tasks
        (dolist (task (nreverse tasks))
          (insert "- ✓ " task "\n"))
      (insert "- No completed tasks logged\n"))))

(defun rk/insert-todays-time-summary ()
  "Insert time tracking summary for today."
  (let ((total-time 0)
        (work-time 0)
        (personal-time 0))
    ;; Calculate time by context
    (org-map-entries
     (lambda ()
       (let ((time (org-clock-sum-today)))
         (when (> time 0)
           (setq total-time (+ total-time time))
           (cond
            ((member "@work" (org-get-tags))
             (setq work-time (+ work-time time)))
            ((member "@personal" (org-get-tags))
             (setq personal-time (+ personal-time time)))))))
     nil 'agenda)
    
    (insert (format "- Total tracked: %.1f hours\n" (/ total-time 60.0)))
    (insert (format "- Work: %.1f hours\n" (/ work-time 60.0)))
    (insert (format "- Personal: %.1f hours\n" (/ personal-time 60.0)))))

(defun rk/morning-planning ()
  "Quick morning planning ritual."
  (interactive)
  (org-agenda nil "f") ; Focus view - top 3
  (other-window 1)
  (rk/daily-review))

(defun rk/evening-shutdown ()
  "Evening shutdown ritual."
  (interactive)
  ;; Clock out if needed
  (when (org-clocking-p)
    (org-clock-out))
  
  ;; Process inbox quickly
  (when (> (rk/count-inbox-items) 0)
    (find-file (rk/org-file "inbox.org")))
  
  ;; Run daily review
  (rk/daily-review)
  
  ;; Archive completed tasks
  (when (y-or-n-p "Archive today's completed tasks? ")
    (rk/archive-done-tasks)))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

### Phase 5.2: Time Tracking

#### Task 5.2.1: Clock Configuration and Reporting

**Status:** 📝 PLANNED

**Purpose:** Configure org-clock for effective time tracking with customized reports and analysis.

**Implementation Checklist:**
- [ ] Configure clock persistence and resume
- [ ] Set up clock report templates
- [ ] Create time analysis functions
- [ ] Add clock-in helpers and hydra
- [ ] Configure effort estimates

**Reference Implementation:**
```elisp
;; Clock persistence
(setq org-clock-persist t
      org-clock-persist-file (rk/org-file ".org-clock-save.el")
      org-clock-persist-query-resume nil
      org-clock-in-resume t
      org-clock-out-when-done t
      org-clock-out-remove-zero-time-clocks t)

;; Clock display
(setq org-clock-clocked-in-display 'mode-line
      org-clock-mode-line-total 'today
      org-clock-report-include-clocking-task t)

;; Effort estimates
(setq org-global-properties
      '(("Effort_ALL" . "0:15 0:30 1:00 2:00 3:00 4:00 5:00 6:00 8:00")))

;; Clock report parameters
(setq org-clock-clocktable-default-properties
      '(:maxlevel 4 :scope file :block today :link t :compact t :formula %))

(defun rk/clock-in-to-next ()
  "Clock in to the next task in agenda."
  (interactive)
  (org-agenda-clock-in))

(defun rk/clock-in-last-task ()
  "Clock in on the last task clocked."
  (interactive)
  (org-clock-in-last))

(defun rk/punch-in ()
  "Start continuous clocking."
  (interactive)
  (setq rk/keep-clock-running t)
  (rk/clock-in-default-task))

(defun rk/punch-out ()
  "Stop continuous clocking."
  (interactive)
  (setq rk/keep-clock-running nil)
  (when (org-clocking-p)
    (org-clock-out)))

(defun rk/clock-in-default-task ()
  "Clock in to the default organization task."
  (save-excursion
    (org-with-point-at (org-id-find rk/organization-task-id 'marker)
      (org-clock-in))))

;; Automatic clocking
(defvar rk/organization-task-id "organization")

(defun rk/clock-out-maybe ()
  "Clock out when task is marked DONE."
  (when (and (member (org-get-todo-state) org-done-keywords)
             (org-clocking-p))
    (org-clock-out)))

(add-hook 'org-after-todo-state-change-hook 'rk/clock-out-maybe)

;; Time reporting functions
(defun rk/weekly-time-report ()
  "Generate weekly time report."
  (interactive)
  (let ((report-buffer "*Weekly Time Report*"))
    (switch-to-buffer (get-buffer-create report-buffer))
    (erase-buffer)
    (org-mode)
    (insert "* Weekly Time Report\n")
    (insert "#+BEGIN: clocktable :scope agenda :maxlevel 3 :block thisweek :link t\n")
    (insert "#+END:\n\n")
    (insert "* Daily Breakdown\n")
    (insert "#+BEGIN: clocktable :scope agenda :maxlevel 2 :block thisweek :step day :link t\n")
    (insert "#+END:\n\n")
    (insert "* By Category\n")
    (insert "#+BEGIN: clocktable :scope agenda :maxlevel 2 :block thisweek :tags t\n")
    (insert "#+END:\n")
    (goto-char (point-min))
    (org-update-all-dblocks)))

;; Clock hydra for quick access
(with-eval-after-load 'hydra
  (defhydra rk/clock-hydra (:hint nil)
    "
Clock: _i_n _o_ut _c_ancel _g_oto _l_ast _r_eport _e_ffort _p_unch-in _P_unch-out
"
    ("i" org-clock-in)
    ("o" org-clock-out)
    ("c" org-clock-cancel)
    ("g" org-clock-goto)
    ("l" org-clock-in-last)
    ("r" rk/weekly-time-report)
    ("e" org-set-effort)
    ("p" rk/punch-in)
    ("P" rk/punch-out)
    ("q" nil "quit" :exit t)))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

#### Task 5.2.2: Pomodoro Integration

**Status:** 📝 PLANNED

**Purpose:** Integrate pomodoro technique with org-clock for focused work sessions with breaks.

**Implementation Checklist:**
- [ ] Set up org-pomodoro package configuration
- [ ] Create custom pomodoro workflows
- [ ] Add break management and notifications
- [ ] Configure pomodoro statistics
- [ ] Build focus session templates

**Reference Implementation:**
```elisp
;; Pomodoro configuration
(use-package org-pomodoro
  :after org
  :config
  (setq org-pomodoro-length 25
        org-pomodoro-short-break-length 5
        org-pomodoro-long-break-length 15
        org-pomodoro-long-break-frequency 4
        org-pomodoro-ask-upon-killing t
        org-pomodoro-format "🍅 %s"
        org-pomodoro-short-break-format "☕ %s"
        org-pomodoro-long-break-format "🌴 %s")
  
  ;; Sound notifications
  (setq org-pomodoro-audio-player (executable-find "afplay")
        org-pomodoro-finished-sound-p t
        org-pomodoro-short-break-sound-p t
        org-pomodoro-long-break-sound-p t)
  
  ;; Hooks
  (add-hook 'org-pomodoro-finished-hook
            (lambda ()
              (rk/notify "Pomodoro completed!" "Time for a break.")))
  
  (add-hook 'org-pomodoro-break-finished-hook
            (lambda ()
              (rk/notify "Break finished!" "Ready for next pomodoro?")))
  
  (add-hook 'org-pomodoro-started-hook
            (lambda ()
              (rk/prepare-focus-mode))))

(defun rk/notify (title message)
  "Send system notification."
  (cond
   ((eq system-type 'darwin)
    (shell-command 
     (format "osascript -e 'display notification \"%s\" with title \"%s\"'" 
             message title)))
   ((eq system-type 'gnu/linux)
    (shell-command (format "notify-send \"%s\" \"%s\"" title message)))))

(defun rk/prepare-focus-mode ()
  "Prepare environment for focused work."
  (delete-other-windows)
  (when (fboundp 'do-not-disturb-mode)
    (do-not-disturb-mode 1)))

(defun rk/start-focus-session ()
  "Start a focused work session with pomodoros."
  (interactive)
  (let* ((task (org-agenda-get-marked-entries))
         (estimated-pomodoros (read-number "Estimated pomodoros: " 2)))
    (org-pomodoro estimated-pomodoros)))

(defun rk/pomodoro-daily-summary ()
  "Show summary of today's pomodoros."
  (interactive)
  (let ((count 0)
        (tasks '()))
    (org-map-entries
     (lambda ()
       (let ((pomodoros (org-entry-get nil "POMODOROS")))
         (when pomodoros
           (let ((today-pomodoros (rk/count-todays-pomodoros pomodoros)))
             (when (> today-pomodoros 0)
               (setq count (+ count today-pomodoros))
               (push (cons (org-get-heading t t t t) today-pomodoros) tasks))))))
     nil 'agenda)
    
    (message "Today: %d pomodoros completed on %d tasks" 
             count (length tasks))))

;; Pomodoro planning
(defun rk/plan-pomodoros ()
  "Plan pomodoros for today's tasks."
  (interactive)
  (org-agenda nil "f") ; Focus view
  (let ((total-pomodoros 0))
    (save-excursion
      (goto-char (point-min))
      (while (org-agenda-next-item 1)
        (let ((effort (org-agenda-get-effort)))
          (when effort
            (let ((minutes (org-duration-to-minutes effort))
                  (pomodoros (ceiling (/ minutes 25.0))))
              (setq total-pomodoros (+ total-pomodoros pomodoros))
              (org-agenda-add-note)
              (insert (format "Estimated pomodoros: %d" pomodoros)))))))
    (message "Total pomodoros planned: %d (%.1f hours)" 
             total-pomodoros 
             (* total-pomodoros (/ 25.0 60)))))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

### Phase 5.3: Batch Processing

#### Task 5.3.1: Interactive Inbox Processing

**Status:** 📝 PLANNED

**Purpose:** Create efficient batch processing workflows for handling multiple inbox items quickly.

**Implementation Checklist:**
- [ ] Build interactive inbox processor
- [ ] Create bulk refile interface
- [ ] Add quick decision shortcuts
- [ ] Configure processing statistics
- [ ] Create inbox zero workflow

**Reference Implementation:**
```elisp
(defun rk/process-inbox ()
  "Interactive inbox processing with quick decisions."
  (interactive)
  (find-file (rk/org-file "inbox.org"))
  (goto-char (point-min))
  (org-next-visible-heading 1)
  (rk/inbox-processing-mode 1))

(define-minor-mode rk/inbox-processing-mode
  "Minor mode for efficient inbox processing."
  :lighter " InboxProc"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Quick decision keys
            (define-key map (kbd "n") 'rk/process-next-action)
            (define-key map (kbd "t") 'rk/process-todo)
            (define-key map (kbd "p") 'rk/process-project)
            (define-key map (kbd "s") 'rk/process-someday)
            (define-key map (kbd "w") 'rk/process-waiting)
            (define-key map (kbd "d") 'rk/process-delete)
            (define-key map (kbd "r") 'rk/process-refile)
            ;; Navigation
            (define-key map (kbd "j") 'org-next-visible-heading)
            (define-key map (kbd "k") 'org-previous-visible-heading)
            ;; Finish
            (define-key map (kbd "q") 'rk/finish-inbox-processing)
            map)
  (when rk/inbox-processing-mode
    (rk/show-inbox-help)))

(defun rk/show-inbox-help ()
  "Show inbox processing help."
  (let ((help-window (split-window-below -10)))
    (set-window-buffer help-window 
                       (get-buffer-create "*Inbox Processing Help*"))
    (with-current-buffer "*Inbox Processing Help*"
      (erase-buffer)
      (insert "INBOX PROCESSING KEYS:\n")
      (insert "n - Next Action    t - TODO           p - Project\n")
      (insert "s - Someday/Maybe  w - Waiting For    d - Delete\n")
      (insert "r - Refile         j/k - Next/Prev    q - Quit\n"))))

(defun rk/process-next-action ()
  "Process current item as NEXT action."
  (interactive)
  (org-todo "NEXT")
  (rk/quick-tag)
  (rk/quick-schedule)
  (rk/auto-refile-by-context)
  (org-next-visible-heading 1))

(defun rk/process-todo ()
  "Process current item as TODO."
  (interactive)
  (org-todo "TODO")
  (rk/quick-tag)
  (rk/auto-refile-by-context)
  (org-next-visible-heading 1))

(defun rk/process-project ()
  "Process current item as new project."
  (interactive)
  (org-todo "TODO")
  (org-set-tags ":project:")
  (let ((context (completing-read "Context: " '("work" "personal"))))
    (org-refile nil nil 
                (list nil (rk/org-file (format "%s/projects.org" context))
                      nil nil)))
  (org-next-visible-heading 1))

(defun rk/process-someday ()
  "Process current item as someday/maybe."
  (interactive)
  (org-todo "SOMEDAY")
  (let ((context (completing-read "Context: " '("work" "personal"))))
    (org-refile nil nil 
                (list nil (rk/org-file (format "%s/someday.org" context))
                      nil nil)))
  (org-next-visible-heading 1))

(defun rk/process-waiting ()
  "Process current item as waiting for."
  (interactive)
  (org-todo "WAITING")
  (let ((waiting-for (read-string "Waiting for: ")))
    (org-set-property "WAITING_FOR" waiting-for))
  (rk/auto-refile-by-context)
  (org-next-visible-heading 1))

(defun rk/process-delete ()
  "Delete current inbox item."
  (interactive)
  (when (y-or-n-p "Delete this item? ")
    (org-cut-subtree)
    (message "Item deleted")))

(defun rk/quick-tag ()
  "Quick tag selection for common contexts."
  (let ((tag (completing-read "Tag: " 
                              '("@work" "@home" "@office" "@phone" 
                                "@computer" "@errands" "@5min" "@15min"))))
    (org-set-tags (list tag))))

(defun rk/quick-schedule ()
  "Quick scheduling options."
  (let ((when (completing-read "Schedule: " 
                               '("today" "tomorrow" "this week" 
                                 "next week" "someday"))))
    (cond
     ((string= when "today") (org-schedule nil "+0d"))
     ((string= when "tomorrow") (org-schedule nil "+1d"))
     ((string= when "this week") (org-schedule nil "+3d"))
     ((string= when "next week") (org-schedule nil "+7d"))
     ((string= when "someday") nil)))) ; Don't schedule

(defun rk/auto-refile-by-context ()
  "Automatically refile based on tags."
  (let ((tags (org-get-tags)))
    (cond
     ((member "@work" tags)
      (org-refile nil nil 
                  (list nil (rk/org-file "work/gtd.org") nil nil)))
     ((member "@personal" tags)
      (org-refile nil nil 
                  (list nil (rk/org-file "personal/gtd.org") nil nil))))))

(defun rk/finish-inbox-processing ()
  "Finish inbox processing and show summary."
  (interactive)
  (rk/inbox-processing-mode -1)
  (delete-window (get-buffer-window "*Inbox Processing Help*"))
  (let ((remaining (rk/count-inbox-items)))
    (if (= remaining 0)
        (message "Inbox Zero achieved! 🎉")
      (message "%d items remaining in inbox" remaining))))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

#### Task 5.3.2: Bulk Operations

**Status:** 📝 PLANNED

**Purpose:** Create bulk operations for efficient task management across multiple items.

**Implementation Checklist:**
- [ ] Implement bulk tag operations
- [ ] Create bulk scheduling functions
- [ ] Add bulk state changes
- [ ] Configure bulk archiving
- [ ] Build selection and filtering helpers

**Reference Implementation:**
```elisp
(defun rk/bulk-tag-add ()
  "Add tags to multiple agenda items."
  (interactive)
  (let ((tag (completing-read "Tag to add: " 
                              (mapcar 'car org-tag-alist))))
    (org-agenda-bulk-mark-regexp ".")
    (org-agenda-bulk-action
     `(lambda ()
        (org-set-tags (cons ,tag (org-get-tags)))))))

(defun rk/bulk-tag-remove ()
  "Remove tags from multiple agenda items."
  (interactive)
  (let ((tag (completing-read "Tag to remove: " 
                              (org-get-buffer-tags))))
    (org-agenda-bulk-mark-regexp ".")
    (org-agenda-bulk-action
     `(lambda ()
        (org-set-tags (remove ,tag (org-get-tags)))))))

(defun rk/bulk-schedule ()
  "Schedule multiple tasks at once."
  (interactive)
  (let ((date (org-read-date nil nil nil "Schedule to: ")))
    (org-agenda-bulk-action
     `(lambda ()
        (org-schedule nil ,date)))))

(defun rk/bulk-set-effort ()
  "Set effort for multiple tasks."
  (interactive)
  (let ((effort (completing-read "Effort: " 
                                 '("0:15" "0:30" "1:00" "2:00" 
                                   "3:00" "4:00" "5:00" "6:00" "8:00"))))
    (org-agenda-bulk-action
     `(lambda ()
        (org-set-effort ,effort)))))

(defun rk/bulk-process-by-energy ()
  "Process tasks based on current energy level."
  (interactive)
  (let ((energy (completing-read "Current energy: " 
                                 '("high" "medium" "low"))))
    (cond
     ((string= energy "high")
      (org-agenda-filter-apply '("@high_energy" "@deep") 'tag))
     ((string= energy "low")
      (org-agenda-filter-apply '("@low_energy" "@routine" "@5min") 'tag)))
    (message "Filtered tasks for %s energy" energy)))

(defun rk/bulk-defer ()
  "Defer multiple tasks to future."
  (interactive)
  (let ((days (read-number "Defer by days: " 7)))
    (org-agenda-bulk-action
     `(lambda ()
        (org-schedule nil (format "+%dd" ,days))))))

;; Smart bulk operations
(defun rk/smart-bulk-refile ()
  "Smart refile based on task properties."
  (interactive)
  (org-agenda-bulk-action
   (lambda ()
     (let ((tags (org-get-tags))
           (todo-state (org-get-todo-state)))
       (cond
        ;; Projects go to project files
        ((member "project" tags)
         (if (member "@work" tags)
             (org-refile nil nil 
                         (list nil (rk/org-file "work/projects.org") nil nil))
           (org-refile nil nil 
                       (list nil (rk/org-file "personal/projects.org") nil nil))))
        ;; Someday items
        ((string= todo-state "SOMEDAY")
         (if (member "@work" tags)
             (org-refile nil nil 
                         (list nil (rk/org-file "work/someday.org") nil nil))
           (org-refile nil nil 
                       (list nil (rk/org-file "personal/someday.org") nil nil))))
        ;; Regular tasks by context
        ((member "@work" tags)
         (org-refile nil nil 
                     (list nil (rk/org-file "work/gtd.org") nil nil)))
        ((member "@personal" tags)
         (org-refile nil nil 
                     (list nil (rk/org-file "personal/gtd.org") nil nil))))))))

;; Bulk cleanup operations
(defun rk/bulk-cleanup ()
  "Cleanup completed and old tasks."
  (interactive)
  (when (y-or-n-p "Archive all DONE tasks older than 7 days? ")
    (org-agenda-bulk-mark-regexp "DONE")
    (org-agenda-bulk-action
     (lambda ()
       (let ((closed (org-entry-get nil "CLOSED")))
         (when (and closed
                    (> (time-to-days (time-subtract 
                                      (current-time)
                                      (org-time-string-to-time closed)))
                       7))
           (org-archive-subtree)))))))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

### Phase 5.4: Advanced Enhancements

#### Task 5.4.1: Mobile Capture Integration

**Status:** 📝 PLANNED

**Purpose:** Enable capture from mobile devices through various synchronization methods.

**Implementation Checklist:**
- [ ] Configure mobile capture directory
- [ ] Create mobile inbox processing
- [ ] Set up sync detection and auto-import
- [ ] Build mobile template converters
- [ ] Add mobile-specific capture formats

**Reference Implementation:**
```elisp
;; Mobile capture configuration
(setq rk/mobile-inbox-dir "~/Dropbox/org-mobile/"
      rk/mobile-inbox-file (concat rk/mobile-inbox-dir "inbox.org"))

(defun rk/process-mobile-inbox ()
  "Process captures from mobile devices."
  (interactive)
  (when (file-exists-p rk/mobile-inbox-file)
    (let ((mobile-items 0))
      (with-temp-buffer
        (insert-file-contents rk/mobile-inbox-file)
        (goto-char (point-min))
        (while (re-search-forward "^\\* " nil t)
          (setq mobile-items (1+ mobile-items))))
      
      (if (> mobile-items 0)
          (progn
            (find-file rk/mobile-inbox-file)
            (goto-char (point-min))
            (message "Processing %d mobile captures..." mobile-items)
            (rk/mobile-processing-mode 1))
        (message "No mobile captures to process")))))

(define-minor-mode rk/mobile-processing-mode
  "Mode for processing mobile captures."
  :lighter " MobileProc"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-i") 'rk/import-mobile-item)
            (define-key map (kbd "C-c C-a") 'rk/import-all-mobile)
            (define-key map (kbd "C-c C-d") 'rk/discard-mobile-item)
            map))

(defun rk/import-mobile-item ()
  "Import current mobile item to GTD inbox."
  (interactive)
  (org-copy-subtree)
  (with-current-buffer (find-file-noselect (rk/org-file "inbox.org"))
    (goto-char (point-max))
    (org-paste-subtree)
    (save-buffer))
  (org-cut-subtree)
  (message "Imported to GTD inbox"))

(defun rk/import-all-mobile ()
  "Import all mobile items at once."
  (interactive)
  (goto-char (point-min))
  (let ((count 0))
    (while (re-search-forward "^\\* " nil t)
      (rk/import-mobile-item)
      (setq count (1+ count)))
    (message "Imported %d items" count)
    (when (= (buffer-size) 0)
      (delete-file rk/mobile-inbox-file)
      (kill-buffer))))

;; Auto-detection of new mobile captures
(defun rk/check-mobile-inbox ()
  "Check for new mobile captures."
  (when (and (file-exists-p rk/mobile-inbox-file)
             (> (nth 7 (file-attributes rk/mobile-inbox-file)) 0))
    (message "New mobile captures available! Use M-x rk/process-mobile-inbox")))

(run-with-idle-timer 300 t 'rk/check-mobile-inbox) ; Check every 5 minutes

;; Mobile-friendly capture templates
(defun rk/setup-mobile-templates ()
  "Create simplified templates for mobile capture."
  (let ((mobile-file (concat rk/mobile-inbox-dir "templates.txt")))
    (with-temp-file mobile-file
      (insert "MOBILE CAPTURE TEMPLATES\n")
      (insert "========================\n\n")
      (insert "Quick Task:\n")
      (insert "* TODO \n\n")
      (insert "Phone Call:\n")
      (insert "* TODO Call :@phone:\n\n")
      (insert "Errand:\n")
      (insert "* TODO :@errands:\n\n")
      (insert "Idea:\n")
      (insert "* Idea: \n\n"))))

;; Integration with iOS Shortcuts / Android Tasker
(defun rk/mobile-quick-capture-endpoint ()
  "Process quick capture from mobile automation."
  (let* ((title (or (getenv "CAPTURE_TITLE") "Mobile Capture"))
         (body (or (getenv "CAPTURE_BODY") ""))
         (type (or (getenv "CAPTURE_TYPE") "TODO")))
    (with-temp-file rk/mobile-inbox-file
      (when (file-exists-p rk/mobile-inbox-file)
        (insert-file-contents rk/mobile-inbox-file))
      (goto-char (point-max))
      (insert (format "* %s %s\n" type title))
      (when (not (string-empty-p body))
        (insert body "\n"))
      (insert (format "CAPTURED: [%s]\n\n" 
                      (format-time-string "%Y-%m-%d %a %H:%M"))))))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

#### Task 5.4.2: Email Integration

**Status:** 📝 PLANNED

**Purpose:** Create seamless integration between email and GTD system for task creation and reference.

**Implementation Checklist:**
- [ ] Configure email link handlers
- [ ] Create email-to-task capture
- [ ] Build email template processors
- [ ] Add email follow-up workflows
- [ ] Configure waiting-for email tracking

**Reference Implementation:**
```elisp
;; Email integration configuration
(require 'org-protocol)

;; Email link format (works with mu4e, notmuch, etc.)
(setq org-link-email-description-format "Email: %s")

(defun rk/capture-email-task ()
  "Capture task from email."
  (interactive)
  (let* ((subject (or (message-fetch-field "Subject") ""))
         (from (or (message-fetch-field "From") ""))
         (date (or (message-fetch-field "Date") ""))
         (message-id (or (message-fetch-field "Message-ID") "")))
    (org-capture nil "e")))

;; Email capture template
(add-to-list 'org-capture-templates
             '("e" "Email Task" entry (file+headline org-default-notes-file "Email")
               "* TODO %? :email:\n:PROPERTIES:\n:EMAIL_FROM: %:from\n:EMAIL_SUBJECT: %:subject\n:EMAIL_DATE: %:date\n:EMAIL_ID: %:message-id\n:END:\n\n%a"
               :prepend t))

;; Email follow-up system
(defun rk/schedule-email-followup ()
  "Schedule a follow-up for current email."
  (interactive)
  (let* ((subject (message-fetch-field "Subject"))
         (from (message-fetch-field "From"))
         (days (read-number "Follow up in days: " 3)))
    (org-capture-string
     (format "* TODO Follow up: %s :@email:followup:\nSCHEDULED: <%s>\n:PROPERTIES:\n:EMAIL_FROM: %s\n:END:\n"
             subject
             (format-time-string "%Y-%m-%d %a" 
                                 (time-add (current-time) 
                                           (days-to-time days)))
             from)
     "tw")))

(defun rk/email-to-waiting ()
  "Convert email to WAITING task."
  (interactive)
  (let* ((subject (message-fetch-field "Subject"))
         (to (message-fetch-field "To"))
         (date (format-time-string "%Y-%m-%d")))
    (org-capture-string
     (format "* WAITING %s :@email:\n:PROPERTIES:\n:WAITING_FOR: %s\n:SENT_DATE: [%s]\n:END:\n"
             subject to date)
     "tw")))

;; Batch email processing
(defun rk/process-email-batch ()
  "Process multiple emails into tasks."
  (interactive)
  (let ((emails (rk/get-marked-emails))
        (processed 0))
    (dolist (email emails)
      (with-current-buffer (get-buffer email)
        (rk/capture-email-task)
        (setq processed (1+ processed))))
    (message "Processed %d emails into tasks" processed)))

;; Email template responses linked to tasks
(defun rk/email-task-template ()
  "Generate email template based on task type."
  (let ((task-type (org-entry-get nil "TASK_TYPE")))
    (cond
     ((string= task-type "meeting-request")
      (insert "Thank you for your meeting request.\n\n")
      (insert "I have the following time slots available:\n")
      (insert "- \n- \n- \n\n")
      (insert "Please let me know which works best for you.\n"))
     
     ((string= task-type "information-request")
      (insert "Thank you for your inquiry.\n\n")
      (insert "Regarding your question about ")))))

;; Link email threads to projects
(defun rk/link-email-to-project ()
  "Link current email thread to a project."
  (interactive)
  (let* ((subject (message-fetch-field "Subject"))
         (project (completing-read "Link to project: " 
                                   (rk/get-all-projects))))
    (with-current-buffer (find-file-noselect 
                          (rk/find-project-file project))
      (goto-char (point-max))
      (insert (format "\n** Email Thread: %s\n" subject))
      (insert (format "[[%s][Email Link]]\n" (rk/get-email-link))))))

;; Smart email filing
(defun rk/file-email-by-rules ()
  "File email according to GTD rules."
  (let ((from (message-fetch-field "From"))
        (subject (message-fetch-field "Subject")))
    (cond
     ;; Waiting for replies
     ((string-match "Re: " subject)
      (when (rk/sent-by-me-p from)
        (rk/email-to-waiting)))
     ;; Action requests
     ((string-match "\\(action\\|request\\|please\\)" subject)
      (rk/capture-email-task))
     ;; FYI - file for reference
     ((string-match "\\(FYI\\|info\\)" subject)
      (rk/file-email-reference)))))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

## Implementation Process

Each task should follow this standard process:

1. **Development:**
   - Add code to `codelahoma-org.org` in appropriate sections
   - Consider creating separate sections for major features
   - Tangle the file to generate `codelahoma-org.el`
   - Reload configuration or restart Spacemacs

2. **Testing:**
   - Run through complete review workflows
   - Test time tracking accuracy
   - Verify batch operations on test data
   - Validate mobile and email integrations
   - Check performance with large datasets

3. **Documentation:**
   - Create workflow guides for each feature
   - Document keyboard shortcuts
   - Add troubleshooting sections
   - Create quick reference cards

4. **Commit:**
   - Stage changes to both .org and .el files
   - Commit with descriptive message:
     ```
     feat: implement GTD advanced features phase 5
     
     - Comprehensive review system with weekly/daily workflows
     - Time tracking with pomodoro integration
     - Batch processing for efficient task management
     - Mobile capture and email integration
     ```

## Testing Strategy

### Manual Testing Checklist:

1. **Review System:**
   - [ ] Weekly review captures all aspects
   - [ ] Daily review is quick and effective
   - [ ] Project health metrics are accurate
   - [ ] Review templates are comprehensive
   - [ ] Statistics calculate correctly

2. **Time Tracking:**
   - [ ] Clock persistence works across sessions
   - [ ] Pomodoro timers function correctly
   - [ ] Reports generate accurate data
   - [ ] Effort estimates integrate properly
   - [ ] Time summaries are meaningful

3. **Batch Processing:**
   - [ ] Inbox processing mode handles all cases
   - [ ] Bulk operations work on multiple items
   - [ ] Smart filing rules apply correctly
   - [ ] Performance is acceptable with many items
   - [ ] Undo works for bulk operations

4. **Mobile/Email:**
   - [ ] Mobile captures import correctly
   - [ ] Email links resolve properly
   - [ ] Follow-up scheduling works
   - [ ] Templates generate correctly
   - [ ] Sync detection functions

## Rollback Plan

In case of issues:

1. Disable advanced features by commenting out requires
2. Remove timer-based functions
3. Clear mobile sync directories
4. Disable email capture templates
5. Revert to basic GTD configuration

## Conclusion

This phase completes the GTD system with advanced features that transform it from a simple task manager into a comprehensive productivity system. The review workflows ensure nothing falls through the cracks, time tracking provides insights into where time is spent, batch processing makes maintenance efficient, and modern integrations ensure the system works with how people actually work today. Together with the previous phases, this creates a complete, professional-grade GTD implementation in Spacemacs.