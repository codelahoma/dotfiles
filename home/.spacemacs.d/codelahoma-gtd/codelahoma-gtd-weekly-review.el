;;; codelahoma-gtd-weekly-review.el --- Weekly review system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Comprehensive weekly GTD review process with guided workflow and progress tracking.
;; Implements the standard GTD weekly review with Get Clear, Get Current, Get Creative phases.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)
(require 'codelahoma-gtd-daily-review)

;;; Weekly Review Configuration

(defcustom codelahoma-gtd-weekly-review-day 5
  "Day of week for weekly review (0=Sunday, 5=Friday)."
  :type 'integer
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-weekly-review-steps
  '((:name "Collect Loose Papers and Materials" :phase "clear" :time 5)
    (:name "Get Inbox to Zero" :phase "clear" :time 30)
    (:name "Empty Your Head" :phase "clear" :time 10)
    (:name "Review Action Lists" :phase "current" :time 15)
    (:name "Review Previous Calendar" :phase "current" :time 5)
    (:name "Review Upcoming Calendar" :phase "current" :time 5)
    (:name "Review Waiting For List" :phase "current" :time 10)
    (:name "Review Project List" :phase "current" :time 30)
    (:name "Review Someday/Maybe List" :phase "creative" :time 15)
    (:name "Review Goals and Objectives" :phase "creative" :time 10))
  "Steps in weekly review with estimated times."
  :type '(repeat (plist :name string :phase string :time integer))
  :group 'codelahoma-gtd)

;;; Weekly Review Interface

(defvar codelahoma-gtd-weekly-review-buffer "*GTD Weekly Review*"
  "Buffer name for weekly review.")

(defvar codelahoma-gtd-weekly-review-current-step 0
  "Current step in weekly review.")

(defvar codelahoma-gtd-weekly-review-start-time nil
  "Start time of current weekly review.")

(defvar codelahoma-gtd-weekly-review-step-times nil
  "Time tracking for each step.")

(defun codelahoma-gtd-weekly-review ()
  "Start guided weekly review process."
  (interactive)
  (setq codelahoma-gtd-weekly-review-current-step 0)
  (setq codelahoma-gtd-weekly-review-start-time (current-time))
  (setq codelahoma-gtd-weekly-review-step-times nil)
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
      
      ;; Phase indicator
      (let ((phase (plist-get step :phase)))
        (insert (format "* Phase: %s\n" 
                       (cond
                        ((string= phase "clear") "Get Clear")
                        ((string= phase "current") "Get Current")
                        ((string= phase "creative") "Get Creative")
                        (t phase)))))
      
      ;; Current step
      (insert (format "\n* Current Step: %s\n" (plist-get step :name)))
      (insert (format "Estimated time: %d minutes\n\n" (plist-get step :time)))
      
      ;; Step-specific content
      (codelahoma-gtd-weekly-review-step-content step)
      
      ;; Navigation
      (insert "\n\n")
      (when (> codelahoma-gtd-weekly-review-current-step 0)
        (insert-button "[Previous]" 
                      'action (lambda (_) (codelahoma-gtd-weekly-review-previous)))
        (insert "  "))
      (insert-button "[Complete Step]" 
                    'action (lambda (_) (codelahoma-gtd-weekly-review-next)))
      (insert "  ")
      (insert-button "[Skip]" 
                    'action (lambda (_) (codelahoma-gtd-weekly-review-skip)))
      (insert "  ")
      (insert-button "[Cancel Review]" 
                    'action (lambda (_) (codelahoma-gtd-weekly-review-cancel)))
      
      (read-only-mode 1))
    (switch-to-buffer buffer)))

(defun codelahoma-gtd-weekly-review-step-content (step)
  "Generate content for review STEP."
  (let ((step-name (plist-get step :name)))
    (cond
     ((string-match-p "Collect Loose" step-name)
      (codelahoma-gtd-review-collect-content))
     ((string-match-p "Inbox to Zero" step-name)
      (codelahoma-gtd-review-inbox-content))
     ((string-match-p "Empty Your Head" step-name)
      (codelahoma-gtd-review-mindsweep-content))
     ((string-match-p "Review Action Lists" step-name)
      (codelahoma-gtd-review-actions-content))
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

(defun codelahoma-gtd-review-collect-content ()
  "Generate collect loose papers content."
  (insert "Gather all loose papers, notes, business cards, and materials.\n\n")
  (insert "Check these locations:\n")
  (insert "- [ ] Physical inbox/trays\n")
  (insert "- [ ] Desk and workspace\n")
  (insert "- [ ] Briefcase/bag\n")
  (insert "- [ ] Car\n")
  (insert "- [ ] Pockets\n")
  (insert "- [ ] Notes apps on phone\n\n")
  (insert "Put everything in your physical inbox or capture digitally."))

(defun codelahoma-gtd-review-inbox-content ()
  "Generate inbox review content."
  (let ((inbox-count (codelahoma-gtd-count-entries "inbox.org")))
    (insert (format "Current inbox items: %d\n\n" inbox-count))
    (if (> inbox-count 0)
        (progn
          (insert "Process each item:\n")
          (insert "1. What is it?\n")
          (insert "2. Is it actionable?\n")
          (insert "3. What's the next action?\n")
          (insert "4. Will it take less than 2 minutes?\n\n")
          (insert-button "[Open Inbox]" 
                        'action (lambda (_) (codelahoma-gtd-open-inbox)))
          (insert "  ")
          (insert-button "[Process Inbox]" 
                        'action (lambda (_) (codelahoma-gtd-process-inbox))))
      (insert "✓ Inbox is empty!"))))

(defun codelahoma-gtd-review-mindsweep-content ()
  "Generate mind sweep content."
  (insert "Capture anything on your mind:\n\n")
  (insert "Think about:\n")
  (insert "- [ ] Upcoming events or deadlines\n")
  (insert "- [ ] Projects that need attention\n")
  (insert "- [ ] Promises or commitments made\n")
  (insert "- [ ] Ideas you've been considering\n")
  (insert "- [ ] Things you're worried about\n")
  (insert "- [ ] Items you've been putting off\n\n")
  (insert-button "[Quick Capture]" 
                'action (lambda (_) 
                          (when (featurep 'codelahoma-gtd-quick)
                            (codelahoma-gtd-quick-capture-task))))
  (insert "\n\nCapture items:\n")
  (dotimes (i 5)
    (insert (format "%d. [ ] \n" (1+ i)))))

(defun codelahoma-gtd-review-actions-content ()
  "Generate action lists review content."
  (insert "Review all next actions:\n")
  (insert "- Mark complete if done\n")
  (insert "- Update if changed\n")
  (insert "- Ensure each has a clear next physical action\n\n")
  (let ((next-count (codelahoma-gtd-count-entries "next-actions.org" "NEXT"))
        (todo-count (codelahoma-gtd-count-entries "next-actions.org" "TODO")))
    (insert (format "Current status:\n"))
    (insert (format "- NEXT actions: %d\n" next-count))
    (insert (format "- TODO items: %d\n\n" todo-count)))
  (insert-button "[Review Next Actions]" 
                'action (lambda (_) (codelahoma-gtd-open-next-actions)))
  (insert "  ")
  (when (featurep 'codelahoma-gtd-contexts)
    (insert-button "[By Context]" 
                  'action (lambda (_) (codelahoma-gtd-agenda-by-context)))))

(defun codelahoma-gtd-review-past-calendar ()
  "Review past calendar entries."
  (insert "Review past calendar (last 2 weeks):\n\n")
  (insert "Look for:\n")
  (insert "- [ ] Meetings that need follow-up\n")
  (insert "- [ ] Commitments made\n")
  (insert "- [ ] Action items from events\n\n")
  
  (let ((two-weeks-ago (time-subtract (current-time) (days-to-time 14))))
    (insert "Past events:\n")
    (codelahoma-gtd-show-calendar-range two-weeks-ago (current-time))))

(defun codelahoma-gtd-review-future-calendar ()
  "Review upcoming calendar entries."
  (insert "Review upcoming calendar (next 4 weeks):\n\n")
  (insert "Check for:\n")
  (insert "- [ ] Events that need preparation\n")
  (insert "- [ ] Travel arrangements needed\n")
  (insert "- [ ] Agenda items to prepare\n")
  (insert "- [ ] Gifts or cards to buy\n\n")
  
  (let ((four-weeks-later (time-add (current-time) (days-to-time 28))))
    (insert "Upcoming events:\n")
    (codelahoma-gtd-show-calendar-range (current-time) four-weeks-later)))

(defun codelahoma-gtd-review-waiting-for-content ()
  "Generate waiting for review content."
  (let ((waiting-count (codelahoma-gtd-count-entries "waiting-for.org" "WAITING")))
    (insert (format "Items waiting for: %d\n\n" waiting-count))
    (insert "For each item check:\n")
    (insert "- [ ] Still needed?\n")
    (insert "- [ ] Follow up required?\n")
    (insert "- [ ] Can be moved to active?\n\n"))
  (insert-button "[Review Waiting For]" 
                'action (lambda (_) (codelahoma-gtd-open-waiting-for))))

(defun codelahoma-gtd-review-projects-content ()
  "Generate project review content."
  (insert "Review each project:\n")
  (insert "- [ ] Outcome still desired?\n")
  (insert "- [ ] Next action identified?\n")
  (insert "- [ ] Status accurate?\n\n")
  
  (let ((projects (codelahoma-gtd-get-all-projects))
        (active 0)
        (stalled 0))
    (dolist (project projects)
      (if (codelahoma-gtd-project-has-next-action-p project)
          (cl-incf active)
        (cl-incf stalled)))
    
    (insert (format "Project Status:\n"))
    (insert (format "- Total projects: %d\n" (length projects)))
    (insert (format "- Active (have next actions): %d\n" active))
    (when (> stalled 0)
      (insert (format "- ⚠️  Stalled (no next action): %d\n" stalled)))
    (insert "\n"))
  
  (insert-button "[Review Projects]" 
                'action (lambda (_) (codelahoma-gtd-open-projects)))
  (insert "  ")
  (insert-button "[Find Stalled]" 
                'action (lambda (_) (codelahoma-gtd-find-stalled-projects))))

(defun codelahoma-gtd-review-someday-content ()
  "Generate someday/maybe review content."
  (let ((someday-count (codelahoma-gtd-count-entries "someday.org")))
    (insert (format "Someday/Maybe items: %d\n\n" someday-count))
    (insert "Review for:\n")
    (insert "- [ ] Items to activate now\n")
    (insert "- [ ] Items no longer interested in\n")
    (insert "- [ ] New someday/maybe items\n\n"))
  (insert-button "[Review Someday/Maybe]" 
                'action (lambda (_) (codelahoma-gtd-open-someday)))
  (insert "  ")
  (when (featurep 'codelahoma-gtd-agenda)
    (insert-button "[Activate Items]" 
                  'action (lambda (_) (codelahoma-gtd-someday-review)))))

(defun codelahoma-gtd-review-goals-content ()
  "Generate goals review content."
  (insert "Review your higher horizons:\n\n")
  (insert "* Areas of Focus\n")
  (when (boundp 'codelahoma-gtd-areas-of-focus)
    (dolist (area codelahoma-gtd-areas-of-focus)
      (insert (format "- [ ] %s - On track?\n" area))))
  (insert "\n* Goals (1-2 years)\n")
  (insert "- [ ] Progress on key goals?\n")
  (insert "- [ ] Any goals to add/remove?\n\n")
  (insert "* Vision (3-5 years)\n")
  (insert "- [ ] Still aligned with vision?\n")
  (insert "- [ ] Course corrections needed?\n"))

;;; Review Navigation

(defun codelahoma-gtd-weekly-review-next ()
  "Move to next step in weekly review."
  (interactive)
  ;; Record time for current step
  (push (cons codelahoma-gtd-weekly-review-current-step 
             (float-time (time-subtract (current-time) 
                                       (or (cdr (assoc codelahoma-gtd-weekly-review-current-step
                                                      codelahoma-gtd-weekly-review-step-times))
                                          codelahoma-gtd-weekly-review-start-time))))
        codelahoma-gtd-weekly-review-step-times)
  
  (if (< codelahoma-gtd-weekly-review-current-step 
        (1- (length codelahoma-gtd-weekly-review-steps)))
      (progn
        (cl-incf codelahoma-gtd-weekly-review-current-step)
        (codelahoma-gtd-weekly-review-show-step))
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

(defun codelahoma-gtd-weekly-review-cancel ()
  "Cancel weekly review."
  (interactive)
  (when (y-or-n-p "Cancel weekly review? Progress will be lost. ")
    (kill-buffer codelahoma-gtd-weekly-review-buffer)))

(defun codelahoma-gtd-weekly-review-complete ()
  "Complete weekly review."
  (let ((duration (float-time (time-subtract (current-time) 
                                            codelahoma-gtd-weekly-review-start-time))))
    (with-current-buffer (get-buffer-create codelahoma-gtd-weekly-review-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Weekly Review Complete!\n\n")
      (insert (format "* Review completed in %.1f minutes\n\n" (/ duration 60)))
      
      ;; Time breakdown
      (insert "* Time Breakdown\n")
      (dolist (step codelahoma-gtd-weekly-review-steps)
        (let* ((idx (cl-position step codelahoma-gtd-weekly-review-steps))
               (time-spent (cdr (assoc idx codelahoma-gtd-weekly-review-step-times))))
          (when time-spent
            (insert (format "- %s: %.1f min\n" 
                           (plist-get step :name)
                           (/ time-spent 60))))))
      
      (insert "\n* Summary\n")
      (codelahoma-gtd-weekly-review-summary)
      
      (insert "\n* Next Week's Focus\n")
      (insert "Top 3 priorities:\n")
      (insert "1. [ ] \n2. [ ] \n3. [ ] \n")
      
      (insert "\n* Insights & Reflections\n")
      (insert "Key insight from this week:\n- \n\n")
      (insert "What to improve next week:\n- \n")
      
      (read-only-mode 1))
    
    ;; Save and record review
    (codelahoma-gtd-save-weekly-review)
    (when (featurep 'codelahoma-gtd-analytics)
      (codelahoma-gtd-record-review 'weekly))))

;;; Weekly Metrics

(defun codelahoma-gtd-weekly-review-summary ()
  "Generate weekly summary."
  (let* ((completed-this-week (codelahoma-gtd-count-completed-this-week))
         (created-this-week (codelahoma-gtd-count-created-this-week))
         (inbox-processed (- created-this-week 
                           (codelahoma-gtd-count-entries "inbox.org"))))
    (insert (format "- Tasks completed: %d\n" completed-this-week))
    (insert (format "- Tasks created: %d\n" created-this-week))
    (insert (format "- Inbox items processed: ~%d\n" inbox-processed))
    (insert (format "- Active projects: %d\n" 
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
      (let ((content (buffer-string)))
        (with-temp-file filepath
          (insert content))))
    (message "Weekly review saved to %s" filename)))

;;; Utility Functions

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
  (let ((project-file (expand-file-name "projects.org" codelahoma-gtd-directory)))
    (when (file-exists-p project-file)
      (with-current-buffer (find-file-noselect project-file)
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward (concat "^\\* PROJECT " 
                                         (regexp-quote (plist-get project :title))) 
                                  nil t)
            (let ((project-end (save-excursion 
                                (org-end-of-subtree t)
                                (point))))
              (re-search-forward "^\\*+ NEXT " project-end t))))))))

(defun codelahoma-gtd-show-calendar-range (start-time end-time)
  "Show calendar entries between START-TIME and END-TIME."
  (let ((calendar-file (expand-file-name "calendar.org" codelahoma-gtd-directory)))
    (when (file-exists-p calendar-file)
      (let ((entries '()))
        ;; Simplified calendar display
        (insert "See calendar for detailed schedule.\n")))))

(defun codelahoma-gtd-count-completed-this-week ()
  "Count tasks completed this week."
  (let ((week-start (- (time-to-days (current-time))
                      (mod (- (time-to-days (current-time)) 4) 7))) ; Monday start
        (count 0))
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\*+ DONE" nil t)
            (let ((closed-time (org-entry-get nil "CLOSED")))
              (when (and closed-time
                        (>= (time-to-days (org-time-string-to-time closed-time))
                           week-start))
                (cl-incf count)))))))
    count))

(defun codelahoma-gtd-count-created-this-week ()
  "Count tasks created this week."
  (let ((week-start (- (time-to-days (current-time))
                      (mod (- (time-to-days (current-time)) 4) 7))) ; Monday start
        (count 0))
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\*+ \\(TODO\\|NEXT\\|DONE\\)" nil t)
            (let ((created (or (org-entry-get nil "CREATED")
                             (org-entry-get nil "CAPTURED"))))
              (when (and created
                        (>= (time-to-days (org-time-string-to-time created))
                           week-start))
                (cl-incf count)))))))
    count))

;;; Quick Weekly Check

(defun codelahoma-gtd-quick-weekly-check ()
  "Quick weekly review status check."
  (interactive)
  (let* ((last-review (codelahoma-gtd-last-weekly-review-date))
         (days-since (if last-review
                        (- (time-to-days (current-time))
                          (time-to-days last-review))
                      999)))
    (if (> days-since 7)
        (message "⚠️  Weekly review overdue by %d days! Use: SPC o o r w" 
                (- days-since 7))
      (message "✓ Weekly review done %d days ago" days-since))))

(defun codelahoma-gtd-last-weekly-review-date ()
  "Get date of last weekly review."
  (let ((reviews (directory-files codelahoma-gtd-review-directory nil 
                                 ".*-weekly-review\\.org$")))
    (when reviews
      (let ((latest (car (last (sort reviews 'string<)))))
        (when (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" latest)
          (date-to-time (match-string 1 latest)))))))

(provide 'codelahoma-gtd-weekly-review)
;;; codelahoma-gtd-weekly-review.el ends here