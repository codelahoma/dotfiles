;;; codelahoma-gtd-daily-review.el --- Daily review workflows -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Daily review workflows for morning planning and evening reflection.
;; Provides quick, efficient routines to maintain GTD system integrity.

;;; Code:

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

(defcustom codelahoma-gtd-review-directory
  (expand-file-name "reviews" codelahoma-gtd-directory)
  "Directory to store review history."
  :type 'directory
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
      (insert (format "- Waiting for: %d\n"
                     (codelahoma-gtd-count-entries "waiting-for.org" "WAITING")))
      
      ;; Daily focus
      (insert "\n* Today's Focus (Pick 1-3)\n")
      (insert "1. [ ] \n2. [ ] \n3. [ ] \n")
      
      ;; Time estimate
      (insert (format "\n* Review completed in %.1f minutes\n"
                     (/ (float-time (time-subtract (current-time) start-time)) 60))))
    
    (switch-to-buffer review-buffer)
    (goto-char (point-min))
    (re-search-forward "Today's Focus" nil t)
    (forward-line 1)
    (end-of-line)))

(defun codelahoma-gtd-morning-check-calendar ()
  "Check today's calendar items."
  (insert "* Today's Schedule\n")
  (let ((calendar-file (expand-file-name "calendar.org" codelahoma-gtd-directory)))
    (if (file-exists-p calendar-file)
        (let ((entries (org-agenda-get-day-entries 
                       calendar-file
                       (calendar-current-date))))
          (if entries
              (dolist (entry entries)
                (when (stringp entry)
                  (insert (format "- %s\n" entry))))
            (insert "- No scheduled items\n")))
      (insert "- Calendar file not found\n"))))

(defun codelahoma-gtd-morning-review-priorities ()
  "Review and set daily priorities."
  (insert "\n* Priority Review\n")
  ;; Get high priority items
  (let ((high-priority-tasks (codelahoma-gtd-get-priority-tasks ?A)))
    (if high-priority-tasks
        (progn
          (insert "High priority tasks:\n")
          (dolist (task (seq-take high-priority-tasks 5))
            (insert (format "- [ ] %s\n" (plist-get task :title)))))
      (insert "No high priority tasks\n"))))

(defun codelahoma-gtd-morning-check-energy ()
  "Set energy level for the day."
  (insert "\n* Energy Check\n")
  (insert "Current energy level: ")
  (insert "[ ] High  [ ] Medium  [ ] Low\n")
  (when (featurep 'codelahoma-gtd-contexts)
    (insert "\nPreferred contexts for today:\n")
    (insert "[ ] @computer  [ ] @home  [ ] @office  [ ] @errands  [ ] @phone\n")))

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
      (insert "- [ ] \n- [ ] \n- [ ] \n\n")
      
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
      (insert (format "\n* Review completed in %.1f minutes\n"
                     (/ (float-time (time-subtract (current-time) start-time)) 60))))
    
    (switch-to-buffer review-buffer)
    (goto-char (point-min))
    (search-forward "Capture Loose Ends" nil t)
    (forward-line 2)
    (end-of-line)))

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
  (insert "1. [ ] \n2. [ ] \n3. [ ] \n")
  
  ;; Check tomorrow's calendar
  (let ((calendar-file (expand-file-name "calendar.org" codelahoma-gtd-directory)))
    (when (file-exists-p calendar-file)
      (let* ((tomorrow (calendar-gregorian-from-absolute 
                       (1+ (calendar-absolute-from-gregorian (calendar-current-date)))))
             (entries (org-agenda-get-day-entries calendar-file tomorrow)))
        (when entries
          (insert "\nScheduled for tomorrow:\n")
          (dolist (entry entries)
            (when (stringp entry)
              (insert (format "- %s\n" entry)))))))))

;;; Review History

(defun codelahoma-gtd-save-daily-review (buffer review-type)
  "Save daily review BUFFER to history with REVIEW-TYPE."
  (let* ((date (format-time-string "%Y-%m-%d"))
         (filename (format "%s-%s-review.org" date review-type))
         (filepath (expand-file-name filename codelahoma-gtd-review-directory)))
    (unless (file-exists-p codelahoma-gtd-review-directory)
      (make-directory codelahoma-gtd-review-directory t))
    (with-current-buffer buffer
      (let ((content (buffer-string)))
        (with-temp-file filepath
          (insert content))))
    (message "Review saved to %s" filename)))

(defun codelahoma-gtd-save-morning-review ()
  "Save current morning review."
  (interactive)
  (when (get-buffer "*GTD Morning Review*")
    (codelahoma-gtd-save-daily-review (get-buffer "*GTD Morning Review*") "morning")))

(defun codelahoma-gtd-save-evening-review ()
  "Save current evening review."
  (interactive)
  (when (get-buffer "*GTD Evening Review*")
    (codelahoma-gtd-save-daily-review (get-buffer "*GTD Evening Review*") "evening")))

;;; Quick Daily Checks

(defun codelahoma-gtd-quick-morning-check ()
  "Ultra-quick morning check."
  (interactive)
  (let ((cal-items (codelahoma-gtd-count-today-scheduled))
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
    (setf (alist-get date codelahoma-gtd-daily-metrics nil nil 'equal) (cdr date-metrics))))

(defun codelahoma-gtd-daily-metrics-summary ()
  "Show summary of daily metrics."
  (interactive)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (metrics (alist-get today codelahoma-gtd-daily-metrics nil nil 'equal)))
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
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\*+ DONE" nil t)
            (let ((closed-time (org-entry-get nil "CLOSED")))
              (when (and closed-time (string-match today closed-time))
                (push (list :title (org-get-heading t t t t)
                           :file file)
                      completed)))))))
    (nreverse completed)))

(defun codelahoma-gtd-get-priority-tasks (priority)
  "Get tasks with specific PRIORITY."
  (let ((tasks '()))
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\*+ \\(TODO\\|NEXT\\)" nil t)
            (when (= (or (org-get-priority (org-get-heading)) org-priority-default) priority)
              (push (list :title (org-get-heading t t t t)
                         :priority priority
                         :file file)
                    tasks))))))
    (nreverse tasks)))

(defun codelahoma-gtd-count-today-scheduled ()
  "Count items scheduled for today."
  (let ((count 0)
        (calendar-file (expand-file-name "calendar.org" codelahoma-gtd-directory)))
    (when (file-exists-p calendar-file)
      (let ((entries (org-agenda-get-day-entries 
                     calendar-file
                     (calendar-current-date))))
        (setq count (length entries))))
    count))

;;; Review Journal Integration

(defun codelahoma-gtd-daily-journal-entry ()
  "Create daily journal entry linked to reviews."
  (interactive)
  (let* ((date (format-time-string "%Y-%m-%d"))
         (journal-file (expand-file-name 
                       (format "journal/%s.org" date)
                       codelahoma-gtd-directory))
         (journal-dir (file-name-directory journal-file)))
    (unless (file-exists-p journal-dir)
      (make-directory journal-dir t))
    (find-file journal-file)
    (when (= (buffer-size) 0)
      (insert "#+TITLE: Journal - " (format-time-string "%A, %B %d, %Y") "\n\n")
      (insert "* Morning Review\n")
      (insert (format "[[file:../reviews/%s-morning-review.org][Morning Review]]\n\n" date))
      (insert "* Day Log\n\n")
      (insert "* Evening Review\n")
      (insert (format "[[file:../reviews/%s-evening-review.org][Evening Review]]\n" date)))))

(provide 'codelahoma-gtd-daily-review)
;;; codelahoma-gtd-daily-review.el ends here