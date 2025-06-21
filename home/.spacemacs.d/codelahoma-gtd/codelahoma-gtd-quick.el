;;; codelahoma-gtd-quick.el --- Quick access commands for GTD -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Quick access commands and shortcuts for efficient GTD workflow navigation.
;; Provides rapid status checks, instant navigation, and workflow shortcuts.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)
(require 'hydra)

;;; Quick Status Commands

(defun codelahoma-gtd-quick-status ()
  "Quick GTD status in minibuffer."
  (interactive)
  (let* ((inbox (codelahoma-gtd-count-entries "inbox.org"))
         (next (codelahoma-gtd-count-entries "next-actions.org" "NEXT"))
         (waiting (codelahoma-gtd-count-entries "waiting-for.org" "WAITING"))
         (projects (codelahoma-gtd-count-entries "projects.org" "PROJECT"))
         (overdue (codelahoma-gtd-count-overdue)))
    (message "📥 Inbox: %d | ⚡ Next: %d | ⏳ Waiting: %d | 📁 Projects: %d | ⚠️  Overdue: %d" 
             inbox next waiting projects overdue)))

(defun codelahoma-gtd-quick-today ()
  "Show quick summary of today's items."
  (interactive)
  (let* ((scheduled-today 0)
         (deadline-today 0)
         (meetings 0))
    ;; Count scheduled items
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (let ((scheduled (org-get-scheduled-time nil))
                 (deadline (org-get-deadline-time nil)))
             (when (and scheduled
                        (= (time-to-days scheduled) (time-to-days (current-time))))
               (cl-incf scheduled-today))
             (when (and deadline
                        (= (time-to-days deadline) (time-to-days (current-time))))
               (cl-incf deadline-today))
             (when (and (member (org-get-todo-state) '("MEETING" "APPOINTMENT"))
                        scheduled
                        (= (time-to-days scheduled) (time-to-days (current-time))))
               (cl-incf meetings))))
         "TODO|NEXT|MEETING|APPOINTMENT")))
    (message "Today: 📅 %d scheduled | 🎯 %d deadlines | 👥 %d meetings"
             scheduled-today deadline-today meetings)))

(defun codelahoma-gtd-quick-week-ahead ()
  "Show quick summary of the week ahead."
  (interactive)
  (let ((week-items '()))
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (let ((scheduled (org-get-scheduled-time nil))
                 (deadline (org-get-deadline-time nil)))
             (when (or scheduled deadline)
               (let ((days-until (if scheduled
                                   (- (time-to-days scheduled) (time-to-days (current-time)))
                                 (- (time-to-days deadline) (time-to-days (current-time))))))
                 (when (and (>= days-until 0) (<= days-until 7))
                   (push (format "%s: %s"
                               (if (<= days-until 1) "Soon" 
                                 (format "%dd" days-until))
                               (org-get-heading t t t t))
                         week-items))))))
         "TODO|NEXT|WAITING|PROJECT")))
    (if week-items
        (message "Week ahead:\n%s" (mapconcat 'identity (nreverse week-items) "\n"))
      (message "Nothing scheduled for the week ahead"))))

;;; Quick Capture Variants

(defun codelahoma-gtd-quick-capture-task ()
  "Ultra-quick task capture to inbox."
  (interactive)
  (let ((task (read-string "Quick task: ")))
    (when (not (string-empty-p task))
      (with-current-buffer (find-file-noselect 
                           (expand-file-name "inbox.org" codelahoma-gtd-directory))
        (goto-char (point-max))
        (insert (format "\n* TODO %s\n:PROPERTIES:\n:CAPTURED: %s\n:QUICK_CAPTURE: t\n:END:\n"
                       task (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (save-buffer))
      (message "✓ Captured: %s" task))))

(defun codelahoma-gtd-quick-note ()
  "Quick note capture."
  (interactive)
  (let ((note (read-string "Quick note: ")))
    (when (not (string-empty-p note))
      (with-current-buffer (find-file-noselect
                           (expand-file-name "inbox.org" codelahoma-gtd-directory))
        (goto-char (point-max))
        (insert (format "\n* %s :NOTE:\n:PROPERTIES:\n:CAPTURED: %s\n:END:\n"
                       note (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (save-buffer))
      (message "✓ Note captured"))))

(defun codelahoma-gtd-quick-waiting-for ()
  "Quick waiting-for item."
  (interactive)
  (let ((item (read-string "Waiting for: "))
        (person (read-string "From whom: ")))
    (when (not (string-empty-p item))
      (with-current-buffer (find-file-noselect
                           (expand-file-name "waiting-for.org" codelahoma-gtd-directory))
        (goto-char (point-max))
        (insert (format "\n* WAITING %s\n:PROPERTIES:\n:WAITING_ON: %s\n:WAITING_SINCE: %s\n:END:\n"
                       item person (format-time-string "[%Y-%m-%d %a]")))
        (save-buffer))
      (message "✓ Added to waiting list: %s from %s" item person))))

(defun codelahoma-gtd-quick-calendar-event ()
  "Quick calendar event capture."
  (interactive)
  (let ((event (read-string "Event: "))
        (date (org-read-date nil nil nil "Date: ")))
    (when (not (string-empty-p event))
      (with-current-buffer (find-file-noselect
                           (expand-file-name "calendar.org" codelahoma-gtd-directory))
        (goto-char (point-max))
        (insert (format "\n* %s\nSCHEDULED: <%s>\n:PROPERTIES:\n:CAPTURED: %s\n:END:\n"
                       event date (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (save-buffer))
      (message "✓ Event scheduled: %s on %s" event date))))

;;; Quick Navigation

(defhydra codelahoma-gtd-quick-nav (:color blue :hint nil)
  "
GTD Quick Navigation
--------------------
_i_: Inbox      _p_: Projects   _n_: Next Actions
_w_: Waiting    _s_: Someday    _c_: Calendar
_a_: Areas      _r_: Reference  _m_: Media

_d_: Daily Review   _k_: Weekly Review   _?_: Help

_q_: Quit
"
  ("i" codelahoma-gtd-open-inbox)
  ("p" codelahoma-gtd-open-projects)
  ("n" codelahoma-gtd-open-next-actions)
  ("w" codelahoma-gtd-open-waiting-for)
  ("s" codelahoma-gtd-open-someday)
  ("c" codelahoma-gtd-open-calendar)
  ("a" (find-file (expand-file-name "areas" codelahoma-gtd-directory)))
  ("r" (find-file (expand-file-name "reference.org" codelahoma-gtd-directory)))
  ("m" codelahoma-gtd-open-media)
  ("d" codelahoma-gtd-daily-review)
  ("k" codelahoma-gtd-weekly-review)
  ("?" codelahoma-gtd-help)
  ("q" nil))

;;; Context Switching

(defun codelahoma-gtd-quick-switch-context ()
  "Quick context switch."
  (interactive)
  (when (featurep 'codelahoma-gtd-contexts)
    (let ((context (completing-read "Switch to context: " 
                                   (mapcar #'car codelahoma-gtd-contexts))))
      (setq codelahoma-gtd-current-context context)
      (message "Context switched to: %s" context)
      ;; Optionally refresh agenda or current view
      (when (eq major-mode 'org-agenda-mode)
        (org-agenda-redo)))))

(defun codelahoma-gtd-quick-energy-level ()
  "Quick energy level adjustment."
  (interactive)
  (when (featurep 'codelahoma-gtd-contexts)
    (let ((energy (completing-read "Current energy level: " 
                                  '("high" "medium" "low"))))
      (setq codelahoma-gtd-current-energy energy)
      (message "Energy level set to: %s" energy))))

;;; Workflow Shortcuts

(defun codelahoma-gtd-quick-process-inbox ()
  "Quick process first inbox item."
  (interactive)
  (codelahoma-gtd-open-inbox)
  (goto-char (point-min))
  (when (org-next-visible-heading 1)
    (codelahoma-gtd-decision-tree)))

(defun codelahoma-gtd-quick-complete-and-next ()
  "Mark current task done and suggest next."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-todo "DONE")
    (save-buffer)
    (codelahoma-gtd-smart-next-action)))

(defun codelahoma-gtd-quick-delegate ()
  "Quick delegate current task."
  (interactive)
  (when (eq major-mode 'org-mode)
    (codelahoma-gtd-delegate-task)))

(defun codelahoma-gtd-quick-defer ()
  "Quick defer current task."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-todo "HOLD")
    (org-schedule nil "+1w")
    (message "Task deferred for 1 week")))

;;; Quick Jumps

(defun codelahoma-gtd-jump-to-project ()
  "Quick jump to a project."
  (interactive)
  (let ((projects (codelahoma-gtd-get-all-projects)))
    (if projects
        (let* ((titles (mapcar (lambda (p) (plist-get p :title)) projects))
               (selected (completing-read "Jump to project: " titles)))
          (find-file (expand-file-name "projects.org" codelahoma-gtd-directory))
          (goto-char (point-min))
          (re-search-forward (concat "^\\* PROJECT " (regexp-quote selected)) nil t)
          (org-show-entry))
      (message "No projects found"))))

(defun codelahoma-gtd-jump-to-context-tasks ()
  "Jump to tasks for a specific context."
  (interactive)
  (when (featurep 'codelahoma-gtd-contexts)
    (let ((context (completing-read "Context: " 
                                   (mapcar #'car codelahoma-gtd-contexts))))
      (org-tags-view nil (format "+%s" context)))))

;;; Quick Reviews

(defun codelahoma-gtd-quick-inbox-count ()
  "Show inbox count with option to process."
  (interactive)
  (let ((count (codelahoma-gtd-count-entries "inbox.org")))
    (if (> count 0)
        (when (y-or-n-p (format "%d items in inbox. Process now? " count))
          (codelahoma-gtd-process-inbox))
      (message "Inbox is empty! 🎉"))))

(defun codelahoma-gtd-quick-stalled-projects ()
  "Quick check for stalled projects."
  (interactive)
  (let ((stalled-count 0))
    (with-current-buffer (find-file-noselect 
                         (expand-file-name "projects.org" codelahoma-gtd-directory))
      (org-map-entries
       (lambda ()
         (let ((has-next nil))
           (save-excursion
             (org-narrow-to-subtree)
             (goto-char (point-min))
             (setq has-next (re-search-forward "^\\*+ NEXT " nil t))
             (widen))
           (unless has-next
             (cl-incf stalled-count))))
       "PROJECT"))
    (if (> stalled-count 0)
        (when (y-or-n-p (format "%d stalled projects. Review now? " stalled-count))
          (codelahoma-gtd-find-stalled-projects))
      (message "No stalled projects! All projects have next actions."))))

;;; Dashboard

(defun codelahoma-gtd-quick-dashboard ()
  "Display quick GTD dashboard in minibuffer."
  (interactive)
  (let* ((inbox (codelahoma-gtd-count-entries "inbox.org"))
         (next (codelahoma-gtd-count-entries "next-actions.org" "NEXT"))
         (active-projects (length (seq-filter 
                                 (lambda (p) (string= (plist-get p :status) "Active"))
                                 (codelahoma-gtd-get-all-projects))))
         (today-items (codelahoma-gtd-count-today-items)))
    (message "GTD Dashboard | Inbox: %d | Next: %d | Projects: %d | Today: %d | %s"
             inbox next active-projects today-items
             (format-time-string "%A, %B %d"))))

(defun codelahoma-gtd-count-today-items ()
  "Count items scheduled for today."
  (let ((count 0))
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (let ((scheduled (org-get-scheduled-time nil)))
             (when (and scheduled
                        (= (time-to-days scheduled) (time-to-days (current-time))))
               (cl-incf count))))
         "TODO|NEXT")))
    count))

(provide 'codelahoma-gtd-quick)
;;; codelahoma-gtd-quick.el ends here