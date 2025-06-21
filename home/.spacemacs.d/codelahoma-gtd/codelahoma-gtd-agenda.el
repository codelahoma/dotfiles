;;; codelahoma-gtd-agenda.el --- Custom GTD agenda views -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Custom agenda views for GTD workflow that match daily patterns and provide
;; quick access to relevant information.

;;; Code:

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

(defun codelahoma-gtd-review-completed-today ()
  "Review tasks completed today."
  (interactive)
  (let ((org-agenda-files (codelahoma-gtd-agenda-files)))
    (org-tags-view nil "TODO=\"DONE\"+CLOSED>=\"<today>\"")))

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
          (search category-keep)))
  
  ;; Custom agenda commands
  (setq org-agenda-custom-commands
        '(("g" "GTD Views"
           ((agenda "" ((org-agenda-span 'day)))
            (todo "NEXT" ((org-agenda-overriding-header "Next Actions")))
            (todo "WAITING" ((org-agenda-overriding-header "Waiting For")))
            (todo "PROJECT" ((org-agenda-overriding-header "Active Projects")))))
          ("d" "Daily Dashboard" codelahoma-gtd-daily-dashboard)
          ("w" "Weekly Planning" codelahoma-gtd-weekly-planning)
          ("c" "By Context" codelahoma-gtd-agenda-by-context)
          ("e" "By Energy" codelahoma-gtd-agenda-by-energy))))

;;; Morning Review

(defun codelahoma-gtd-morning-review ()
  "Morning review routine."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  
  ;; Left: Today's agenda
  (org-agenda nil "a")
  
  ;; Right: Review checklist
  (other-window 1)
  (switch-to-buffer (get-buffer-create "*Morning Review*"))
  (erase-buffer)
  (insert "Morning Review Checklist\n")
  (insert "========================\n\n")
  (insert "[ ] Review calendar for today\n")
  (insert "[ ] Check weather and dress accordingly\n")
  (insert "[ ] Review next actions\n")
  (insert "[ ] Choose 3 MITs (Most Important Tasks)\n")
  (insert "[ ] Set energy level for the day\n")
  (insert "[ ] Clear desk and workspace\n\n")
  (insert "Press 'q' to close this buffer")
  (read-only-mode 1))

(provide 'codelahoma-gtd-agenda)
;;; codelahoma-gtd-agenda.el ends here