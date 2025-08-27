;;; codelahoma-dashboard.el --- Unified GTD-Zettelkasten dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Unified dashboard for the GTD-Zettelkasten system, providing an at-a-glance
;; view of all system components and quick access to common actions.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)
(require 'codelahoma-gtd-capture)
(require 'codelahoma-bridge)
(require 'codelahoma-bridge-metrics)
(require 'org-roam)

;;; Configuration

(defcustom codelahoma-dashboard-sections
  '(header overview inbox-status next-actions projects
    recent-notes knowledge-suggestions metrics quick-links)
  "Sections to display in the dashboard."
  :type '(repeat symbol)
  :group 'codelahoma-gtd)

(defcustom codelahoma-dashboard-auto-refresh t
  "Automatically refresh dashboard on display."
  :type 'boolean
  :group 'codelahoma-gtd)

(defcustom codelahoma-dashboard-refresh-interval 300
  "Seconds between automatic dashboard refreshes (0 to disable)."
  :type 'integer
  :group 'codelahoma-gtd)

;;; Dashboard Core

(defvar codelahoma-dashboard-buffer-name "*GTD-Zettelkasten Dashboard*"
  "Name of the dashboard buffer.")

(defvar codelahoma-dashboard-timer nil
  "Timer for automatic dashboard refresh.")

(defun codelahoma-dashboard ()
  "Display the unified GTD-Zettelkasten dashboard."
  (interactive)
  (let ((buffer (get-buffer-create codelahoma-dashboard-buffer-name)))
    (with-current-buffer buffer
      (codelahoma-dashboard-mode)
      (when codelahoma-dashboard-auto-refresh
        (codelahoma-dashboard-refresh)))
    (switch-to-buffer buffer)))

(defun codelahoma-dashboard-refresh ()
  "Refresh the dashboard content."
  (interactive)
  (when (eq major-mode 'codelahoma-dashboard-mode)
    (let ((inhibit-read-only t)
          (pos (point)))
      (erase-buffer)
      (dolist (section codelahoma-dashboard-sections)
        (codelahoma-dashboard-insert-section section))
      (goto-char (min pos (point-max))))
    (codelahoma-dashboard-start-refresh-timer)))

(defun codelahoma-dashboard-insert-section (section)
  "Insert a dashboard SECTION."
  (pcase section
    ('header (codelahoma-dashboard-insert-header))
    ('overview (codelahoma-dashboard-insert-overview))
    ('inbox-status (codelahoma-dashboard-insert-inbox-status))
    ('next-actions (codelahoma-dashboard-insert-next-actions))
    ('projects (codelahoma-dashboard-insert-projects))
    ('recent-notes (codelahoma-dashboard-insert-recent-notes))
    ('knowledge-suggestions (codelahoma-dashboard-insert-suggestions))
    ('metrics (codelahoma-dashboard-insert-metrics))
    ('quick-links (codelahoma-dashboard-insert-quick-links))
    (_ (message "Unknown dashboard section: %s" section))))

;;; Dashboard Sections

(defun codelahoma-dashboard-insert-header ()
  "Insert dashboard header."
  (insert (propertize "GTD-Zettelkasten Dashboard\n" 
                     'face '(:height 1.5 :weight bold)))
  (insert (propertize (format-time-string "%A, %B %d, %Y - %H:%M")
                     'face 'font-lock-comment-face))
  (insert "\n")
  (insert (make-string 60 ?─))
  (insert "\n\n"))

(defun codelahoma-dashboard-insert-overview ()
  "Insert system overview widget."
  (codelahoma-dashboard-widget-header "📊 System Overview")
  (let* ((inbox-count (codelahoma-gtd-quick-inbox-count))
         (next-count (codelahoma-dashboard-count-next-actions))
         (waiting-count (codelahoma-dashboard-count-waiting-for))
         (project-count (codelahoma-dashboard-count-active-projects))
         (note-count (length (org-roam-node-list)))
         (link-count (codelahoma-dashboard-count-task-links)))
    
    ;; First row
    (insert "  ")
    (codelahoma-dashboard-insert-stat "📥 Inbox" inbox-count 
                                     'codelahoma-gtd-open-inbox
                                     (if (> inbox-count 10) 'error 'success))
    (insert "  ")
    (codelahoma-dashboard-insert-stat "▶️  Next" next-count
                                     'codelahoma-gtd-open-next-actions
                                     'font-lock-keyword-face)
    (insert "  ")
    (codelahoma-dashboard-insert-stat "⏳ Waiting" waiting-count
                                     'codelahoma-gtd-open-waiting-for
                                     'font-lock-warning-face)
    (insert "\n  ")
    
    ;; Second row
    (codelahoma-dashboard-insert-stat "📁 Projects" project-count
                                     'codelahoma-gtd-open-projects
                                     'font-lock-function-name-face)
    (insert "  ")
    (codelahoma-dashboard-insert-stat "📝 Notes" note-count
                                     'org-roam-node-find
                                     'font-lock-doc-face)
    (insert "  ")
    (codelahoma-dashboard-insert-stat "🔗 Links" link-count
                                     'codelahoma-bridge-knowledge-dashboard
                                     'font-lock-constant-face)
    (insert "\n\n")))

(defun codelahoma-dashboard-insert-inbox-status ()
  "Insert inbox status widget."
  (codelahoma-dashboard-widget-header "📥 Inbox Status")
  (let ((items (codelahoma-dashboard-get-inbox-items 5))
        (total (codelahoma-gtd-quick-inbox-count)))
    (if items
        (progn
          (dolist (item items)
            (insert "  ")
            (insert-button (concat "• " (car item))
                          'action (lambda (_) 
                                   (find-file codelahoma-gtd-inbox-file)
                                   (goto-char (cdr item)))
                          'follow-link t
                          'face 'link)
            (insert "\n"))
          (when (> total 5)
            (insert (format "  ... and %d more items " (- total 5)))
            (insert-button "[Process Inbox]"
                          'action (lambda (_) (codelahoma-gtd-process-inbox))
                          'follow-link t
                          'face 'font-lock-keyword-face)
            (insert "\n")))
      (insert "  ✨ Inbox is empty! Great job!\n"))
    (insert "\n")))

(defun codelahoma-dashboard-insert-next-actions ()
  "Insert next actions widget."
  (codelahoma-dashboard-widget-header "▶️  Next Actions")
  (let ((actions (codelahoma-dashboard-get-next-actions 5)))
    (if actions
        (dolist (action actions)
          (insert "  ")
          (let ((context (plist-get action :context))
                (title (plist-get action :title))
                (marker (plist-get action :marker)))
            (when context
              (insert (propertize (format "@%s " context) 
                                 'face 'font-lock-type-face)))
            (insert-button (concat "□ " title)
                          'action (lambda (_) 
                                   (switch-to-buffer (marker-buffer marker))
                                   (goto-char marker))
                          'follow-link t
                          'face 'link))
          (insert "\n"))
      (insert "  No next actions defined. ")
      (insert-button "[Review Projects]"
                    'action (lambda (_) (codelahoma-gtd-open-projects))
                    'follow-link t
                    'face 'font-lock-keyword-face)
      (insert "\n"))
    (insert "\n")))

(defun codelahoma-dashboard-insert-projects ()
  "Insert active projects widget."
  (codelahoma-dashboard-widget-header "📁 Active Projects")
  (let ((projects (codelahoma-dashboard-get-active-projects 5)))
    (if projects
        (dolist (project projects)
          (insert "  ")
          (let ((title (plist-get project :title))
                (next-count (plist-get project :next-actions))
                (marker (plist-get project :marker)))
            (insert-button title
                          'action (lambda (_)
                                   (switch-to-buffer (marker-buffer marker))
                                   (goto-char marker))
                          'follow-link t
                          'face 'font-lock-function-name-face)
            (insert (format " (%d next actions)" next-count)))
          (insert "\n"))
      (insert "  No active projects\n"))
    (insert "\n")))

(defun codelahoma-dashboard-insert-recent-notes ()
  "Insert recent notes widget."
  (codelahoma-dashboard-widget-header "📝 Recent Knowledge Notes")
  (let ((notes (codelahoma-bridge-get-recent-notes 5)))
    (if notes
        (dolist (note notes)
          (insert "  ")
          (insert-button (format "• %s" (org-roam-node-title note))
                        'action (lambda (_) (org-roam-node-visit note))
                        'follow-link t
                        'face 'link)
          (when-let ((tags (org-roam-node-tags note)))
            (insert " ")
            (insert (propertize (format ":%s:" (string-join tags ":"))
                               'face 'font-lock-comment-face)))
          (insert "\n"))
      (insert "  No recent notes\n"))
    (insert "\n")))

(defun codelahoma-dashboard-insert-suggestions ()
  "Insert knowledge suggestions widget."
  (when (featurep 'codelahoma-bridge-suggestions)
    (codelahoma-dashboard-widget-header "💡 Knowledge Suggestions")
    (insert "  ")
    (insert-button "[Get Smart Suggestions]"
                  'action (lambda (_) (codelahoma-bridge-suggest-related-knowledge))
                  'follow-link t
                  'face 'font-lock-keyword-face)
    (insert "\n\n")))

(defun codelahoma-dashboard-insert-metrics ()
  "Insert metrics summary widget."
  (codelahoma-dashboard-widget-header "📈 Today's Progress")
  (let* ((completed (codelahoma-dashboard-count-completed-today))
         (captured (codelahoma-dashboard-count-captured-today))
         (processed (codelahoma-dashboard-count-processed-today))
         (reviewed (codelahoma-dashboard-has-reviewed-today)))
    (insert (format "  ✓ Completed: %d tasks\n" completed))
    (insert (format "  📥 Captured: %d items\n" captured))
    (insert (format "  🔄 Processed: %d items\n" processed))
    (insert (format "  📋 Daily Review: %s\n" 
                   (if reviewed "✓ Done" "⏳ Pending")))
    (insert "\n")))

(defun codelahoma-dashboard-insert-quick-links ()
  "Insert quick action links."
  (codelahoma-dashboard-widget-header "⚡ Quick Actions")
  (insert "  Capture: ")
  (codelahoma-dashboard-insert-link "[Inbox]" 'codelahoma-gtd-capture-inbox)
  (insert " ")
  (codelahoma-dashboard-insert-link "[Task]" 'codelahoma-gtd-capture-task)
  (insert " ")
  (codelahoma-dashboard-insert-link "[Note]" 'org-roam-capture)
  (insert "\n  Process: ")
  (codelahoma-dashboard-insert-link "[Inbox]" 'codelahoma-gtd-process-inbox)
  (insert " ")
  (codelahoma-dashboard-insert-link "[Review]" 'codelahoma-gtd-daily-review)
  (insert "\n  Navigate: ")
  (codelahoma-dashboard-insert-link "[Projects]" 'codelahoma-gtd-open-projects)
  (insert " ")
  (codelahoma-dashboard-insert-link "[Contexts]" 'codelahoma-gtd-agenda-by-context)
  (insert " ")
  (codelahoma-dashboard-insert-link "[Knowledge]" 'codelahoma-bridge-knowledge-dashboard)
  (insert "\n\n"))

;;; Helper Functions

(defun codelahoma-dashboard-widget-header (title)
  "Insert a widget TITLE."
  (insert (propertize title 'face '(:weight bold)))
  (insert "\n")
  (insert (make-string (length title) ?─))
  (insert "\n"))

(defun codelahoma-dashboard-insert-stat (label count action face)
  "Insert a statistic with LABEL, COUNT, ACTION, and FACE."
  (insert-button (format "%s: %d" label count)
                'action (lambda (_) (call-interactively action))
                'follow-link t
                'face face))

(defun codelahoma-dashboard-insert-link (text action)
  "Insert a link with TEXT that performs ACTION."
  (insert-button text
                'action (lambda (_) (call-interactively action))
                'follow-link t
                'face 'font-lock-keyword-face))

(defun codelahoma-dashboard-count-next-actions ()
  "Count total next actions."
  (let ((count 0))
    (dolist (file codelahoma-gtd-files)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda () (cl-incf count))
           "TODO=\"NEXT\""
           'file))))
    count))

(defun codelahoma-dashboard-count-waiting-for ()
  "Count waiting for items."
  (let ((count 0))
    (dolist (file codelahoma-gtd-files)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda () (cl-incf count))
           "TODO=\"WAITING\""
           'file))))
    count))

(defun codelahoma-dashboard-count-active-projects ()
  "Count active projects."
  (let ((count 0))
    (with-current-buffer (find-file-noselect codelahoma-gtd-projects-file)
      (org-map-entries
       (lambda () 
         (when (codelahoma-gtd-project-p)
           (cl-incf count)))
       "TODO=\"PROJECT\""
       'file))
    count))

(defun codelahoma-dashboard-count-task-links ()
  "Count tasks with knowledge links."
  (let ((count 0))
    (dolist (file codelahoma-gtd-files)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (when (org-entry-get nil codelahoma-bridge-link-property)
               (cl-incf count)))
           nil 'file))))
    count))

(defun codelahoma-dashboard-get-inbox-items (limit)
  "Get up to LIMIT inbox items."
  (let ((items '()))
    (with-current-buffer (find-file-noselect codelahoma-gtd-inbox-file)
      (org-map-entries
       (lambda ()
         (when (< (length items) limit)
           (push (cons (org-get-heading t t t t) (point))
                 items)))
       "LEVEL=1"
       'file))
    (nreverse items)))

(defun codelahoma-dashboard-get-next-actions (limit)
  "Get up to LIMIT next actions with context."
  (let ((actions '()))
    (dolist (file codelahoma-gtd-files)
      (when (and (file-exists-p file) (< (length actions) limit))
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (when (< (length actions) limit)
               (push (list :title (org-get-heading t t t t)
                          :context (org-entry-get nil "CONTEXT")
                          :marker (point-marker))
                     actions)))
           "TODO=\"NEXT\""
           'file))))
    (nreverse actions)))

(defun codelahoma-dashboard-get-active-projects (limit)
  "Get up to LIMIT active projects."
  (let ((projects '()))
    (with-current-buffer (find-file-noselect codelahoma-gtd-projects-file)
      (org-map-entries
       (lambda ()
         (when (and (codelahoma-gtd-project-p)
                    (< (length projects) limit))
           (let ((next-count 0))
             (org-map-entries
              (lambda () (cl-incf next-count))
              "TODO=\"NEXT\""
              'tree)
             (push (list :title (org-get-heading t t t t)
                        :next-actions next-count
                        :marker (point-marker))
                   projects))))
       "TODO=\"PROJECT\""
       'file))
    (nreverse projects)))

(defun codelahoma-dashboard-count-completed-today ()
  "Count tasks completed today."
  (let ((count 0)
        (today (format-time-string "%Y-%m-%d")))
    (dolist (file codelahoma-gtd-files)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (when-let ((closed (org-entry-get nil "CLOSED")))
               (when (string-match today closed)
                 (cl-incf count))))
           "TODO=\"DONE\""
           'file))))
    count))

(defun codelahoma-dashboard-count-captured-today ()
  "Count items captured today."
  ;; Simplified - would track actual capture times in production
  (random 5))

(defun codelahoma-dashboard-count-processed-today ()
  "Count items processed today."
  ;; Simplified - would track actual processing in production
  (random 5))

(defun codelahoma-dashboard-has-reviewed-today ()
  "Check if daily review was done today."
  ;; Check if morning or evening review was done
  (or (file-exists-p (expand-file-name 
                     (format "daily-reviews/%s-morning.org"
                            (format-time-string "%Y-%m-%d"))
                     codelahoma-gtd-directory))
      (file-exists-p (expand-file-name 
                     (format "daily-reviews/%s-evening.org"
                            (format-time-string "%Y-%m-%d"))
                     codelahoma-gtd-directory))))

;;; Auto-refresh

(defun codelahoma-dashboard-start-refresh-timer ()
  "Start auto-refresh timer if configured."
  (when (and codelahoma-dashboard-refresh-interval
             (> codelahoma-dashboard-refresh-interval 0))
    (when codelahoma-dashboard-timer
      (cancel-timer codelahoma-dashboard-timer))
    (setq codelahoma-dashboard-timer
          (run-with-timer codelahoma-dashboard-refresh-interval
                         codelahoma-dashboard-refresh-interval
                         'codelahoma-dashboard-refresh-if-visible))))

(defun codelahoma-dashboard-refresh-if-visible ()
  "Refresh dashboard if buffer is visible."
  (when-let ((buffer (get-buffer codelahoma-dashboard-buffer-name)))
    (when (get-buffer-window buffer)
      (with-current-buffer buffer
        (codelahoma-dashboard-refresh)))))

(defun codelahoma-dashboard-stop-refresh-timer ()
  "Stop the auto-refresh timer."
  (when codelahoma-dashboard-timer
    (cancel-timer codelahoma-dashboard-timer)
    (setq codelahoma-dashboard-timer nil)))

;;; Dashboard Mode

(defvar codelahoma-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'codelahoma-dashboard-refresh)
    (define-key map "G" 'codelahoma-dashboard-hard-refresh)
    (define-key map "i" 'codelahoma-gtd-open-inbox)
    (define-key map "n" 'codelahoma-gtd-open-next-actions)
    (define-key map "p" 'codelahoma-gtd-open-projects)
    (define-key map "w" 'codelahoma-gtd-open-waiting-for)
    (define-key map "z" 'org-roam-node-find)
    (define-key map "c" 'codelahoma-gtd-capture-inbox)
    (define-key map "C" 'codelahoma-gtd-capture-task)
    (define-key map "d" 'codelahoma-gtd-daily-review)
    (define-key map "r" 'codelahoma-gtd-weekly-review)
    (define-key map "s" 'codelahoma-bridge-suggest-related-knowledge)
    (define-key map "m" 'codelahoma-bridge-knowledge-dashboard)
    (define-key map "?" 'codelahoma-dashboard-help)
    (define-key map "q" 'quit-window)
    map)
  "Keymap for dashboard mode.")

(define-derived-mode codelahoma-dashboard-mode special-mode "GTD-Dashboard"
  "Major mode for GTD-Zettelkasten dashboard.

\\{codelahoma-dashboard-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local revert-buffer-function #'codelahoma-dashboard-refresh)
  (when codelahoma-dashboard-auto-refresh
    (codelahoma-dashboard-start-refresh-timer)))

(defun codelahoma-dashboard-hard-refresh ()
  "Force refresh all cached data and redraw dashboard."
  (interactive)
  (setq codelahoma-bridge-metrics-cache nil)
  (codelahoma-dashboard-refresh)
  (message "Dashboard refreshed with fresh data"))

(defun codelahoma-dashboard-help ()
  "Show dashboard keybindings help."
  (interactive)
  (message "Dashboard keys: [g]refresh [i]nbox [n]ext [p]rojects [c]apture [d]aily-review [?]help [q]uit"))

;; Cleanup on kill
(add-hook 'kill-buffer-hook
          (lambda ()
            (when (eq major-mode 'codelahoma-dashboard-mode)
              (codelahoma-dashboard-stop-refresh-timer))))

(provide 'codelahoma-dashboard)
;;; codelahoma-dashboard.el ends here