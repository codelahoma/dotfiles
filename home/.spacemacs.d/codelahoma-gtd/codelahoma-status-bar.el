;;; codelahoma-status-bar.el --- Status bar integration for GTD-Zettelkasten -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Status bar (mode line) integration for the GTD-Zettelkasten system,
;; providing at-a-glance system status and quick actions.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)
(require 'codelahoma-gtd-contexts)
(require 'cl-lib)

;;; Configuration

(defcustom codelahoma-status-bar-enabled t
  "Enable GTD status in mode line."
  :type 'boolean
  :group 'codelahoma-gtd)

(defcustom codelahoma-status-bar-update-interval 60
  "Seconds between status bar updates."
  :type 'integer
  :group 'codelahoma-gtd)

(defcustom codelahoma-status-bar-format
  '(codelahoma-status-bar-inbox
    codelahoma-status-bar-next-actions
    codelahoma-status-bar-context
    codelahoma-status-bar-daily-progress)
  "Components to display in status bar."
  :type '(repeat symbol)
  :group 'codelahoma-gtd)

(defcustom codelahoma-status-bar-inbox-warning-threshold 10
  "Inbox count above which to show warning color."
  :type 'integer
  :group 'codelahoma-gtd)

;;; Status Bar Core

(defvar codelahoma-mode-line-string ""
  "String to display in mode line.")

(defvar codelahoma-status-bar-timer nil
  "Timer for status bar updates.")

(defvar codelahoma-status-bar-cache nil
  "Cache for expensive computations.")

(defvar codelahoma-status-bar-cache-time nil
  "Time when cache was last updated.")

(defun codelahoma-status-bar-enable ()
  "Enable GTD status in mode line."
  (interactive)
  ;; Add to mode line
  (unless (memq 'codelahoma-mode-line-string global-mode-string)
    (setq global-mode-string
          (append global-mode-string '(codelahoma-mode-line-string))))
  ;; Start updates
  (codelahoma-status-bar-update)
  (when codelahoma-status-bar-timer
    (cancel-timer codelahoma-status-bar-timer))
  (setq codelahoma-status-bar-timer
        (run-with-timer 0 codelahoma-status-bar-update-interval
                       'codelahoma-status-bar-update))
  (message "GTD status bar enabled"))

(defun codelahoma-status-bar-disable ()
  "Disable GTD status in mode line."
  (interactive)
  ;; Remove from mode line
  (setq global-mode-string
        (delq 'codelahoma-mode-line-string global-mode-string))
  ;; Stop updates
  (when codelahoma-status-bar-timer
    (cancel-timer codelahoma-status-bar-timer)
    (setq codelahoma-status-bar-timer nil))
  (setq codelahoma-mode-line-string "")
  (force-mode-line-update t)
  (message "GTD status bar disabled"))

(defun codelahoma-status-bar-update ()
  "Update the status bar."
  (when codelahoma-status-bar-enabled
    (let ((components '()))
      (dolist (component codelahoma-status-bar-format)
        (when-let ((text (funcall component)))
          (push text components)))
      (setq codelahoma-mode-line-string
            (if components
                (concat " [GTD: " (string-join (nreverse components) " | ") "]")
              ""))
      (force-mode-line-update t))))

;;; Status Components

(defun codelahoma-status-bar-inbox ()
  "Return inbox status for mode line."
  (let* ((count (codelahoma-status-bar-get-cached 'inbox-count
                                                  'codelahoma-gtd-quick-inbox-count))
         (face (cond
                ((= count 0) 'success)
                ((> count codelahoma-status-bar-inbox-warning-threshold) 'error)
                (t 'warning))))
    (propertize 
     (format "📥%d" count)
     'face face
     'help-echo (format "%d items in inbox\nmouse-1: Open inbox\nmouse-3: Process inbox" count)
     'mouse-face 'mode-line-highlight
     'local-map (codelahoma-status-bar-inbox-map))))

(defun codelahoma-status-bar-next-actions ()
  "Return next actions count for mode line."
  (let ((count (codelahoma-status-bar-get-cached 'next-count
                                                 'codelahoma-status-bar-count-next-actions)))
    (propertize
     (format "▶%d" count)
     'face (if (= count 0) 'shadow 'font-lock-keyword-face)
     'help-echo (format "%d next actions\nmouse-1: View next actions" count)
     'mouse-face 'mode-line-highlight
     'local-map (codelahoma-status-bar-next-actions-map))))

(defun codelahoma-status-bar-context ()
  "Return current context for mode line."
  (when (bound-and-true-p codelahoma-gtd-current-context)
    (propertize
     (format "@%s" codelahoma-gtd-current-context)
     'face 'font-lock-type-face
     'help-echo (format "Current context: %s\nmouse-1: Change context" 
                       codelahoma-gtd-current-context)
     'mouse-face 'mode-line-highlight
     'local-map (codelahoma-status-bar-context-map))))

(defun codelahoma-status-bar-daily-progress ()
  "Return daily progress for mode line."
  (let* ((completed (codelahoma-status-bar-get-cached 'completed-today
                                                     'codelahoma-status-bar-count-completed-today))
         (goal 5) ; Daily goal
         (percentage (min 100 (round (* 100.0 (/ (float completed) goal)))))
         (face (cond
                ((>= percentage 100) 'success)
                ((>= percentage 50) 'warning)
                (t 'shadow))))
    (propertize
     (format "✓%d/%d" completed goal)
     'face face
     'help-echo (format "Daily progress: %d%% (%d/%d tasks completed)\nmouse-1: Daily review" 
                       percentage completed goal)
     'mouse-face 'mode-line-highlight
     'local-map (codelahoma-status-bar-progress-map))))

;;; Keymaps

(defun codelahoma-status-bar-inbox-map ()
  "Create keymap for inbox status."
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 
                (lambda () (interactive) (codelahoma-gtd-open-inbox)))
    (define-key map [mode-line mouse-3] 
                (lambda () (interactive) (codelahoma-gtd-process-inbox)))
    map))

(defun codelahoma-status-bar-next-actions-map ()
  "Create keymap for next actions status."
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 
                (lambda () (interactive) (codelahoma-gtd-open-next-actions)))
    map))

(defun codelahoma-status-bar-context-map ()
  "Create keymap for context status."
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 
                (lambda () (interactive) (codelahoma-gtd-quick-switch-context)))
    map))

(defun codelahoma-status-bar-progress-map ()
  "Create keymap for progress status."
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 
                (lambda () (interactive) (codelahoma-gtd-daily-review)))
    map))

;;; Helper Functions

(defun codelahoma-status-bar-get-cached (key compute-fn)
  "Get cached value for KEY or compute with COMPUTE-FN."
  (let ((cache-valid (and codelahoma-status-bar-cache-time
                         (< (float-time (time-subtract (current-time)
                                                      codelahoma-status-bar-cache-time))
                            30)))) ; 30 second cache
    (if (and cache-valid (plist-member codelahoma-status-bar-cache key))
        (plist-get codelahoma-status-bar-cache key)
      ;; Recompute
      (let ((value (funcall compute-fn)))
        (setq codelahoma-status-bar-cache
              (plist-put codelahoma-status-bar-cache key value))
        (setq codelahoma-status-bar-cache-time (current-time))
        value))))

(defun codelahoma-status-bar-count-next-actions ()
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

(defun codelahoma-status-bar-count-completed-today ()
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

;;; Notifications

(defvar codelahoma-status-bar-notification nil
  "Current notification message.")

(defvar codelahoma-status-bar-notification-timer nil
  "Timer for clearing notifications.")

(defun codelahoma-status-bar-notify (message &optional type duration)
  "Show notification MESSAGE of TYPE for DURATION seconds."
  (setq codelahoma-status-bar-notification
        (propertize (format " [%s] " message)
                   'face (pcase type
                          ('success 'success)
                          ('warning 'warning)
                          ('error 'error)
                          (_ 'bold))))
  ;; Temporarily replace mode line
  (let ((original codelahoma-mode-line-string))
    (setq codelahoma-mode-line-string codelahoma-status-bar-notification)
    (force-mode-line-update t)
    ;; Clear after duration
    (when codelahoma-status-bar-notification-timer
      (cancel-timer codelahoma-status-bar-notification-timer))
    (setq codelahoma-status-bar-notification-timer
          (run-with-timer (or duration 3) nil
                         (lambda ()
                           (setq codelahoma-mode-line-string original)
                           (setq codelahoma-status-bar-notification nil)
                           (force-mode-line-update t))))))

;;; Click Actions

(defun codelahoma-status-bar-click-menu ()
  "Show menu of GTD actions."
  (interactive)
  (let ((choice (completing-read "GTD Action: "
                                '("Open Dashboard"
                                  "Process Inbox"
                                  "View Next Actions"
                                  "Daily Review"
                                  "Weekly Review"
                                  "Capture to Inbox"
                                  "Switch Context"
                                  "Knowledge Search"))))
    (pcase choice
      ("Open Dashboard" (codelahoma-dashboard))
      ("Process Inbox" (codelahoma-gtd-process-inbox))
      ("View Next Actions" (codelahoma-gtd-open-next-actions))
      ("Daily Review" (codelahoma-gtd-daily-review))
      ("Weekly Review" (codelahoma-gtd-weekly-review))
      ("Capture to Inbox" (codelahoma-gtd-capture-inbox))
      ("Switch Context" (codelahoma-gtd-quick-switch-context))
      ("Knowledge Search" (codelahoma-search)))))

;;; Integration Hooks

(defun codelahoma-status-bar-task-completed ()
  "Hook run when a task is completed."
  (codelahoma-status-bar-notify "Task completed!" 'success)
  (codelahoma-status-bar-invalidate-cache)
  (codelahoma-status-bar-update))

(defun codelahoma-status-bar-item-captured ()
  "Hook run when an item is captured."
  (codelahoma-status-bar-notify "Captured!" 'success 2)
  (codelahoma-status-bar-invalidate-cache)
  (codelahoma-status-bar-update))

(defun codelahoma-status-bar-context-changed ()
  "Hook run when context changes."
  (codelahoma-status-bar-notify 
   (format "Context: @%s" codelahoma-gtd-current-context) nil 2)
  (codelahoma-status-bar-update))

(defun codelahoma-status-bar-invalidate-cache ()
  "Invalidate the status bar cache."
  (setq codelahoma-status-bar-cache nil
        codelahoma-status-bar-cache-time nil))

;;; Sparkline Progress

(defun codelahoma-status-bar-sparkline (values max-value width)
  "Create a sparkline string from VALUES with MAX-VALUE and WIDTH."
  (let* ((chars "▁▂▃▄▅▆▇█")
         (levels (length chars)))
    (mapconcat
     (lambda (val)
       (let ((level (min (1- levels)
                        (floor (* (/ (float val) max-value) levels)))))
         (char-to-string (aref chars level))))
     values "")))

(defun codelahoma-status-bar-weekly-sparkline ()
  "Return a sparkline of the week's progress."
  (let ((daily-counts (codelahoma-status-bar-get-week-counts)))
    (codelahoma-status-bar-sparkline daily-counts 10 7)))

(defun codelahoma-status-bar-get-week-counts ()
  "Get completion counts for the past week."
  ;; Simplified - would track actual daily counts in production
  (list 3 5 2 7 6 4 0)) ; Mon-Sun

;;; Minor Mode

;;;###autoload
(define-minor-mode codelahoma-status-bar-mode
  "Toggle GTD status bar display."
  :global t
  :group 'codelahoma-gtd
  :init-value nil
  (if codelahoma-status-bar-mode
      (when (fboundp 'codelahoma-status-bar-enable)
        (codelahoma-status-bar-enable))
    (when (fboundp 'codelahoma-status-bar-disable)
      (codelahoma-status-bar-disable))))

;;; Initialize

;; Set up hooks
(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (when (string= org-state "DONE")
              (codelahoma-status-bar-task-completed))))

;; Auto-enable will be handled by codelahoma-ui.el after all modules are loaded

(provide 'codelahoma-status-bar)
;;; codelahoma-status-bar.el ends here