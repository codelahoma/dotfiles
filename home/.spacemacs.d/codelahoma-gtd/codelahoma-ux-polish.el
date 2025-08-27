;;; codelahoma-ux-polish.el --- UX polish for GTD-Zettelkasten -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; User experience enhancements for the GTD-Zettelkasten system including
;; onboarding, tooltips, undo/redo, confirmations, and help system.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)
(require 'codelahoma-dashboard)
(require 'cl-lib)

;;; Configuration

(defcustom codelahoma-ux-enable-tooltips t
  "Enable helpful tooltips throughout the system."
  :type 'boolean
  :group 'codelahoma-gtd)

(defcustom codelahoma-ux-enable-confirmations t
  "Enable confirmation dialogs for destructive actions."
  :type 'boolean
  :group 'codelahoma-gtd)

(defcustom codelahoma-ux-undo-limit 50
  "Maximum number of undo operations to track."
  :type 'integer
  :group 'codelahoma-gtd)

(defcustom codelahoma-ux-show-welcome-on-startup nil
  "Show welcome screen on startup."
  :type 'boolean
  :group 'codelahoma-gtd)

;;; Setup

(defun codelahoma-ux-setup ()
  "Set up UX enhancements."
  (codelahoma-ux-setup-tooltips)
  (codelahoma-ux-setup-confirmations)
  (codelahoma-ux-setup-help)
  (codelahoma-ux-setup-keybinding-hints)
  (when (codelahoma-ux-first-run-p)
    (codelahoma-ux-onboarding)))

;;; Onboarding

(defvar codelahoma-ux-onboarding-completed nil
  "Whether onboarding has been completed.")

(defun codelahoma-ux-first-run-p ()
  "Check if this is the first run."
  (not (file-exists-p (expand-file-name ".onboarding-complete" 
                                        codelahoma-gtd-directory))))

(defun codelahoma-ux-onboarding ()
  "Run onboarding flow for new users."
  (interactive)
  (let ((buffer (get-buffer-create "*GTD-Zettelkasten Welcome*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (codelahoma-ux-onboarding-mode)
        (insert (propertize "Welcome to GTD-Zettelkasten System!\n" 
                           'face '(:height 1.5 :weight bold)))
        (insert (propertize "====================================\n\n" 
                           'face 'shadow))
        
        (insert "This guided tour will help you get started with your new productivity system.\n\n")
        
        (insert (propertize "Getting Started:\n" 'face '(:weight bold)))
        (insert "────────────────\n\n")
        
        ;; Step 1
        (insert "1. ")
        (insert-button "Set up your GTD files"
                      'action (lambda (_) (codelahoma-ux-setup-files))
                      'follow-link t
                      'face 'link)
        (insert " - Initialize inbox, projects, and other GTD files\n\n")
        
        ;; Step 2
        (insert "2. ")
        (insert-button "Configure your contexts"
                      'action (lambda (_) (codelahoma-ux-setup-contexts))
                      'follow-link t
                      'face 'link)
        (insert " - Define contexts like @home, @work, @computer\n\n")
        
        ;; Step 3
        (insert "3. ")
        (insert-button "Learn essential commands"
                      'action (lambda (_) (codelahoma-ux-show-essential-commands))
                      'follow-link t
                      'face 'link)
        (insert " - Master the key commands for daily use\n\n")
        
        ;; Step 4
        (insert "4. ")
        (insert-button "Try your first capture"
                      'action (lambda (_) 
                               (delete-window)
                               (codelahoma-gtd-capture-inbox))
                      'follow-link t
                      'face 'link)
        (insert " - Capture a thought or task to your inbox\n\n")
        
        ;; Step 5
        (insert "5. ")
        (insert-button "Open the dashboard"
                      'action (lambda (_) 
                               (delete-window)
                               (codelahoma-dashboard))
                      'follow-link t
                      'face 'link)
        (insert " - See your system overview\n\n")
        
        (insert "\n" (propertize "Quick Reference:\n" 'face '(:weight bold)))
        (insert "───────────────\n")
        (insert "• Main Dashboard: " (propertize "SPC o o SPC" 'face 'font-lock-keyword-face) "\n")
        (insert "• Capture to Inbox: " (propertize "SPC o o c i" 'face 'font-lock-keyword-face) "\n")
        (insert "• Process Inbox: " (propertize "SPC o o p i" 'face 'font-lock-keyword-face) "\n")
        (insert "• Command Palette: " (propertize "SPC o o o" 'face 'font-lock-keyword-face) "\n")
        (insert "• Search Everything: " (propertize "SPC o o s s" 'face 'font-lock-keyword-face) "\n")
        
        (insert "\n")
        (insert-button "[Skip Onboarding]"
                      'action (lambda (_) 
                               (codelahoma-ux-mark-onboarding-complete)
                               (kill-buffer))
                      'face 'shadow)
        (insert " ")
        (insert-button "[Interactive Tutorial]"
                      'action (lambda (_) (codelahoma-ux-interactive-tutorial))
                      'face 'font-lock-keyword-face)
        
        (goto-char (point-min))
        (forward-line 4)))
    (switch-to-buffer buffer)))

(defun codelahoma-ux-setup-files ()
  "Set up GTD files interactively."
  (interactive)
  (when (y-or-n-p "Create GTD directory structure? ")
    (codelahoma-gtd-initialize)
    (message "GTD files created successfully!")
    (codelahoma-ux-notify "✓ GTD files initialized" 'success)))

(defun codelahoma-ux-setup-contexts ()
  "Set up contexts interactively."
  (interactive)
  (let ((contexts '()))
    (while (y-or-n-p "Add a context? ")
      (let ((context (read-string "Context name (without @): ")))
        (push (concat "@" context) contexts)))
    (when contexts
      (customize-save-variable 'codelahoma-gtd-contexts (nreverse contexts))
      (message "Contexts saved: %s" (string-join codelahoma-gtd-contexts ", ")))))

(defun codelahoma-ux-show-essential-commands ()
  "Show essential commands in a buffer."
  (interactive)
  (with-output-to-temp-buffer "*Essential GTD Commands*"
    (princ "Essential GTD-Zettelkasten Commands\n")
    (princ "===================================\n\n")
    (princ "Daily Workflow:\n")
    (princ "──────────────\n")
    (princ (format "%-25s %s\n" "Dashboard:" "SPC o o SPC"))
    (princ (format "%-25s %s\n" "Capture thought:" "SPC o o c i"))
    (princ (format "%-25s %s\n" "Process inbox:" "SPC o o p i"))
    (princ (format "%-25s %s\n" "View next actions:" "SPC o o n n"))
    (princ (format "%-25s %s\n" "Daily review:" "SPC o o r d"))
    (princ "\nKnowledge Management:\n")
    (princ "───────────────────\n")
    (princ (format "%-25s %s\n" "Create note:" "SPC o o z c"))
    (princ (format "%-25s %s\n" "Find note:" "SPC o o z n"))
    (princ (format "%-25s %s\n" "Link task to note:" "SPC o o i l"))
    (princ (format "%-25s %s\n" "Search everything:" "SPC o o s s"))
    (princ "\nQuick Access:\n")
    (princ "────────────\n")
    (princ (format "%-25s %s\n" "Command palette:" "SPC o o o"))
    (princ (format "%-25s %s\n" "Quick navigation:" "SPC o o q q"))
    (princ (format "%-25s %s\n" "Help:" "SPC o o h"))))

(defun codelahoma-ux-mark-onboarding-complete ()
  "Mark onboarding as complete."
  (let ((marker-file (expand-file-name ".onboarding-complete" 
                                      codelahoma-gtd-directory)))
    (with-temp-file marker-file
      (insert (format-time-string "%Y-%m-%d %H:%M:%S")))
    (setq codelahoma-ux-onboarding-completed t)))

;;; Interactive Tutorial

(defvar codelahoma-ux-tutorial-steps nil
  "Current tutorial steps.")

(defvar codelahoma-ux-tutorial-current 0
  "Current tutorial step.")

(defun codelahoma-ux-interactive-tutorial ()
  "Start interactive tutorial."
  (interactive)
  (setq codelahoma-ux-tutorial-steps
        '((:title "Capture a Thought"
           :instruction "Let's start by capturing a thought. Press the key sequence shown."
           :key "SPC o o c i"
           :action codelahoma-gtd-capture-inbox
           :verify codelahoma-ux-verify-capture)
          (:title "Open Your Inbox"
           :instruction "Great! Now let's look at your inbox."
           :key "SPC o o n i"
           :action codelahoma-gtd-open-inbox
           :verify (lambda () t))
          (:title "Process an Item"
           :instruction "Let's process that item you just captured."
           :key "SPC o o p i"
           :action codelahoma-gtd-process-inbox
           :verify (lambda () t))
          (:title "View Dashboard"
           :instruction "Finally, let's see your dashboard."
           :key "SPC o o SPC"
           :action codelahoma-dashboard
           :verify (lambda () t))))
  (setq codelahoma-ux-tutorial-current 0)
  (codelahoma-ux-tutorial-next-step))

(defun codelahoma-ux-tutorial-next-step ()
  "Show next tutorial step."
  (if (< codelahoma-ux-tutorial-current (length codelahoma-ux-tutorial-steps))
      (let* ((step (nth codelahoma-ux-tutorial-current codelahoma-ux-tutorial-steps))
             (title (plist-get step :title))
             (instruction (plist-get step :instruction))
             (key (plist-get step :key)))
        (codelahoma-ux-tutorial-show-step title instruction key))
    (codelahoma-ux-tutorial-complete)))

(defun codelahoma-ux-tutorial-show-step (title instruction key)
  "Show tutorial step with TITLE, INSTRUCTION, and KEY."
  (let ((buffer (get-buffer-create "*GTD Tutorial*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Step %d: %s\n" 
                                   (1+ codelahoma-ux-tutorial-current) title)
                           'face '(:height 1.3 :weight bold)))
        (insert "\n" instruction "\n\n")
        (insert "Press: ")
        (insert (propertize key 'face 'font-lock-keyword-face))
        (insert "\n\n")
        (insert-button "[Skip Tutorial]"
                      'action (lambda (_) (codelahoma-ux-tutorial-complete))
                      'face 'shadow)))
    (display-buffer buffer '(display-buffer-at-bottom))))

(defun codelahoma-ux-verify-capture ()
  "Verify capture was performed."
  (> (codelahoma-gtd-quick-inbox-count) 0))

(defun codelahoma-ux-tutorial-complete ()
  "Complete the tutorial."
  (interactive)
  (when-let ((buffer (get-buffer "*GTD Tutorial*")))
    (kill-buffer buffer))
  (message "Tutorial complete! You're ready to use GTD-Zettelkasten.")
  (codelahoma-dashboard))

;;; Smart Tooltips

(defvar codelahoma-ux-tooltips nil
  "Alist of command to tooltip mappings.")

(defun codelahoma-ux-setup-tooltips ()
  "Set up helpful tooltips."
  (setq codelahoma-ux-tooltips
        '((codelahoma-gtd-capture-inbox . "Quickly capture a thought to inbox (C-c c)")
          (codelahoma-gtd-process-inbox . "Process inbox items one by one")
          (codelahoma-gtd-open-projects . "View and manage your projects")
          (codelahoma-bridge-link-task-to-note . "Connect this task with relevant knowledge")
          (codelahoma-dashboard . "Open unified dashboard for system overview")
          (codelahoma-search . "Search across all tasks and notes")
          (codelahoma-command-palette . "Quick access to all commands")))
  
  ;; Add tooltips to help-echo
  (when codelahoma-ux-enable-tooltips
    (advice-add 'call-interactively :before #'codelahoma-ux-show-tooltip)))

(defun codelahoma-ux-show-tooltip (command &rest _)
  "Show tooltip for COMMAND if available."
  (when-let ((tooltip (and codelahoma-ux-enable-tooltips
                          (alist-get command codelahoma-ux-tooltips))))
    (message tooltip)))

;;; Undo/Redo Support

(defvar codelahoma-ux-undo-stack nil
  "Stack of undoable operations.")

(defvar codelahoma-ux-redo-stack nil
  "Stack of redoable operations.")

(cl-defstruct codelahoma-ux-operation
  "An undoable operation."
  description
  undo-fn
  redo-fn
  timestamp)

(defun codelahoma-ux-push-undo (description undo-fn redo-fn)
  "Push an undoable operation with DESCRIPTION, UNDO-FN, and REDO-FN."
  (push (make-codelahoma-ux-operation
         :description description
         :undo-fn undo-fn
         :redo-fn redo-fn
         :timestamp (current-time))
        codelahoma-ux-undo-stack)
  ;; Clear redo stack on new operation
  (setq codelahoma-ux-redo-stack nil)
  ;; Limit stack size
  (when (> (length codelahoma-ux-undo-stack) codelahoma-ux-undo-limit)
    (setq codelahoma-ux-undo-stack 
          (seq-take codelahoma-ux-undo-stack codelahoma-ux-undo-limit))))

(defun codelahoma-ux-undo ()
  "Undo last GTD operation."
  (interactive)
  (if codelahoma-ux-undo-stack
      (let ((operation (pop codelahoma-ux-undo-stack)))
        (condition-case err
            (progn
              (funcall (codelahoma-ux-operation-undo-fn operation))
              (push operation codelahoma-ux-redo-stack)
              (message "Undone: %s" (codelahoma-ux-operation-description operation)))
          (error (message "Undo failed: %s" (error-message-string err)))))
    (message "Nothing to undo")))

(defun codelahoma-ux-redo ()
  "Redo last undone GTD operation."
  (interactive)
  (if codelahoma-ux-redo-stack
      (let ((operation (pop codelahoma-ux-redo-stack)))
        (condition-case err
            (progn
              (funcall (codelahoma-ux-operation-redo-fn operation))
              (push operation codelahoma-ux-undo-stack)
              (message "Redone: %s" (codelahoma-ux-operation-description operation)))
          (error (message "Redo failed: %s" (error-message-string err)))))
    (message "Nothing to redo")))

;;; Confirmations

(defun codelahoma-ux-setup-confirmations ()
  "Set up confirmation dialogs."
  (when codelahoma-ux-enable-confirmations
    ;; Add advice to destructive operations
    (advice-add 'codelahoma-gtd-archive-completed-projects 
                :around #'codelahoma-ux-confirm-wrapper)
    (advice-add 'org-archive-subtree 
                :around #'codelahoma-ux-confirm-wrapper)))

(defun codelahoma-ux-confirm-wrapper (orig-fun &rest args)
  "Wrapper to add confirmation to ORIG-FUN with ARGS."
  (if (codelahoma-ux-confirm-destructive 
       (format "Are you sure you want to %s?" 
              (codelahoma-ux-get-friendly-name orig-fun)))
      (apply orig-fun args)
    (message "Cancelled")))

(defun codelahoma-ux-confirm-destructive (prompt)
  "Show confirmation dialog with PROMPT."
  (if codelahoma-ux-enable-confirmations
      (yes-or-no-p prompt)
    t))

(defun codelahoma-ux-get-friendly-name (function)
  "Get friendly name for FUNCTION."
  (pcase function
    ('codelahoma-gtd-archive-completed-projects "archive completed projects")
    ('org-archive-subtree "archive this item")
    (_ (symbol-name function))))

;;; Keybinding Hints

(defvar codelahoma-ux-hint-timer nil
  "Timer for showing hints.")

(defun codelahoma-ux-setup-keybinding-hints ()
  "Set up keybinding hints system."
  (add-hook 'codelahoma-dashboard-mode-hook #'codelahoma-ux-show-dashboard-hints)
  (add-hook 'org-mode-hook #'codelahoma-ux-check-gtd-file-hints))

(defun codelahoma-ux-show-dashboard-hints ()
  "Show hints in dashboard."
  (when codelahoma-ux-hint-timer
    (cancel-timer codelahoma-ux-hint-timer))
  (setq codelahoma-ux-hint-timer
        (run-with-idle-timer 
         3 nil
         (lambda ()
           (message "Tip: Press '?' for help, 'g' to refresh dashboard")))))

(defun codelahoma-ux-check-gtd-file-hints ()
  "Check if we're in a GTD file and show appropriate hints."
  (when (member (buffer-file-name) codelahoma-gtd-files)
    (run-with-idle-timer 
     2 nil
     (lambda ()
       (cond
        ((string= (buffer-file-name) codelahoma-gtd-inbox-file)
         (message "Tip: Use SPC o o p i to process inbox items"))
        ((string= (buffer-file-name) codelahoma-gtd-projects-file)
         (message "Tip: Use TAB to expand/collapse projects")))))))

;;; Interactive Help

(defun codelahoma-ux-help ()
  "Show context-sensitive help."
  (interactive)
  (let ((context (codelahoma-ux-current-context)))
    (pcase context
      ('dashboard (codelahoma-ux-help-dashboard))
      ('inbox (codelahoma-ux-help-inbox))
      ('project (codelahoma-ux-help-project))
      ('review (codelahoma-ux-help-review))
      ('knowledge (codelahoma-ux-help-knowledge))
      (_ (codelahoma-ux-help-general)))))

(defun codelahoma-ux-current-context ()
  "Determine current context."
  (cond
   ((eq major-mode 'codelahoma-dashboard-mode) 'dashboard)
   ((string= (buffer-file-name) codelahoma-gtd-inbox-file) 'inbox)
   ((and (buffer-file-name)
         (string= (buffer-file-name) codelahoma-gtd-projects-file)) 'project)
   ((bound-and-true-p org-roam-mode) 'knowledge)
   (t 'general)))

(defun codelahoma-ux-help-dashboard ()
  "Show dashboard-specific help."
  (message "Dashboard: [g]refresh [i]nbox [n]ext-actions [c]apture [?]help"))

(defun codelahoma-ux-help-inbox ()
  "Show inbox-specific help."
  (message "Inbox: [SPC o o p i]process [C-c C-w]refile [C-c C-c]tags"))

(defun codelahoma-ux-help-general ()
  "Show general help."
  (message "GTD Help: [SPC o o h]help [SPC o o o]commands [SPC o o SPC]dashboard"))

;;; Notifications

(defun codelahoma-ux-notify (message &optional type duration)
  "Show notification MESSAGE of TYPE for DURATION."
  (let ((formatted-message
         (pcase type
           ('success (concat "✅ " message))
           ('warning (concat "⚠️  " message))
           ('error (concat "❌ " message))
           (_ message))))
    (message formatted-message)
    ;; Also update status bar if available
    (when (fboundp 'codelahoma-status-bar-notify)
      (codelahoma-status-bar-notify message type duration))))

;;; Progress Indicators

(defun codelahoma-ux-with-progress (message fn)
  "Run FN with progress MESSAGE."
  (let ((reporter (make-progress-reporter message)))
    (unwind-protect
        (funcall fn reporter)
      (progress-reporter-done reporter))))

;;; Error Recovery

(defun codelahoma-ux-safe-operation (operation &rest args)
  "Run OPERATION with ARGS safely, showing user-friendly errors."
  (condition-case err
      (apply operation args)
    (error
     (codelahoma-ux-notify 
      (format "Operation failed: %s" (error-message-string err))
      'error)
     nil)))

;;; Welcome Screen

(defun codelahoma-ux-welcome-screen ()
  "Show welcome screen with daily summary."
  (interactive)
  (let ((buffer (get-buffer-create "*GTD Welcome*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Good " 'face '(:height 1.3)))
        (insert (propertize (codelahoma-ux-time-greeting) 
                           'face '(:height 1.3 :weight bold)))
        (insert (propertize "!\n\n" 'face '(:height 1.3)))
        
        ;; Daily summary
        (insert "Today's Summary:\n")
        (insert "───────────────\n")
        (insert (format "• Inbox items: %d\n" (codelahoma-gtd-quick-inbox-count)))
        (insert (format "• Next actions: %d\n" 
                       (codelahoma-dashboard-count-next-actions)))
        (insert (format "• Completed today: %d\n" 
                       (codelahoma-dashboard-count-completed-today)))
        
        (insert "\n")
        (insert-button "[Open Dashboard]"
                      'action (lambda (_) 
                               (kill-buffer)
                               (codelahoma-dashboard))
                      'face 'font-lock-keyword-face)
        (insert " ")
        (insert-button "[Start Daily Review]"
                      'action (lambda (_)
                               (kill-buffer)
                               (codelahoma-gtd-daily-review))
                      'face 'link)))
    (display-buffer buffer)))

(defun codelahoma-ux-time-greeting ()
  "Return time-appropriate greeting."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (cond
     ((< hour 12) "morning")
     ((< hour 17) "afternoon")
     (t "evening"))))

;;; Mode Definition

(define-derived-mode codelahoma-ux-onboarding-mode special-mode "GTD-Onboarding"
  "Major mode for GTD onboarding."
  (setq truncate-lines t))

;;; Initialize

(defun codelahoma-ux-initialize ()
  "Initialize UX enhancements."
  (codelahoma-ux-setup)
  (when codelahoma-ux-show-welcome-on-startup
    (add-hook 'after-init-hook #'codelahoma-ux-welcome-screen)))

;; Set up on load
(add-hook 'after-init-hook #'codelahoma-ux-initialize)

(provide 'codelahoma-ux-polish)
;;; codelahoma-ux-polish.el ends here