;;; codelahoma-gtd-loader.el --- Simple loader for GTD system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0

;;; Commentary:
;; Simple, linear loader for the GTD-Zettelkasten system.
;; This avoids all circular dependencies by loading modules in the correct order.

;;; Code:

(defun codelahoma-gtd-load-system ()
  "Load the GTD system in the correct order."
  (interactive)
  (let ((gtd-dir "~/.spacemacs.d/codelahoma-gtd/"))
    ;; Core modules - no dependencies on each other
    (load-file (concat gtd-dir "codelahoma-gtd-config.el"))
    
    ;; Roam - depends only on config
    (when (file-exists-p (concat gtd-dir "codelahoma-gtd-roam.el"))
      (load-file (concat gtd-dir "codelahoma-gtd-roam.el")))
    
    ;; Core - depends on config (roam functions called conditionally)
    (load-file (concat gtd-dir "codelahoma-gtd-core.el"))
    
    ;; Basic modules - depend on config and core
    (load-file (concat gtd-dir "codelahoma-gtd-capture.el"))
    (load-file (concat gtd-dir "codelahoma-gtd-process.el"))
    (load-file (concat gtd-dir "codelahoma-gtd-review.el"))
    
    ;; Phase 3 modules - optional
    (when (file-exists-p (concat gtd-dir "codelahoma-gtd-contexts.el"))
      (load-file (concat gtd-dir "codelahoma-gtd-contexts.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-gtd-agenda.el"))
      (load-file (concat gtd-dir "codelahoma-gtd-agenda.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-gtd-quick.el"))
      (load-file (concat gtd-dir "codelahoma-gtd-quick.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-gtd-autosave.el"))
      (load-file (concat gtd-dir "codelahoma-gtd-autosave.el")))
    
    ;; Phase 4 modules - review enhancements
    (when (file-exists-p (concat gtd-dir "codelahoma-gtd-daily-review.el"))
      (load-file (concat gtd-dir "codelahoma-gtd-daily-review.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-gtd-weekly-review.el"))
      (load-file (concat gtd-dir "codelahoma-gtd-weekly-review.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-gtd-monthly-review.el"))
      (load-file (concat gtd-dir "codelahoma-gtd-monthly-review.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-gtd-analytics.el"))
      (load-file (concat gtd-dir "codelahoma-gtd-analytics.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-gtd-reminders.el"))
      (load-file (concat gtd-dir "codelahoma-gtd-reminders.el")))
    
    ;; Phase 5 modules - bridge
    (when (file-exists-p (concat gtd-dir "codelahoma-bridge.el"))
      (load-file (concat gtd-dir "codelahoma-bridge.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-bridge-workflows.el"))
      (load-file (concat gtd-dir "codelahoma-bridge-workflows.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-bridge-projects.el"))
      (load-file (concat gtd-dir "codelahoma-bridge-projects.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-bridge-suggestions.el"))
      (load-file (concat gtd-dir "codelahoma-bridge-suggestions.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-bridge-metrics.el"))
      (load-file (concat gtd-dir "codelahoma-bridge-metrics.el")))
    
    ;; Phase 6 modules - UI enhancements (NO circular deps!)
    (when (file-exists-p (concat gtd-dir "codelahoma-dashboard.el"))
      (load-file (concat gtd-dir "codelahoma-dashboard.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-command-palette.el"))
      (load-file (concat gtd-dir "codelahoma-command-palette.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-unified-search.el"))
      (load-file (concat gtd-dir "codelahoma-unified-search.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-status-bar.el"))
      (load-file (concat gtd-dir "codelahoma-status-bar.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-ux-polish.el"))
      (load-file (concat gtd-dir "codelahoma-ux-polish.el")))
    
    (message "GTD modules loaded")))

(defun codelahoma-gtd-setup-keybindings ()
  "Set up GTD keybindings under SPC o o."
  ;; Need to be in org-mode for the keybindings to work
  (with-eval-after-load 'org
    (spacemacs/declare-prefix "o" "user-defined")
    (spacemacs/declare-prefix "o o" "GTD/Zettelkasten")
    
    ;; Main entry point - simple help
    (spacemacs/set-leader-keys
      "o o ?" 'codelahoma-gtd-help)
    
    ;; Capture submenu
    (spacemacs/declare-prefix "o o c" "capture")
    (spacemacs/set-leader-keys
      "o o c i" 'codelahoma-gtd-capture-inbox
      "o o c t" 'codelahoma-gtd-capture-task)
    
    ;; Process submenu
    (spacemacs/declare-prefix "o o p" "process")
    (spacemacs/set-leader-keys
      "o o p i" 'codelahoma-gtd-process-inbox)
    
    ;; Navigate submenu
    (spacemacs/declare-prefix "o o n" "navigate")
    (spacemacs/set-leader-keys
      "o o n i" 'codelahoma-gtd-open-inbox
      "o o n p" 'codelahoma-gtd-open-projects)
    
    ;; Review submenu
    (spacemacs/declare-prefix "o o r" "review")
    (spacemacs/set-leader-keys
      "o o r w" 'codelahoma-gtd-weekly-review)
    
    ;; Save command
    (spacemacs/set-leader-keys
      "o o w" 'org-save-all-org-buffers)
    
    (message "GTD keybindings configured"))
  
  ;; Also add a simple help function
  (defun codelahoma-gtd-help ()
    "Show GTD system help."
    (interactive)
    (message "GTD System: Use SPC o o c i to capture, SPC o o n i to view inbox")))

(defun codelahoma-gtd-init ()
  "Initialize the GTD system."
  (interactive)
  ;; Make sure org is loaded first
  (require 'org)
  
  ;; Load all modules
  (codelahoma-gtd-load-system)
  
  ;; Initialize the system
  (when (fboundp 'codelahoma-gtd-initialize)
    (codelahoma-gtd-initialize))
  
  ;; Setup keybindings - this will happen after org is loaded
  (codelahoma-gtd-setup-keybindings)
  
  ;; Force immediate keybinding setup if org is already loaded
  (when (featurep 'org)
    (spacemacs/declare-prefix "o" "user-defined")
    (spacemacs/declare-prefix "o o" "GTD/Zettelkasten")
    (spacemacs/set-leader-keys
      "o o ?" 'codelahoma-gtd-help
      "o o c i" 'codelahoma-gtd-capture-inbox
      "o o n i" 'codelahoma-gtd-open-inbox
      "o o p i" 'codelahoma-gtd-process-inbox))
  
  ;; Enable auto-save if available
  (when (fboundp 'codelahoma-gtd-enable-auto-save)
    (codelahoma-gtd-enable-auto-save))
  
  ;; Enable status bar if available
  (when (and (fboundp 'codelahoma-status-bar-enable)
             (boundp 'codelahoma-status-bar-enabled)
             codelahoma-status-bar-enabled)
    (codelahoma-status-bar-enable))
  
  (message "Personal GTD-Zettelkasten system loaded"))

(provide 'codelahoma-gtd-loader)
;;; codelahoma-gtd-loader.el ends here