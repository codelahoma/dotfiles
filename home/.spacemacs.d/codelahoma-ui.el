;;; codelahoma-ui.el --- Unified UI for GTD-Zettelkasten system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Unified keybindings and interface for the personal GTD-Zettelkasten system.
;; All commands live under the SPC o o namespace.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)
(require 'codelahoma-gtd-capture)
(require 'codelahoma-gtd-process)
(require 'codelahoma-gtd-review)
(require 'codelahoma-gtd-roam)
;; Phase 3 modules
(when (file-exists-p "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-contexts.el")
  (require 'codelahoma-gtd-contexts))
(when (file-exists-p "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-agenda.el")
  (require 'codelahoma-gtd-agenda))
(when (file-exists-p "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-quick.el")
  (require 'codelahoma-gtd-quick))
(when (file-exists-p "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-autosave.el")
  (require 'codelahoma-gtd-autosave))
;; Bridge module (Phase 5)
(when (file-exists-p "~/.spacemacs.d/codelahoma-gtd/codelahoma-bridge.el")
  (require 'codelahoma-bridge))
(when (file-exists-p "~/.spacemacs.d/codelahoma-gtd/codelahoma-bridge-workflows.el")
  (require 'codelahoma-bridge-workflows))
(when (file-exists-p "~/.spacemacs.d/codelahoma-gtd/codelahoma-bridge-projects.el")
  (require 'codelahoma-bridge-projects))

(defun codelahoma-gtd-setup-keybindings ()
  "Set up GTD keybindings under SPC o o."
  (spacemacs/declare-prefix "o o" "GTD/Zettelkasten")
  
  ;; Capture submenu
  (spacemacs/declare-prefix "o o c" "capture")
  (spacemacs/set-leader-keys
    "o o c i" 'codelahoma-gtd-capture-inbox
    "o o c c" 'codelahoma-gtd-capture-generic
    "o o c t" 'codelahoma-gtd-capture-task
    "o o c p" 'codelahoma-gtd-capture-phone
    "o o c m" 'codelahoma-gtd-capture-meeting
    "o o c e" 'codelahoma-gtd-capture-email
    "o o c x" 'codelahoma-gtd-capture-with-context)
  
  ;; Process submenu
  (spacemacs/declare-prefix "o o p" "process")
  (spacemacs/set-leader-keys
    "o o p i" 'codelahoma-gtd-process-inbox
    "o o p b" 'codelahoma-gtd-bulk-process
    "o o p c" 'codelahoma-gtd-clarify-item
    "o o p s" 'codelahoma-gtd-process-statistics)
  
  ;; Decision support (Phase 3)
  (spacemacs/set-leader-keys
    "o o p d" 'codelahoma-gtd-decision-tree
    "o o p n" 'codelahoma-gtd-smart-next-action)
  
  ;; Task state submenu
  (spacemacs/declare-prefix "o o t" "task states")
  (spacemacs/set-leader-keys
    "o o t n" 'codelahoma-gtd-set-next-action
    "o o t t" 'org-todo
    "o o t s" 'org-schedule
    "o o t d" 'org-deadline
    "o o t p" 'org-priority)
  
  ;; Navigate submenu
  (spacemacs/declare-prefix "o o n" "navigate")
  (spacemacs/set-leader-keys
    "o o n i" 'codelahoma-gtd-open-inbox
    "o o n p" 'codelahoma-gtd-open-projects
    "o o n n" 'codelahoma-gtd-open-next-actions
    "o o n w" 'codelahoma-gtd-open-waiting-for
    "o o n s" 'codelahoma-gtd-open-someday
    "o o n c" 'codelahoma-gtd-open-calendar
    "o o n m" 'codelahoma-gtd-open-media)
  
  ;; Review submenu
  (spacemacs/declare-prefix "o o r" "review")
  (spacemacs/set-leader-keys
    "o o r d" 'codelahoma-gtd-daily-review
    "o o r w" 'codelahoma-gtd-weekly-review
    "o o r m" 'codelahoma-gtd-monthly-review
    "o o r q" 'codelahoma-gtd-quarterly-review)
  
  ;; Enhanced daily review commands (Phase 4)
  (when (featurep 'codelahoma-gtd-daily-review)
    (spacemacs/set-leader-keys
      "o o r M" 'codelahoma-gtd-morning-review
      "o o r E" 'codelahoma-gtd-evening-review
      "o o r s m" 'codelahoma-gtd-save-morning-review
      "o o r s e" 'codelahoma-gtd-save-evening-review
      "o o r j" 'codelahoma-gtd-daily-journal-entry
      ;; Quick checks
      "o o r c m" 'codelahoma-gtd-quick-morning-check
      "o o r c e" 'codelahoma-gtd-quick-evening-check))
  
  ;; Weekly review commands (Phase 4)
  (when (featurep 'codelahoma-gtd-weekly-review)
    (spacemacs/set-leader-keys
      "o o r c w" 'codelahoma-gtd-quick-weekly-check))
  
  ;; Monthly/quarterly review commands (Phase 4)
  (when (featurep 'codelahoma-gtd-monthly-review)
    (spacemacs/set-leader-keys
      "o o r a" 'codelahoma-gtd-annual-review
      "o o r s m" 'codelahoma-gtd-save-monthly-review
      "o o r s q" 'codelahoma-gtd-save-quarterly-review))
  
  ;; Analytics and insights (Phase 4)
  (when (featurep 'codelahoma-gtd-analytics)
    (spacemacs/set-leader-keys
      "o o r i" 'codelahoma-gtd-insights-dashboard
      "o o r e" 'codelahoma-gtd-export-analytics))
  
  ;; Reminders and automation (Phase 4)
  (when (featurep 'codelahoma-gtd-reminders)
    (spacemacs/declare-prefix "o o R" "reminders")
    (spacemacs/set-leader-keys
      "o o R e" 'codelahoma-gtd-enable-auto-reminders
      "o o R d" 'codelahoma-gtd-disable-auto-reminders
      "o o R s" 'codelahoma-gtd-reminder-status
      "o o R r" 'codelahoma-gtd-start-reminders
      "o o R x" 'codelahoma-gtd-stop-reminders
      "o o R i" 'codelahoma-gtd-inbox-overflow-check
      "o o R p" 'codelahoma-gtd-stalled-projects-check
      "o o R c" 'codelahoma-gtd-check-streak-status))
  
  ;; Agenda submenu
  (spacemacs/declare-prefix "o o a" "agenda")
  (spacemacs/set-leader-keys
    "o o a a" 'org-agenda
    "o o a g" (lambda () (interactive) (org-agenda nil "g")))
  
  ;; Enhanced agenda views (Phase 3)
  (when (featurep 'codelahoma-gtd-agenda)
    (spacemacs/set-leader-keys
      "o o a d" 'codelahoma-gtd-daily-dashboard
      "o o a w" 'codelahoma-gtd-weekly-planning
      "o o a c" 'codelahoma-gtd-agenda-by-context
      "o o a e" 'codelahoma-gtd-agenda-by-energy
      "o o a f" 'codelahoma-gtd-focus-session
      "o o a m" 'codelahoma-gtd-morning-review
      "o o a s" 'codelahoma-gtd-someday-review))
  
  ;; Zettelkasten submenu (when org-roam is available)
  (spacemacs/declare-prefix "o o z" "zettelkasten")
  (spacemacs/declare-prefix "o o z f" "find")
  (with-eval-after-load 'org-roam
    (spacemacs/set-leader-keys
      "o o z n" 'org-roam-node-find
      "o o z i" 'org-roam-node-insert
      "o o z c" 'org-roam-capture
      "o o z d" 'org-roam-dailies-goto-today
      "o o z b" 'org-roam-buffer-toggle
      ;; Quick capture submenu
      "o o z p" 'codelahoma-gtd-roam-capture-permanent
      "o o z l" 'codelahoma-gtd-roam-capture-literature
      "o o z r" 'codelahoma-gtd-roam-capture-reference
      "o o z j" 'codelahoma-gtd-roam-capture-project
      ;; Search submenu
      "o o z f p" 'codelahoma-gtd-roam-find-permanent
      "o o z f l" 'codelahoma-gtd-roam-find-literature
      "o o z f j" 'codelahoma-gtd-roam-find-by-project))
  
  ;; Integration submenu (Phase 5)
  (when (featurep 'codelahoma-bridge)
    (spacemacs/declare-prefix "o o i" "integrate")
    (spacemacs/set-leader-keys
      "o o i l" 'codelahoma-bridge-link-task-to-note
      "o o i e" 'codelahoma-bridge-extract-tasks-from-note
      "o o i n" 'codelahoma-bridge-navigate-link
      "o o i c" 'codelahoma-bridge-create-linked-note
      "o o i u" 'codelahoma-bridge-unlink-task
      "o o i s" 'codelahoma-bridge-show-linked-tasks
      "o o i U" 'codelahoma-bridge-update-links
      "o o i k" 'codelahoma-bridge-count-links
      "o o i j" 'codelahoma-bridge-jump-to-linked))
  
  ;; Knowledge workflows (Phase 5)
  (when (featurep 'codelahoma-bridge-workflows)
    (spacemacs/declare-prefix "o o k" "knowledge")
    (spacemacs/set-leader-keys
      "o o k r" 'codelahoma-bridge-create-reading-task
      "o o k R" 'codelahoma-bridge-research-workflow
      "o o k l" 'codelahoma-bridge-learning-project
      "o o k i" 'codelahoma-bridge-insight-capture
      "o o k k" 'codelahoma-bridge-knowledge-review))
  
  ;; Project submenu
  (spacemacs/declare-prefix "o o j" "projects")
  (spacemacs/set-leader-keys
    "o o j n" 'codelahoma-gtd-new-project
    "o o j t" 'codelahoma-gtd-new-project-from-template
    "o o j l" 'codelahoma-gtd-list-projects
    "o o j s" 'codelahoma-gtd-project-status
    "o o j f" 'codelahoma-gtd-find-stalled-projects
    "o o j a" 'codelahoma-gtd-archive-completed-projects)
  
  ;; Project knowledge integration (Phase 5)
  (when (featurep 'codelahoma-bridge-projects)
    (spacemacs/set-leader-keys
      "o o j w" 'codelahoma-bridge-create-project-wiki
      "o o j W" 'codelahoma-bridge-open-project-wiki
      "o o j r" 'codelahoma-bridge-add-project-reference
      "o o j d" 'codelahoma-bridge-decision-log
      "o o j L" 'codelahoma-bridge-project-lessons-learned
      "o o j S" 'codelahoma-bridge-save-lessons-learned
      "o o j k" 'codelahoma-bridge-project-knowledge-summary))
  
  ;; Context submenu (Phase 3)
  (when (featurep 'codelahoma-gtd-contexts)
    (spacemacs/declare-prefix "o o x" "contexts")
    (spacemacs/set-leader-keys
      "o o x a" 'codelahoma-gtd-assign-context
      "o o x f" 'codelahoma-gtd-filter-by-context
      "o o x s" 'codelahoma-gtd-suggest-next-task
      "o o x e" 'codelahoma-gtd-set-energy-level))
  
  ;; Quick access submenu (Phase 3)
  (when (featurep 'codelahoma-gtd-quick)
    (spacemacs/declare-prefix "o o q" "quick")
    (spacemacs/set-leader-keys
      "o o q q" 'codelahoma-gtd-quick-nav/body
      "o o q s" 'codelahoma-gtd-quick-status
      "o o q t" 'codelahoma-gtd-quick-today
      "o o q w" 'codelahoma-gtd-quick-week-ahead
      "o o q c" 'codelahoma-gtd-quick-capture-task
      "o o q n" 'codelahoma-gtd-quick-note
      "o o q d" 'codelahoma-gtd-quick-dashboard
      "o o q i" 'codelahoma-gtd-quick-inbox-count
      "o o q p" 'codelahoma-gtd-quick-stalled-projects
      "o o q x" 'codelahoma-gtd-quick-switch-context
      "o o q e" 'codelahoma-gtd-quick-energy-level
      "o o q j" 'codelahoma-gtd-jump-to-project
      ;; Quick workflow commands
      "o o q RET" 'codelahoma-gtd-quick-process-inbox
      "o o q SPC" 'codelahoma-gtd-quick-complete-and-next))
  
  ;; Save and backup submenu (Phase 3)
  (when (featurep 'codelahoma-gtd-autosave)
    (spacemacs/declare-prefix "o o b" "backup")
    (spacemacs/set-leader-keys
      "o o b e" 'codelahoma-gtd-enable-auto-save
      "o o b d" 'codelahoma-gtd-disable-auto-save
      "o o b s" 'codelahoma-gtd-auto-save-status
      "o o b l" 'codelahoma-gtd-list-backups
      "o o b r" 'codelahoma-gtd-restore-backup
      "o o b c" 'codelahoma-gtd-check-all-files
      "o o b !" 'codelahoma-gtd-emergency-backup))
  
  ;; Save command
  (spacemacs/set-leader-keys
    "o o s" 'org-save-all-org-buffers)
  
  ;; Help/Documentation
  (spacemacs/set-leader-keys
    "o o h" 'codelahoma-gtd-help
    "o o ?" 'codelahoma-gtd-workflow-status)
  
  ;; Development submenu
  (spacemacs/declare-prefix "o o d" "development")
  (spacemacs/set-leader-keys
    "o o d r" 'codelahoma-gtd-reload
    "o o d b" 'codelahoma-gtd-benchmark-operations
    "o o d h" 'codelahoma-gtd-check-health
    "o o d i" 'codelahoma-gtd-initialize
    "o o d v" 'codelahoma-gtd-validate-structure))

(defun codelahoma-gtd-reload ()
  "Reload all GTD configuration files."
  (interactive)
  (dolist (feature '(codelahoma-gtd-config
                     codelahoma-gtd-core
                     codelahoma-gtd-capture
                     codelahoma-gtd-process
                     codelahoma-gtd-review
                     codelahoma-gtd-roam
                     codelahoma-gtd-contexts
                     codelahoma-gtd-agenda
                     codelahoma-gtd-quick
                     codelahoma-gtd-autosave
                     codelahoma-ui))
    (when (featurep feature)
      (unload-feature feature t)))
  (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-config.el")
  (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-core.el")
  (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-capture.el")
  (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-process.el")
  (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-review.el")
  (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-roam.el")
  ;; Phase 3 modules
  (when (file-exists-p "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-contexts.el")
    (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-contexts.el"))
  (when (file-exists-p "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-agenda.el")
    (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-agenda.el"))
  (when (file-exists-p "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-quick.el")
    (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-quick.el"))
  (when (file-exists-p "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-autosave.el")
    (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-autosave.el"))
  (load-file "~/.spacemacs.d/codelahoma-ui.el")
  (codelahoma-gtd-setup-keybindings)
  (message "GTD system reloaded"))

(defun codelahoma-gtd-benchmark-capture ()
  "Benchmark capture performance."
  (interactive)
  (let ((start-time (current-time)))
    (find-file codelahoma-gtd-inbox-file)
    (goto-char (point-max))
    (insert "\n* TODO Test capture item :benchmark:\n")
    (save-buffer)
    (message "Capture completed in %.3f seconds"
             (float-time (time-subtract (current-time) start-time)))))

(defun codelahoma-gtd-help ()
  "Display GTD-Zettelkasten keybinding help."
  (interactive)
  (with-output-to-temp-buffer "*GTD Help*"
    (princ "GTD-Zettelkasten System Help\n")
    (princ "============================\n\n")
    (princ "All commands start with: SPC o o\n\n")
    
    (princ "Quick Reference:\n")
    (princ "---------------\n")
    (princ "  SPC o o c i   - Capture to inbox\n")
    (princ "  SPC o o n i   - Navigate to inbox\n")
    (princ "  SPC o o p i   - Process inbox items\n")
    (princ "  SPC o o r w   - Weekly review\n")
    (princ "  SPC o o z n   - Find/create Zettelkasten note\n")
    (princ "  SPC o o s     - Save all org buffers\n")
    (princ "  SPC o o ?     - This help\n\n")
    
    (princ "For full keybinding list, see which-key (SPC o o)\n")
    (princ "Or check the implementation plan in .flowloom/plans/\n\n")
    
    (princ "Current Phase: Phase 2 - GTD Engine Core\n")
    (princ "\nPhase 2 Features:\n")
    (princ "-----------------\n")
    (princ "  ✓ Task state management (TODO/NEXT/WAITING/PROJECT)\n")
    (princ "  ✓ Intelligent capture with context detection\n")
    (princ "  ✓ Inbox processing workflow\n")
    (princ "  ✓ Project and area structure\n")
    (princ "  ✓ Refile suggestions and bulk operations\n")
    (princ "\nStatus: " )
    (princ (propertize "Operational" 'face '(:foreground "#86dc2f" :weight bold))))
  (switch-to-buffer-other-window "*GTD Help*"))

(provide 'codelahoma-ui)
;;; codelahoma-ui.el ends here