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
;; Bridge module is optional (Phase 5)
;; (require 'codelahoma-bridge)

(defun codelahoma-gtd-setup-keybindings ()
  "Set up GTD keybindings under SPC o o."
  (spacemacs/declare-prefix "o o" "GTD/Zettelkasten")
  
  ;; Capture submenu
  (spacemacs/declare-prefix "o o c" "capture")
  (spacemacs/set-leader-keys
    "o o c i" 'codelahoma-gtd-capture-inbox
    "o o c c" 'codelahoma-gtd-capture-generic)
  
  ;; Process submenu
  (spacemacs/declare-prefix "o o p" "process")
  (spacemacs/set-leader-keys
    "o o p i" 'codelahoma-gtd-process-inbox)
  
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
    "o o r w" 'codelahoma-gtd-weekly-review)
  
  ;; Agenda submenu
  (spacemacs/declare-prefix "o o a" "agenda")
  (spacemacs/set-leader-keys
    "o o a a" 'org-agenda
    "o o a g" (lambda () (interactive) (message "GTD view - coming soon")))
  
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
  
  ;; Integration submenu (Phase 5 - commented out for now)
  ;; (spacemacs/declare-prefix "o o i" "integrate")
  ;; (spacemacs/set-leader-keys
  ;;   "o o i l" 'codelahoma-bridge-link-to-knowledge
  ;;   "o o i e" 'codelahoma-bridge-extract-actions)
  
  ;; Save command
  (spacemacs/set-leader-keys
    "o o s" 'org-save-all-org-buffers)
  
  ;; Development submenu (temporary for Phase 1)
  (spacemacs/declare-prefix "o o d" "development")
  (spacemacs/set-leader-keys
    "o o d r" 'codelahoma-gtd-reload
    "o o d b" 'codelahoma-gtd-benchmark-capture
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
                     codelahoma-ui))
    (when (featurep feature)
      (unload-feature feature t)))
  (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-config.el")
  (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-core.el")
  (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-capture.el")
  (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-process.el")
  (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-review.el")
  (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-roam.el")
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

(provide 'codelahoma-ui)
;;; codelahoma-ui.el ends here