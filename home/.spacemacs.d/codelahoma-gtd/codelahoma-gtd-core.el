;;; codelahoma-gtd-core.el --- Core GTD functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Core functionality for the personal GTD system including directory management,
;; file operations, and foundational utilities.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-roam)

;;; Task State Management

(defcustom codelahoma-gtd-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "ACTIVE(a)" "|" "DONE(d)")
    (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
    (sequence "PROJECT(p)" "|" "COMPLETED(C)"))
  "GTD task state keywords."
  :type 'sexp
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-todo-keyword-faces
  '(("TODO" . (:foreground "#dc752f" :weight bold))
    ("NEXT" . (:foreground "#4f97d7" :weight bold))
    ("ACTIVE" . (:foreground "#f2241f" :weight bold))
    ("DONE" . (:foreground "#86dc2f" :weight bold))
    ("WAITING" . (:foreground "#b1951d" :weight bold))
    ("HOLD" . (:foreground "#a45bad" :weight bold))
    ("CANCELLED" . (:foreground "#9f8766" :weight bold :strike-through t))
    ("PROJECT" . (:foreground "#2d9574" :weight bold :box t))
    ("COMPLETED" . (:foreground "#86dc2f" :weight bold :box t)))
  "Face properties for GTD todo keywords."
  :type 'alist
  :group 'codelahoma-gtd)

(defun codelahoma-gtd-setup-todo-keywords ()
  "Configure org-mode with GTD todo keywords."
  (setq org-todo-keywords codelahoma-gtd-todo-keywords)
  (setq org-todo-keyword-faces codelahoma-gtd-todo-keyword-faces)
  
  ;; State change logging
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  
  ;; Fast todo selection
  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil))

(defun codelahoma-gtd-set-next-action ()
  "Mark current task as NEXT action."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-todo "NEXT")
    (org-priority ?A)
    (when (org-entry-get nil "DELEGATED_TO")
      (org-set-property "DELEGATED_TO" nil))
    (message "Task marked as NEXT action")))

(defun codelahoma-gtd-delegate-task ()
  "Delegate current task and set to WAITING."
  (interactive)
  (when (eq major-mode 'org-mode)
    (let ((delegate-to (read-string "Delegate to: ")))
      (org-todo "WAITING")
      (org-set-property "DELEGATED_TO" delegate-to)
      (org-set-property "DELEGATED_ON" (format-time-string "[%Y-%m-%d %a]"))
      (org-entry-put nil "WAITING_REASON" (format "Delegated to %s" delegate-to))
      (message "Task delegated to %s" delegate-to))))

(defun codelahoma-gtd-convert-to-project ()
  "Convert current task to a project."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-todo "PROJECT")
    (org-set-property "PROJECT_CREATED" (format-time-string "[%Y-%m-%d %a]"))
    ;; Add project planning template
    (org-end-of-subtree)
    (insert "\n** Outcomes\n- [ ] \n\n** Next Actions\n*** NEXT \n\n** Notes\n")
    (message "Converted to project - add outcomes and next actions")))

;;; Project and Area Structure

(defcustom codelahoma-gtd-project-properties
  '("PROJECT_TYPE" "OUTCOME" "DEADLINE" "STAKEHOLDER" "STATUS")
  "Properties to track for projects."
  :type '(repeat string)
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-areas-of-focus
  '("Personal Development"
    "Health & Fitness"
    "Relationships"
    "Career"
    "Finance"
    "Home & Environment"
    "Hobbies & Recreation"
    "Community & Service")
  "Areas of responsibility for life management."
  :type '(repeat string)
  :group 'codelahoma-gtd)

(defun codelahoma-gtd-ensure-directories ()
  "Ensure all GTD directories exist."
  (interactive)
  (let ((base-dir (expand-file-name "~/personal/org-files/"))
        (dirs '("gtd" "gtd/archive" "gtd/reviews"
                "knowledge" "knowledge/permanent" 
                "knowledge/literature" "knowledge/references"
                "knowledge/projects" "knowledge/daily"
                "areas" "resources" "resources/templates"
                "resources/checklists")))
    (dolist (dir dirs)
      (let ((full-path (expand-file-name dir base-dir)))
        (unless (file-exists-p full-path)
          (make-directory full-path t)
          (message "Created directory: %s" full-path))))))

(defun codelahoma-gtd-verify-files ()
  "Verify all required GTD files exist."
  (interactive)
  (let ((files '(("inbox.org" . "Inbox")
                 ("projects.org" . "Projects") 
                 ("next-actions.org" . "Next Actions")
                 ("waiting-for.org" . "Waiting For")
                 ("someday.org" . "Someday Maybe")
                 ("calendar.org" . "Calendar") 
                 ("media.org" . "Media"))))
    (dolist (file-info files)
      (let* ((filename (car file-info))
             (title (cdr file-info))
             (full-path (expand-file-name filename codelahoma-gtd-directory)))
        (unless (file-exists-p full-path)
          (with-temp-buffer
            (insert (format "#+TITLE: %s\n" title))
            (insert "#+FILETAGS: :gtd:\n")
            (insert "#+STARTUP: overview\n")
            (insert "\n")
            (write-file full-path))
          (message "Created file: %s" full-path))))))

(defun codelahoma-gtd-initialize ()
  "Initialize the GTD system, ensuring all directories and files exist."
  (interactive)
  (codelahoma-gtd-ensure-directories)
  (codelahoma-gtd-verify-files)
  ;; Initialize org-roam
  (codelahoma-gtd-roam-setup)
  (codelahoma-gtd-roam-initialize)
  ;; Phase 2: Setup todo keywords, capture, and processing
  (codelahoma-gtd-setup-todo-keywords)
  (require 'codelahoma-gtd-capture)
  (codelahoma-gtd-setup-capture)
  (require 'codelahoma-gtd-process)
  (codelahoma-gtd-setup-refile-targets)
  (codelahoma-gtd-setup-processing-hooks)
  (message "GTD system initialized successfully"))

(defun codelahoma-gtd-open-file (file)
  "Open a GTD FILE from the configured directory."
  (find-file (expand-file-name file codelahoma-gtd-directory)))

;; File access shortcuts
(defun codelahoma-gtd-open-inbox ()
  "Open the GTD inbox file."
  (interactive)
  (codelahoma-gtd-open-file "inbox.org"))

(defun codelahoma-gtd-open-projects ()
  "Open the GTD projects file."
  (interactive)
  (codelahoma-gtd-open-file "projects.org"))

(defun codelahoma-gtd-open-next-actions ()
  "Open the GTD next actions file."
  (interactive)
  (codelahoma-gtd-open-file "next-actions.org"))

(defun codelahoma-gtd-open-waiting-for ()
  "Open the GTD waiting for file."
  (interactive)
  (codelahoma-gtd-open-file "waiting-for.org"))

(defun codelahoma-gtd-open-someday ()
  "Open the GTD someday/maybe file."
  (interactive)
  (codelahoma-gtd-open-file "someday.org"))

(defun codelahoma-gtd-open-calendar ()
  "Open the GTD calendar file."
  (interactive)
  (codelahoma-gtd-open-file "calendar.org"))

(defun codelahoma-gtd-open-media ()
  "Open the GTD media file."
  (interactive)
  (codelahoma-gtd-open-file "media.org"))

;; Auto-save functionality
(defvar codelahoma-gtd-auto-save-timer nil
  "Timer for auto-saving org buffers.")

(defun codelahoma-gtd-auto-save-org-buffers ()
  "Save all org-mode buffers in the GTD directory."
  (save-excursion
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (and (buffer-file-name)
                 (string-prefix-p (expand-file-name codelahoma-gtd-directory)
                                  (buffer-file-name))
                 (buffer-modified-p))
        (save-buffer)))))

(defun codelahoma-gtd-enable-auto-save ()
  "Enable automatic saving of GTD org buffers."
  (interactive)
  (when (and codelahoma-gtd-auto-save-interval
             (> codelahoma-gtd-auto-save-interval 0))
    (when codelahoma-gtd-auto-save-timer
      (cancel-timer codelahoma-gtd-auto-save-timer))
    (setq codelahoma-gtd-auto-save-timer
          (run-with-timer codelahoma-gtd-auto-save-interval
                          codelahoma-gtd-auto-save-interval
                          'codelahoma-gtd-auto-save-org-buffers))
    (message "GTD auto-save enabled (every %d seconds)" 
             codelahoma-gtd-auto-save-interval)))

(defun codelahoma-gtd-disable-auto-save ()
  "Disable automatic saving of GTD org buffers."
  (interactive)
  (when codelahoma-gtd-auto-save-timer
    (cancel-timer codelahoma-gtd-auto-save-timer)
    (setq codelahoma-gtd-auto-save-timer nil)
    (message "GTD auto-save disabled")))

;; Directory validation
(defun codelahoma-gtd-validate-structure ()
  "Validate GTD directory structure and report status."
  (interactive)
  (let ((base-dir (expand-file-name "~/personal/org-files/"))
        (all-good t)
        (messages '()))
    
    ;; Check base directory
    (unless (file-exists-p base-dir)
      (setq all-good nil)
      (push (format "✗ Base directory missing: %s" base-dir) messages))
    
    ;; Check required directories
    (let ((required-dirs '("gtd" "gtd/archive" "gtd/reviews"
                          "knowledge" "knowledge/permanent" 
                          "knowledge/literature" "knowledge/references"
                          "knowledge/projects" "knowledge/daily"
                          "areas" "resources" "resources/templates"
                          "resources/checklists")))
      (dolist (dir required-dirs)
        (let ((full-path (expand-file-name dir base-dir)))
          (unless (file-exists-p full-path)
            (setq all-good nil)
            (push (format "✗ Directory missing: %s" dir) messages)))))
    
    ;; Check required GTD files
    (let ((required-files '("inbox.org" "projects.org" "next-actions.org"
                           "waiting-for.org" "someday.org" "calendar.org" 
                           "media.org")))
      (dolist (file required-files)
        (let ((full-path (expand-file-name (concat "gtd/" file) base-dir)))
          (unless (file-exists-p full-path)
            (setq all-good nil)
            (push (format "✗ File missing: gtd/%s" file) messages)))))
    
    ;; Report status
    (if all-good
        (message "✓ GTD directory structure validated successfully")
      (message "GTD structure validation failed:\n%s" 
               (mapconcat 'identity (reverse messages) "\n")))
    
    all-good))

;; Run validation on load
(defvar codelahoma-gtd-structure-valid nil
  "Whether the GTD directory structure has been validated.")

(defun codelahoma-gtd-check-structure-on-startup ()
  "Check GTD structure on startup and create if needed."
  (unless codelahoma-gtd-structure-valid
    (if (codelahoma-gtd-validate-structure)
        (setq codelahoma-gtd-structure-valid t)
      (when (y-or-n-p "GTD structure incomplete. Create missing directories/files? ")
        (codelahoma-gtd-ensure-directories)
        (codelahoma-gtd-verify-files)
        (setq codelahoma-gtd-structure-valid (codelahoma-gtd-validate-structure))))))

;; Add to initialization
(add-hook 'after-init-hook 'codelahoma-gtd-check-structure-on-startup)

(provide 'codelahoma-gtd-core)
;;; codelahoma-gtd-core.el ends here