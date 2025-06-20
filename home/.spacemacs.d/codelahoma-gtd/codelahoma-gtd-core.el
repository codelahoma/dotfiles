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