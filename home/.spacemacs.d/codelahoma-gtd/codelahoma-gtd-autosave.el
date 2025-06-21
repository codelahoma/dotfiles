;;; codelahoma-gtd-autosave.el --- Auto-save and data integrity -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Enhanced auto-save functionality and data integrity checks for GTD system.
;; Provides automatic saving, backup creation, and recovery mechanisms.

;;; Code:

(require 'codelahoma-gtd-config)

;;; Auto-save Configuration

(defcustom codelahoma-gtd-auto-save-interval 60
  "Interval in seconds between auto-saves."
  :type 'integer
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-backup-directory
  (expand-file-name "backups" codelahoma-gtd-directory)
  "Directory for GTD backups."
  :type 'directory
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-max-backups 10
  "Maximum number of backups to keep per file."
  :type 'integer
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-enable-versioning t
  "Enable version control for GTD files."
  :type 'boolean
  :group 'codelahoma-gtd)

;;; Auto-save State Variables

(defvar codelahoma-gtd-auto-save-timer nil
  "Timer for auto-saving GTD files.")

(defvar codelahoma-gtd-files-changed nil
  "List of GTD files that have been changed.")

(defvar codelahoma-gtd-last-save-time (current-time)
  "Time of last auto-save.")

(defvar codelahoma-gtd-save-in-progress nil
  "Flag to prevent recursive saves.")

;;; Auto-save Functions

(defun codelahoma-gtd-enable-auto-save ()
  "Enable enhanced auto-save for GTD files."
  (interactive)
  ;; Ensure backup directory exists
  (unless (file-exists-p codelahoma-gtd-backup-directory)
    (make-directory codelahoma-gtd-backup-directory t))
  
  ;; Set up auto-save timer
  (when codelahoma-gtd-auto-save-timer
    (cancel-timer codelahoma-gtd-auto-save-timer))
  
  (setq codelahoma-gtd-auto-save-timer
        (run-with-timer codelahoma-gtd-auto-save-interval
                       codelahoma-gtd-auto-save-interval
                       'codelahoma-gtd-auto-save-changed))
  
  ;; Hook into buffer modifications
  (add-hook 'after-change-functions 'codelahoma-gtd-mark-changed)
  (add-hook 'before-save-hook 'codelahoma-gtd-create-backup)
  (add-hook 'kill-buffer-hook 'codelahoma-gtd-save-on-kill)
  (add-hook 'kill-emacs-hook 'codelahoma-gtd-save-all-on-exit)
  
  ;; Enable auto-save mode for GTD buffers
  (add-hook 'find-file-hook 'codelahoma-gtd-enable-buffer-auto-save)
  
  (message "GTD auto-save enabled (every %d seconds)" 
           codelahoma-gtd-auto-save-interval))

(defun codelahoma-gtd-disable-auto-save ()
  "Disable enhanced auto-save."
  (interactive)
  (when codelahoma-gtd-auto-save-timer
    (cancel-timer codelahoma-gtd-auto-save-timer)
    (setq codelahoma-gtd-auto-save-timer nil))
  (remove-hook 'after-change-functions 'codelahoma-gtd-mark-changed)
  (remove-hook 'before-save-hook 'codelahoma-gtd-create-backup)
  (remove-hook 'kill-buffer-hook 'codelahoma-gtd-save-on-kill)
  (remove-hook 'kill-emacs-hook 'codelahoma-gtd-save-all-on-exit)
  (remove-hook 'find-file-hook 'codelahoma-gtd-enable-buffer-auto-save)
  (message "GTD auto-save disabled"))

(defun codelahoma-gtd-mark-changed (&rest _)
  "Mark current buffer as changed if it's a GTD file."
  (when (and (buffer-file-name)
             (codelahoma-gtd-file-p (buffer-file-name))
             (not codelahoma-gtd-save-in-progress))
    (add-to-list 'codelahoma-gtd-files-changed (buffer-file-name))))

(defun codelahoma-gtd-file-p (file)
  "Check if FILE is a GTD file."
  (and file
       (string-prefix-p (expand-file-name codelahoma-gtd-directory)
                       (expand-file-name file))))

(defun codelahoma-gtd-auto-save-changed ()
  "Auto-save all changed GTD files."
  (when (and codelahoma-gtd-files-changed
             (not codelahoma-gtd-save-in-progress))
    (let ((codelahoma-gtd-save-in-progress t)
          (saved-count 0))
      (dolist (file codelahoma-gtd-files-changed)
        (when-let ((buffer (get-file-buffer file)))
          (with-current-buffer buffer
            (when (and (buffer-modified-p)
                       (not (buffer-read-only)))
              (condition-case err
                  (progn
                    (save-buffer)
                    (cl-incf saved-count))
                (error
                 (message "Failed to auto-save %s: %s" 
                         (file-name-nondirectory file)
                         (error-message-string err))))))))
      (when (> saved-count 0)
        (setq codelahoma-gtd-last-save-time (current-time))
        (message "Auto-saved %d GTD file(s)" saved-count))
      (setq codelahoma-gtd-files-changed nil))))

;;; Backup Functions

(defun codelahoma-gtd-create-backup ()
  "Create a backup of the current GTD file before saving."
  (when (and (buffer-file-name)
             (codelahoma-gtd-file-p (buffer-file-name))
             (file-exists-p (buffer-file-name)))
    (let* ((file (buffer-file-name))
           (backup-file (codelahoma-gtd-backup-filename file)))
      (condition-case err
          (progn
            (copy-file file backup-file t t)
            (codelahoma-gtd-cleanup-old-backups file))
        (error
         (message "Failed to create backup: %s" (error-message-string err)))))))

(defun codelahoma-gtd-backup-filename (file)
  "Generate backup filename for FILE."
  (let* ((filename (file-name-nondirectory file))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (backup-name (format "%s.%s~" filename timestamp)))
    (expand-file-name backup-name codelahoma-gtd-backup-directory)))

(defun codelahoma-gtd-cleanup-old-backups (file)
  "Remove old backups of FILE keeping only the most recent ones."
  (let* ((filename (file-name-nondirectory file))
         (pattern (concat (regexp-quote filename) "\\..*~$"))
         (backups (directory-files codelahoma-gtd-backup-directory t pattern))
         (sorted-backups (sort backups 'string>)))
    (when (> (length sorted-backups) codelahoma-gtd-max-backups)
      (dolist (old-backup (nthcdr codelahoma-gtd-max-backups sorted-backups))
        (delete-file old-backup)))))

;;; Recovery Functions

(defun codelahoma-gtd-list-backups (file)
  "List all backups for FILE."
  (interactive (list (buffer-file-name)))
  (unless (codelahoma-gtd-file-p file)
    (error "Not a GTD file"))
  (let* ((filename (file-name-nondirectory file))
         (pattern (concat (regexp-quote filename) "\\..*~$"))
         (backups (directory-files codelahoma-gtd-backup-directory t pattern))
         (sorted-backups (sort backups 'string>)))
    (if backups
        (with-output-to-temp-buffer "*GTD Backups*"
          (princ (format "Backups for %s:\n\n" filename))
          (dolist (backup sorted-backups)
            (let* ((attrs (file-attributes backup))
                   (size (nth 7 attrs))
                   (time (nth 5 attrs)))
              (princ (format "%s\t%s\t%s\n"
                           (format-time-string "%Y-%m-%d %H:%M:%S" time)
                           (file-size-human-readable size)
                           (file-name-nondirectory backup))))))
      (message "No backups found for %s" filename))))

(defun codelahoma-gtd-restore-backup ()
  "Restore a GTD file from backup."
  (interactive)
  (let* ((file (buffer-file-name))
         (filename (file-name-nondirectory file))
         (pattern (concat (regexp-quote filename) "\\..*~$"))
         (backups (directory-files codelahoma-gtd-backup-directory t pattern))
         (sorted-backups (sort backups 'string>)))
    (if backups
        (let* ((backup-names (mapcar 'file-name-nondirectory sorted-backups))
               (selected (completing-read "Restore from backup: " backup-names))
               (backup-file (expand-file-name selected codelahoma-gtd-backup-directory)))
          (when (y-or-n-p (format "Restore %s from backup? Current file will be backed up first. " 
                                 filename))
            (codelahoma-gtd-create-backup)
            (copy-file backup-file file t)
            (revert-buffer t t)
            (message "Restored from %s" selected)))
      (message "No backups found for %s" filename))))

;;; Data Integrity Functions

(defun codelahoma-gtd-validate-file (file)
  "Validate the structure and integrity of a GTD FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((issues '()))
      ;; Check for basic org-mode structure
      (goto-char (point-min))
      (unless (re-search-forward "^\\*+ " nil t)
        (push "No org headings found" issues))
      
      ;; Check for unclosed property drawers
      (goto-char (point-min))
      (while (re-search-forward "^:PROPERTIES:" nil t)
        (unless (re-search-forward "^:END:" nil t)
          (push "Unclosed property drawer" issues)))
      
      ;; Check for corrupted timestamps
      (goto-char (point-min))
      (while (re-search-forward "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" nil t)
        (let ((date-string (match-string 1)))
          (condition-case nil
              (decode-time (date-to-time date-string))
            (error (push (format "Invalid date: %s" date-string) issues)))))
      
      issues)))

(defun codelahoma-gtd-check-all-files ()
  "Check integrity of all GTD files."
  (interactive)
  (let ((all-issues '()))
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (let ((issues (codelahoma-gtd-validate-file file)))
        (when issues
          (push (cons file issues) all-issues))))
    (if all-issues
        (with-output-to-temp-buffer "*GTD Integrity Check*"
          (princ "GTD File Integrity Issues:\n\n")
          (dolist (file-issues all-issues)
            (princ (format "%s:\n" (car file-issues)))
            (dolist (issue (cdr file-issues))
              (princ (format "  - %s\n" issue)))
            (princ "\n")))
      (message "All GTD files passed integrity check ✓"))))

;;; Buffer-specific Auto-save

(defun codelahoma-gtd-enable-buffer-auto-save ()
  "Enable auto-save for current buffer if it's a GTD file."
  (when (codelahoma-gtd-file-p (buffer-file-name))
    (auto-save-mode 1)
    (setq-local auto-save-visited-interval codelahoma-gtd-auto-save-interval)
    (auto-save-visited-mode 1)))

(defun codelahoma-gtd-save-on-kill ()
  "Save GTD buffer when killing it."
  (when (and (buffer-file-name)
             (codelahoma-gtd-file-p (buffer-file-name))
             (buffer-modified-p)
             (not (buffer-read-only)))
    (save-buffer)))

(defun codelahoma-gtd-save-all-on-exit ()
  "Save all GTD buffers when exiting Emacs."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (buffer-file-name)
                 (codelahoma-gtd-file-p (buffer-file-name))
                 (buffer-modified-p)
                 (not (buffer-read-only)))
        (save-buffer)))))

;;; Status and Monitoring

(defun codelahoma-gtd-auto-save-status ()
  "Show auto-save status."
  (interactive)
  (let* ((enabled (timerp codelahoma-gtd-auto-save-timer))
         (time-since (if codelahoma-gtd-last-save-time
                        (format-seconds "%h hours %m minutes %s seconds ago"
                                      (float-time (time-subtract (current-time)
                                                               codelahoma-gtd-last-save-time)))
                      "never"))
         (changed-count (length codelahoma-gtd-files-changed))
         (backup-count (length (directory-files codelahoma-gtd-backup-directory nil "~$"))))
    (message "Auto-save: %s | Last save: %s | Changed files: %d | Backups: %d"
             (if enabled "ON" "OFF")
             time-since
             changed-count
             backup-count)))

;;; Sync Indicator

(defvar codelahoma-gtd-sync-indicator-mode-line
  '(:eval (codelahoma-gtd-sync-indicator))
  "Mode line indicator for GTD sync status.")

(defun codelahoma-gtd-sync-indicator ()
  "Return sync status indicator for mode line."
  (when (and (codelahoma-gtd-file-p (buffer-file-name))
             (buffer-modified-p))
    (propertize " GTD*" 'face 'warning)))

(defun codelahoma-gtd-enable-sync-indicator ()
  "Enable sync indicator in mode line."
  (interactive)
  (add-to-list 'mode-line-misc-info codelahoma-gtd-sync-indicator-mode-line))

(defun codelahoma-gtd-disable-sync-indicator ()
  "Disable sync indicator in mode line."
  (interactive)
  (setq mode-line-misc-info 
        (delete codelahoma-gtd-sync-indicator-mode-line mode-line-misc-info)))

;;; Emergency Recovery

(defun codelahoma-gtd-emergency-backup ()
  "Create emergency backup of all GTD files."
  (interactive)
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (backup-dir (expand-file-name (format "emergency-%s" timestamp)
                                      codelahoma-gtd-backup-directory)))
    (make-directory backup-dir t)
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (copy-file file (expand-file-name (file-name-nondirectory file) backup-dir) t))
    (message "Emergency backup created in %s" backup-dir)))

(provide 'codelahoma-gtd-autosave)
;;; codelahoma-gtd-autosave.el ends here