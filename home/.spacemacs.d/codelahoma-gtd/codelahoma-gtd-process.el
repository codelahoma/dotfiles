;;; codelahoma-gtd-process.el --- GTD processing functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Inbox processing and task clarification for the personal GTD system.
;; Implements efficient workflows for processing inbox items according to GTD methodology.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)

;;; Refile Configuration

(defcustom codelahoma-gtd-refile-targets
  '(("~/personal/org-files/gtd/projects.org" :maxlevel . 3)
    ("~/personal/org-files/gtd/next-actions.org" :maxlevel . 2)
    ("~/personal/org-files/gtd/someday.org" :maxlevel . 2)
    ("~/personal/org-files/gtd/waiting-for.org" :maxlevel . 2)
    ("~/personal/org-files/areas/" :maxlevel . 2))
  "Refile targets for GTD processing."
  :type 'alist
  :group 'codelahoma-gtd)

;;; Processing Mode

(defvar codelahoma-gtd-processing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'codelahoma-gtd-process-next-item)
    (define-key map "p" 'codelahoma-gtd-process-previous-item)
    (define-key map "r" 'codelahoma-gtd-refile-current)
    (define-key map "d" 'codelahoma-gtd-delete-current)
    (define-key map "t" 'org-todo)
    (define-key map "s" 'org-schedule)
    (define-key map "." 'org-priority)
    (define-key map ":" 'org-set-tags-command)
    (define-key map "w" 'codelahoma-gtd-delegate-task)
    (define-key map "c" 'codelahoma-gtd-convert-to-project)
    (define-key map "?" 'codelahoma-gtd-process-help)
    (define-key map "h" 'codelahoma-gtd-process-help)
    (define-key map "q" 'codelahoma-gtd-finish-processing)
    map)
  "Keymap for GTD processing mode.")

(define-minor-mode codelahoma-gtd-processing-mode
  "Minor mode for GTD inbox processing."
  :lighter " GTD-Process"
  :keymap codelahoma-gtd-processing-mode-map)

;;; Processing Functions

(defun codelahoma-gtd-process-inbox ()
  "Open inbox in processing mode."
  (interactive)
  (find-file (expand-file-name "inbox.org" codelahoma-gtd-directory))
  (goto-char (point-min))
  (org-next-visible-heading 1)
  (codelahoma-gtd-processing-mode 1)
  (message "Processing inbox - use 'h' for help"))

(defun codelahoma-gtd-process-next-item ()
  "Move to next inbox item."
  (interactive)
  (widen)
  (org-next-visible-heading 1)
  (when (org-at-heading-p)
    (org-narrow-to-subtree)
    (org-show-entry)
    (org-show-children)
    (message "Item %d of %d" 
             (codelahoma-gtd-current-item-number)
             (codelahoma-gtd-total-inbox-items))))

(defun codelahoma-gtd-process-previous-item ()
  "Move to previous inbox item."
  (interactive)
  (widen)
  (org-previous-visible-heading 1)
  (when (org-at-heading-p)
    (org-narrow-to-subtree)
    (org-show-entry)
    (org-show-children)))

(defun codelahoma-gtd-refile-current ()
  "Refile current item with GTD logic."
  (interactive)
  (let* ((heading (org-get-heading t t t t))
         (suggested-target (codelahoma-gtd-suggest-refile-target heading)))
    (when suggested-target
      (message "Suggested: %s" suggested-target))
    (org-refile)))

(defun codelahoma-gtd-suggest-refile-target (heading)
  "Suggest a refile target based on heading content."
  (cond
   ((string-match-p "\\bproject\\b\\|\\bplan\\b\\|\\bproposal\\b" heading)
    "projects.org")
   ((string-match-p "\\bwaiting\\b\\|\\bfollow.?up\\b\\|\\bdelegated\\b" heading)
    "waiting-for.org")
   ((string-match-p "\\bsomeday\\b\\|\\bmaybe\\b\\|\\bidea\\b" heading)
    "someday.org")
   ((org-entry-get nil "Effort")
    "next-actions.org")
   (t nil)))

(defun codelahoma-gtd-delete-current ()
  "Delete current inbox item after confirmation."
  (interactive)
  (when (y-or-n-p "Delete this item? ")
    (widen)
    (org-cut-subtree)
    (message "Item deleted")
    (when (org-at-heading-p)
      (codelahoma-gtd-process-next-item))))

;;; Clarification Functions

(defun codelahoma-gtd-clarify-item ()
  "Interactive clarification for current item."
  (interactive)
  (let* ((actionable (y-or-n-p "Is this actionable? "))
         (effort (when actionable
                  (read-string "Estimated effort (5m, 1h, 2d): " "30m"))))
    (if actionable
        (progn
          (org-todo "TODO")
          (when effort (org-set-property "Effort" effort))
          (when (y-or-n-p "Is this a project? ")
            (codelahoma-gtd-convert-to-project))
          (when (y-or-n-p "Can be done in 2 minutes? ")
            (org-todo "NEXT")
            (org-priority ?A)))
      ;; Not actionable
      (if (y-or-n-p "Reference material? ")
          (org-set-tags ":REFERENCE:")
        (when (y-or-n-p "Someday/maybe? ")
          (org-set-tags ":SOMEDAY:"))))))

;;; Bulk Processing

(defun codelahoma-gtd-bulk-process ()
  "Process multiple inbox items at once."
  (interactive)
  (let ((org-agenda-files (list (expand-file-name "inbox.org" codelahoma-gtd-directory))))
    (org-agenda nil "t"))
  (org-agenda-bulk-mark-regexp ".")
  (message "All items marked - choose bulk action"))

;;; Helper Functions

(defun codelahoma-gtd-current-item-number ()
  "Get current item number in inbox."
  (save-excursion
    (widen)
    (let ((count 1))
      (goto-char (point-min))
      (while (and (not (equal (point) (save-excursion (org-back-to-heading) (point))))
                  (org-next-visible-heading 1))
        (cl-incf count))
      count)))

(defun codelahoma-gtd-total-inbox-items ()
  "Get total number of items in inbox."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (let ((count 0))
      (while (org-next-visible-heading 1)
        (cl-incf count))
      count)))

(defun codelahoma-gtd-process-statistics ()
  "Show inbox processing statistics."
  (interactive)
  (let* ((total (codelahoma-gtd-total-inbox-items))
         (current (codelahoma-gtd-current-item-number))
         (processed (1- current))
         (percent (if (> total 0) 
                     (/ (* 100.0 processed) total) 
                   0)))
    (message "Processed %d of %d items (%.0f%%)" processed total percent)))

(defun codelahoma-gtd-finish-processing ()
  "Finish processing and clean up."
  (interactive)
  (widen)
  (codelahoma-gtd-processing-mode -1)
  (org-save-all-org-buffers)
  (message "Inbox processing complete")
  (when (= 0 (codelahoma-gtd-total-inbox-items))
    (message "Inbox zero achieved! 🎉")))

;;; Process Help

(defun codelahoma-gtd-process-help ()
  "Show help for GTD processing mode."
  (interactive)
  (with-output-to-temp-buffer "*GTD Process Help*"
    (princ "GTD Processing Mode Commands\n")
    (princ "============================\n\n")
    (princ "Navigation:\n")
    (princ "  n - Next item\n")
    (princ "  p - Previous item\n\n")
    (princ "Actions:\n")
    (princ "  r - Refile item\n")
    (princ "  d - Delete item\n")
    (princ "  t - Change TODO state\n")
    (princ "  s - Schedule\n")
    (princ "  . - Set priority\n")
    (princ "  : - Set tags\n")
    (princ "  w - Delegate (set to WAITING)\n")
    (princ "  c - Convert to project\n\n")
    (princ "Other:\n")
    (princ "  ? or h - This help\n")
    (princ "  q - Quit processing mode\n"))
  (switch-to-buffer-other-window "*GTD Process Help*"))

;;; Setup

(defun codelahoma-gtd-setup-refile-targets ()
  "Configure org-refile for GTD workflow."
  (setq org-refile-targets
        (append 
         '((nil :maxlevel . 2))  ; Current buffer
         (mapcar (lambda (target)
                  (cons (expand-file-name (car target) "~/personal/org-files/")
                        (cdr target)))
                codelahoma-gtd-refile-targets)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm))

(defun codelahoma-gtd-setup-processing-hooks ()
  "Set up hooks for processing workflow."
  (add-hook 'org-after-refile-insert-hook 'codelahoma-gtd-after-refile))

(defun codelahoma-gtd-after-refile ()
  "Actions to perform after refiling."
  (org-set-property "REFILED" (format-time-string "[%Y-%m-%d %a %H:%M]"))
  (when (string-match-p "projects\\.org" (buffer-file-name))
    (org-set-property "PROJECT_STATUS" "Active")))

(provide 'codelahoma-gtd-process)
;;; codelahoma-gtd-process.el ends here