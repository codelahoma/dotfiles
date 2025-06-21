;;; codelahoma-gtd-review.el --- GTD review workflows -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Core review module that loads daily and weekly review functionality.
;; Extended review workflows are implemented in separate modules.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)

;; Load daily review functionality
(when (file-exists-p "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-daily-review.el")
  (require 'codelahoma-gtd-daily-review))

;; Forward declarations for extended reviews
(defun codelahoma-gtd-monthly-review ()
  "Execute monthly review process."
  (interactive)
  (if (featurep 'codelahoma-gtd-monthly-review)
      (codelahoma-gtd-execute-monthly-review)
    (message "Monthly review - coming in Phase 4")))

(defun codelahoma-gtd-quarterly-review ()
  "Execute quarterly review process."
  (interactive) 
  (if (featurep 'codelahoma-gtd-monthly-review)
      (codelahoma-gtd-execute-quarterly-review)
    (message "Quarterly review - coming in Phase 4")))

;; Temporary compatibility functions
(unless (fboundp 'codelahoma-gtd-daily-review)
  (defun codelahoma-gtd-daily-review ()
    "Execute daily review process."
    (interactive)
    (if (fboundp 'codelahoma-gtd-morning-review)
        (codelahoma-gtd-morning-review)
      (message "Daily review - loading..."))))

(unless (fboundp 'codelahoma-gtd-weekly-review)
  (defun codelahoma-gtd-weekly-review ()
    "Execute weekly review process."
    (interactive)
    (let ((review-buffer (get-buffer-create "*GTD Weekly Review*")))
      (with-current-buffer review-buffer
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: Weekly Review\n")
        (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
        
        (insert "* Get Clear\n")
        (insert "** [ ] Collect Loose Papers and Materials\n")
        (insert "** [ ] Get Inbox to Zero\n")
        (insert "** [ ] Empty Your Head\n\n")
        
        (insert "* Get Current\n")
        (insert "** [ ] Review Action Lists\n")
        (insert "** [ ] Review Previous Calendar\n")
        (insert "** [ ] Review Upcoming Calendar\n")
        (insert "** [ ] Review Waiting For List\n")
        (insert "** [ ] Review Project List\n")
        (insert "** [ ] Review Someday/Maybe List\n\n")
        
        (insert "* Get Creative\n")
        (insert "** [ ] Review Your Purpose and Principles\n")
        (insert "** [ ] Review Your Vision\n")
        (insert "** [ ] Review Your Goals and Objectives\n")
        (insert "** [ ] Review Your Areas of Focus\n")
        (insert "** [ ] Be Creative and Courageous\n"))
      
      (switch-to-buffer review-buffer)
      (goto-char (point-min)))))

(provide 'codelahoma-gtd-review)
;;; codelahoma-gtd-review.el ends here