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

;; Load weekly review functionality
(when (file-exists-p "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-weekly-review.el")
  (require 'codelahoma-gtd-weekly-review))

;; Load monthly/quarterly review functionality
(when (file-exists-p "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-monthly-review.el")
  (require 'codelahoma-gtd-monthly-review))

;; Load analytics functionality
(when (file-exists-p "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-analytics.el")
  (require 'codelahoma-gtd-analytics))

;; These are now defined in the monthly-review module, so only define if not loaded
(unless (fboundp 'codelahoma-gtd-monthly-review)
  (defun codelahoma-gtd-monthly-review ()
    "Execute monthly review process."
    (interactive)
    (message "Loading monthly review module...")))

(unless (fboundp 'codelahoma-gtd-quarterly-review)
  (defun codelahoma-gtd-quarterly-review ()
    "Execute quarterly review process."
    (interactive) 
    (message "Loading quarterly review module...")))

;; Temporary compatibility functions
(unless (fboundp 'codelahoma-gtd-daily-review)
  (defun codelahoma-gtd-daily-review ()
    "Execute daily review process."
    (interactive)
    (if (fboundp 'codelahoma-gtd-morning-review)
        (codelahoma-gtd-morning-review)
      (message "Daily review - loading..."))))

;; This compatibility function is now only used if weekly review module fails to load
(unless (fboundp 'codelahoma-gtd-weekly-review)
  (defun codelahoma-gtd-weekly-review ()
    "Execute weekly review process."
    (interactive)
    (message "Loading weekly review module...")))

(provide 'codelahoma-gtd-review)
;;; codelahoma-gtd-review.el ends here