;;; codelahoma-gtd-config.el --- Personal GTD system configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Configuration and customization options for the personal GTD-Zettelkasten system.
;; This file defines all customizable variables and settings.

;;; Code:

(defgroup codelahoma-gtd nil
  "Personal GTD system configuration."
  :group 'org
  :prefix "codelahoma-gtd-")

(defcustom codelahoma-gtd-directory "~/personal/org-files/gtd/"
  "Directory containing GTD org files."
  :type 'directory
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-archive-directory 
  (expand-file-name "archive/" codelahoma-gtd-directory)
  "Directory for archived GTD items."
  :type 'directory
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-reviews-directory
  (expand-file-name "reviews/" codelahoma-gtd-directory)
  "Directory for review templates and records."
  :type 'directory
  :group 'codelahoma-gtd)

(defcustom codelahoma-knowledge-directory "~/personal/org-files/knowledge/"
  "Directory containing Zettelkasten knowledge files."
  :type 'directory
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-inbox-file
  (expand-file-name "inbox.org" codelahoma-gtd-directory)
  "Path to GTD inbox file."
  :type 'file
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-projects-file
  (expand-file-name "projects.org" codelahoma-gtd-directory)
  "Path to GTD projects file."
  :type 'file
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-next-actions-file
  (expand-file-name "next-actions.org" codelahoma-gtd-directory)
  "Path to GTD next actions file."
  :type 'file
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-waiting-for-file
  (expand-file-name "waiting-for.org" codelahoma-gtd-directory)
  "Path to GTD waiting for file."
  :type 'file
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-someday-file
  (expand-file-name "someday.org" codelahoma-gtd-directory)
  "Path to GTD someday/maybe file."
  :type 'file
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-calendar-file
  (expand-file-name "calendar.org" codelahoma-gtd-directory)
  "Path to GTD calendar file."
  :type 'file
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-media-file
  (expand-file-name "media.org" codelahoma-gtd-directory)
  "Path to GTD media file."
  :type 'file
  :group 'codelahoma-gtd)

;; Task states configuration
(defcustom codelahoma-gtd-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@)" "HOLD(h@)" "|" "DONE(d!)" "CANCELLED(c@)")
    (sequence "MEETING(m)" "|" "DONE(d!)"))
  "GTD task state keywords."
  :type 'sexp
  :group 'codelahoma-gtd)

;; Contexts configuration
(defcustom codelahoma-gtd-contexts
  '("@home" "@office" "@computer" "@phone" "@email" "@errands" 
    "@high-energy" "@low-energy" "@quick" "@deep-work")
  "List of GTD contexts."
  :type '(repeat string)
  :group 'codelahoma-gtd)

;; Areas configuration
(defcustom codelahoma-gtd-areas
  '("personal" "work" "learning" "health" "finance" "relationships")
  "List of life areas for organization."
  :type '(repeat string)
  :group 'codelahoma-gtd)

;; Performance settings
(defcustom codelahoma-gtd-capture-target-time 1.0
  "Target time in seconds for capture operations."
  :type 'float
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-process-batch-size 20
  "Number of items to show at once during inbox processing."
  :type 'integer
  :group 'codelahoma-gtd)

;; Auto-save configuration
(defcustom codelahoma-gtd-auto-save-interval 300
  "Seconds between automatic saves of org buffers (0 to disable)."
  :type 'integer
  :group 'codelahoma-gtd)

(provide 'codelahoma-gtd-config)
;;; codelahoma-gtd-config.el ends here