;;; codelahoma-gtd-capture.el --- GTD capture functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Capture templates and quick entry functionality for the personal GTD system.
;; Implements intelligent context detection and rapid thought collection.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'org-capture)

;;; Capture Templates

(defcustom codelahoma-gtd-capture-templates
  '(("i" "Inbox" entry (file "~/personal/org-files/gtd/inbox.org")
     "* %?\n:PROPERTIES:\n:CAPTURED: %U\n:CONTEXT: %a\n:END:\n"
     :empty-lines 1)
    
    ("t" "Task with context" entry (file "~/personal/org-files/gtd/inbox.org")
     "* TODO %?\n:PROPERTIES:\n:CAPTURED: %U\n:CONTEXT: %a\n:END:\n%i"
     :empty-lines 1)
    
    ("p" "Phone call" entry (file "~/personal/org-files/gtd/inbox.org")
     "* PHONE %? :PHONE:\n:PROPERTIES:\n:CAPTURED: %U\n:PHONE_NUMBER: %^{Phone number}\n:END:\n"
     :clock-in t :clock-resume t)
    
    ("m" "Meeting" entry (file "~/personal/org-files/gtd/inbox.org")
     "* MEETING with %? :MEETING:\n:PROPERTIES:\n:CAPTURED: %U\n:ATTENDEES: %^{Attendees}\n:LOCATION: %^{Location|Office|Remote|Other}\n:END:\n\n** Agenda\n- \n\n** Notes\n"
     :clock-in t :clock-resume t)
    
    ("e" "Email to process" entry (file "~/personal/org-files/gtd/inbox.org")
     "* EMAIL %? :EMAIL:\n:PROPERTIES:\n:CAPTURED: %U\n:FROM: %^{From}\n:SUBJECT: %^{Subject}\n:END:\n"
     :empty-lines 1)
    
    ("w" "Weekly Review" entry (file+datetree "~/personal/org-files/gtd/reviews/weekly-reviews.org")
     "* Weekly Review :REVIEW:\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n\n** Completed This Week\n%?\n\n** Challenges\n\n** Next Week Focus\n\n** Notes\n"
     :jump-to-captured t))
  "GTD capture templates."
  :type 'sexp
  :group 'codelahoma-gtd)

;;; Context Detection

(defun codelahoma-gtd-detect-context ()
  "Detect current context for intelligent capture."
  (cond
   ;; In email buffer
   ((derived-mode-p 'message-mode 'mail-mode 'mu4e-view-mode)
    "email")
   ;; In code buffer
   ((derived-mode-p 'prog-mode)
    (format "coding:%s" (or (and (fboundp 'projectile-project-name)
                                (projectile-project-name)) 
                           "unknown")))
   ;; In web browser
   ((string-match-p "\\*eww\\*\\|\\*w3m\\*" (buffer-name))
    "web-browsing")
   ;; In org-roam note
   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-roam-buffer-p)
         (org-roam-buffer-p))
    "knowledge-work")
   ;; Default
   (t nil)))

;;; Capture Functions

(defun codelahoma-gtd-capture-inbox ()
  "Quick capture to GTD inbox."
  (interactive)
  (org-capture nil "i"))

(defun codelahoma-gtd-capture-generic ()
  "Generic capture with template selection."
  (interactive)
  (org-capture))

(defun codelahoma-gtd-capture-with-context ()
  "Capture with automatic context detection."
  (interactive)
  (let ((context (codelahoma-gtd-detect-context)))
    (org-capture nil (if (string-match-p "email\\|mail" (or context "")) "e" "t"))
    (when context
      (org-set-property "AUTO_CONTEXT" context))))

;;; Auto-tagging

(defun codelahoma-gtd-auto-tag ()
  "Automatically add tags based on entry content."
  (let ((title (org-get-heading t t t t)))
    (when title
      (cond
       ((string-match-p "\\b\\(call\\|phone\\|contact\\)\\b" title)
        (org-set-tags ":PHONE:"))
       ((string-match-p "\\b\\(email\\|mail\\|reply\\)\\b" title)
        (org-set-tags ":EMAIL:"))
       ((string-match-p "\\b\\(meeting\\|discuss\\|conversation\\)\\b" title)
        (org-set-tags ":MEETING:"))
       ((string-match-p "\\b\\(buy\\|purchase\\|order\\)\\b" title)
        (org-set-tags ":ERRAND:"))))))

;;; Capture Hooks

(defun codelahoma-gtd-capture-finalize-hook ()
  "Hook run after capture finalization."
  ;; Add creation timestamp
  (when (and (eq (org-capture-get :type) 'entry)
             (not (org-entry-get nil "CREATED")))
    (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]")))
  
  ;; Auto-tag based on content
  (codelahoma-gtd-auto-tag)
  
  ;; Save all org buffers
  (org-save-all-org-buffers))

;;; Setup

(defun codelahoma-gtd-setup-capture ()
  "Configure org-capture for GTD."
  (setq org-capture-templates 
        (append org-capture-templates codelahoma-gtd-capture-templates))
  (add-hook 'org-capture-after-finalize-hook 'codelahoma-gtd-capture-finalize-hook))

;;; Quick Capture Commands

(defun codelahoma-gtd-capture-task ()
  "Capture a task with context."
  (interactive)
  (org-capture nil "t"))

(defun codelahoma-gtd-capture-phone ()
  "Capture a phone call."
  (interactive)
  (org-capture nil "p"))

(defun codelahoma-gtd-capture-meeting ()
  "Capture a meeting."
  (interactive)
  (org-capture nil "m"))

(defun codelahoma-gtd-capture-email ()
  "Capture an email to process."
  (interactive)
  (org-capture nil "e"))

(provide 'codelahoma-gtd-capture)
;;; codelahoma-gtd-capture.el ends here