;;; codelahoma-gtd-contexts.el --- GTD context management -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Context system for GTD that enables filtering tasks by location, energy level,
;; and available tools.

;;; Code:

(require 'codelahoma-gtd-config)

;;; Context Definitions

(defcustom codelahoma-gtd-contexts
  '(;; Location contexts
    ("@computer" . (:icon "💻" :color "#4f97d7" :description "At computer with full tools"))
    ("@home" . (:icon "🏠" :color "#2d9574" :description "At home location"))
    ("@office" . (:icon "🏢" :color "#b1951d" :description "At office/work location"))
    ("@errands" . (:icon "🚗" :color "#dc752f" :description "Out and about"))
    ("@phone" . (:icon "📱" :color "#c56ec3" :description "Phone calls"))
    ("@anywhere" . (:icon "🌍" :color "#4f97d7" :description "Can be done anywhere"))
    ;; Energy contexts  
    ("@high-energy" . (:icon "⚡" :color "#f2241f" :description "Requires focus and energy"))
    ("@low-energy" . (:icon "🔋" :color "#86dc2f" :description "Low cognitive load"))
    ("@creative" . (:icon "🎨" :color "#a45bad" :description "Creative thinking needed")))
  "GTD contexts with properties."
  :type 'alist
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-time-contexts
  '(("@morning" . (:start "06:00" :end "12:00"))
    ("@afternoon" . (:start "12:00" :end "18:00"))
    ("@evening" . (:start "18:00" :end "22:00"))
    ("@weekend" . (:days (0 6)))) ; Sunday and Saturday
  "Time-based contexts."
  :type 'alist
  :group 'codelahoma-gtd)

;;; Context Variables

(defvar codelahoma-gtd-current-context "@anywhere"
  "Currently active context.")

(defvar codelahoma-gtd-current-energy "medium"
  "Current energy level.")

;;; Context Functions

(defun codelahoma-gtd-current-contexts ()
  "Return list of currently active contexts."
  (let ((contexts '())
        (current-time (current-time))
        (current-hour (string-to-number (format-time-string "%H")))
        (current-day (string-to-number (format-time-string "%w"))))
    ;; Always active contexts
    (push "@anywhere" contexts)
    (push "@phone" contexts)
    
    ;; Location contexts (could be enhanced with actual location detection)
    (when (or (getenv "SSH_CLIENT") (getenv "SSH_TTY"))
      (push "@computer" contexts))
    
    ;; Time-based contexts
    (dolist (time-context codelahoma-gtd-time-contexts)
      (let* ((name (car time-context))
             (props (cdr time-context))
             (start-hour (when (plist-get props :start)
                          (string-to-number (substring (plist-get props :start) 0 2))))
             (end-hour (when (plist-get props :end)
                        (string-to-number (substring (plist-get props :end) 0 2))))
             (days (plist-get props :days)))
        (when (or (and start-hour end-hour
                       (>= current-hour start-hour)
                       (< current-hour end-hour))
                  (and days (member current-day days)))
          (push name contexts))))
    
    contexts))

(defun codelahoma-gtd-assign-context ()
  "Assign context to current task."
  (interactive)
  (when (eq major-mode 'org-mode)
    (let* ((contexts (mapcar #'car codelahoma-gtd-contexts))
           (current (org-entry-get nil "CONTEXT"))
           (selected (completing-read 
                     (format "Context%s: " 
                             (if current (format " (current: %s)" current) ""))
                     contexts nil nil nil nil current)))
      (org-set-property "CONTEXT" selected)
      (message "Context set to %s" selected))))

(defun codelahoma-gtd-filter-by-context (context)
  "Filter agenda by CONTEXT."
  (interactive 
   (list (completing-read "Context: " 
                         (mapcar #'car codelahoma-gtd-contexts))))
  (let ((org-agenda-files (list codelahoma-gtd-directory)))
    (org-tags-view nil (format "CONTEXT=\"%s\"" context))))

(defun codelahoma-gtd-suggest-next-task ()
  "Suggest next task based on current context."
  (interactive)
  (let* ((current-contexts (codelahoma-gtd-current-contexts))
         (tasks '()))
    ;; Collect matching tasks
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (let ((context (org-entry-get nil "CONTEXT"))
                 (todo-state (org-get-todo-state)))
             (when (and (member todo-state '("NEXT" "TODO"))
                       (or (not context)
                           (member context current-contexts)))
               (push (list :title (org-get-heading t t t t)
                          :context context
                          :priority (org-get-priority (org-get-heading))
                          :effort (org-entry-get nil "Effort")
                          :file file
                          :point (point))
                     tasks))))
         "TODO|NEXT")))
    
    ;; Sort by priority and context match
    (setq tasks (sort tasks 
                     (lambda (a b)
                       (> (plist-get a :priority)
                          (plist-get b :priority)))))
    
    ;; Present suggestions
    (if tasks
        (codelahoma-gtd-show-task-suggestions tasks)
      (message "No tasks match current contexts"))))

(defun codelahoma-gtd-show-task-suggestions (tasks)
  "Show TASKS suggestions in a buffer."
  (with-output-to-temp-buffer "*GTD Task Suggestions*"
    (princ "Suggested Next Tasks\n")
    (princ "====================\n\n")
    (princ (format "Active contexts: %s\n\n" 
                   (mapconcat #'identity (codelahoma-gtd-current-contexts) ", ")))
    (let ((count 0))
      (dolist (task (seq-take tasks 10))
        (cl-incf count)
        (princ (format "%d. %s\n" count (plist-get task :title)))
        (princ (format "   Context: %s | Priority: %s | Effort: %s\n"
                      (or (plist-get task :context) "none")
                      (char-to-string (plist-get task :priority))
                      (or (plist-get task :effort) "?")))
        (princ (format "   File: %s\n\n" 
                      (file-name-nondirectory (plist-get task :file)))))))
  (switch-to-buffer-other-window "*GTD Task Suggestions*"))

;;; Energy Level Tracking

(defcustom codelahoma-gtd-energy-levels
  '(("high" . (:value 5 :color "#f2241f"))
    ("good" . (:value 4 :color "#4f97d7"))
    ("medium" . (:value 3 :color "#b1951d"))
    ("low" . (:value 2 :color "#2d9574"))
    ("tired" . (:value 1 :color "#9f8766")))
  "Energy level definitions."
  :type 'alist
  :group 'codelahoma-gtd)

(defun codelahoma-gtd-set-energy-level ()
  "Set current energy level."
  (interactive)
  (let ((level (completing-read "Energy level: " 
                               (mapcar #'car codelahoma-gtd-energy-levels)
                               nil t nil nil codelahoma-gtd-current-energy)))
    (setq codelahoma-gtd-current-energy level)
    (message "Energy level set to: %s" level)))

(defun codelahoma-gtd-match-task-to-energy ()
  "Match tasks to current energy level."
  (let ((energy-value (plist-get (alist-get codelahoma-gtd-current-energy 
                                           codelahoma-gtd-energy-levels 
                                           nil nil #'string=)
                                :value)))
    (cond
     ((>= energy-value 4) "@high-energy")
     ((<= energy-value 2) "@low-energy")
     (t "@anywhere"))))

(defun codelahoma-gtd-energy-match-p (current-energy task-energy)
  "Check if CURRENT-ENERGY matches TASK-ENERGY requirements."
  (let ((current-value (plist-get (alist-get current-energy
                                            codelahoma-gtd-energy-levels
                                            nil nil #'string=)
                                 :value))
        (task-value (plist-get (alist-get task-energy
                                         codelahoma-gtd-energy-levels
                                         nil nil #'string=)
                              :value)))
    (>= current-value (or task-value 3))))

;;; Context Display

(defun codelahoma-gtd-context-indicator ()
  "Return context indicator for mode line."
  (format " [%s|%s]" 
          (substring codelahoma-gtd-current-context 1)
          codelahoma-gtd-current-energy))

(defun codelahoma-gtd-set-context (context)
  "Set current context to CONTEXT."
  (setq codelahoma-gtd-current-context context)
  (message "Context set to %s" context)
  (force-mode-line-update))

(provide 'codelahoma-gtd-contexts)
;;; codelahoma-gtd-contexts.el ends here