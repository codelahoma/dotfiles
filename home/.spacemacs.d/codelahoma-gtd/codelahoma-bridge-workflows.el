;;; codelahoma-bridge-workflows.el --- Knowledge-driven workflows -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org-roam "2.0"))

;;; Commentary:
;; Knowledge-driven workflows that integrate knowledge capture with task management.
;; Provides specialized workflows for reading, research, learning, and insights.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)
(require 'codelahoma-gtd-capture)
(require 'codelahoma-bridge)
(require 'org-roam)

;;; Configuration

(defcustom codelahoma-bridge-reading-directory
  (expand-file-name "reading" codelahoma-gtd-directory)
  "Directory for reading list and notes."
  :type 'directory
  :group 'codelahoma-gtd)

(defcustom codelahoma-bridge-research-directory
  (expand-file-name "research" codelahoma-gtd-directory)
  "Directory for research projects."
  :type 'directory
  :group 'codelahoma-gtd)

(defcustom codelahoma-bridge-learning-directory
  (expand-file-name "learning" codelahoma-gtd-directory)
  "Directory for learning projects."
  :type 'directory
  :group 'codelahoma-gtd)

;;; Reading Workflow

(defun codelahoma-bridge-create-reading-task ()
  "Create a reading task with knowledge capture setup."
  (interactive)
  (let* ((title (read-string "Book/Article title: "))
         (author (read-string "Author (optional): "))
         (type (completing-read "Type: " 
                               '("book" "article" "paper" "video" "course")
                               nil t))
         (url (read-string "URL (optional): "))
         (deadline (org-read-date nil nil nil "Finish by: "))
         (full-title (if (string-empty-p author)
                        title
                      (format "%s - %s" title author))))
    
    ;; Create reading file if needed
    (unless (file-exists-p codelahoma-bridge-reading-directory)
      (make-directory codelahoma-bridge-reading-directory t))
    
    ;; Add to reading list
    (let ((reading-file (expand-file-name "reading-list.org" 
                                         codelahoma-bridge-reading-directory)))
      (with-current-buffer (find-file-noselect reading-file)
        (goto-char (point-max))
        (insert (format "\n* TODO Read: %s\n" full-title))
        (org-set-property "READING_TYPE" type)
        (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %a]"))
        (when (not (string-empty-p url))
          (org-set-property "URL" url))
        (org-deadline nil deadline)
        
        ;; Create placeholder note
        (let ((note-id (codelahoma-bridge-create-reading-note 
                       full-title type author url)))
          (when note-id
            (org-set-property codelahoma-bridge-link-property note-id)
            (insert (format "Notes: [[id:%s][%s]]\n" note-id full-title))))
        
        (save-buffer)))
    
    (message "Reading task created: %s" full-title)))

(defun codelahoma-bridge-create-reading-note (title type &optional author url)
  "Create a knowledge note template for reading TITLE of TYPE."
  (let* ((slug (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" 
                                        (downcase title)))
         (filename (format "%s-%s.org" 
                          (format-time-string "%Y%m%d")
                          slug))
         (filepath (expand-file-name filename org-roam-directory))
         (note-id (org-id-new)))
    
    (with-temp-file filepath
      (insert "#+title: " title "\n")
      (insert "#+created: " (format-time-string "[%Y-%m-%d %a]") "\n")
      (insert "#+roam_tags: reading " type "\n")
      (when author
        (insert "#+author: " author "\n"))
      (when url
        (insert "#+url: " url "\n"))
      (insert "\n:PROPERTIES:\n")
      (insert ":ID: " note-id "\n")
      (insert ":END:\n\n")
      
      ;; Template content
      (insert "* Summary\n\n")
      (insert "* Key Ideas\n\n")
      (insert "* Quotes\n\n")
      (insert "* Personal Reflections\n\n")
      (insert "* Action Items\n")
      (insert "- [ ] \n")
      (insert "\n* References\n"))
    
    ;; Update org-roam database
    (org-roam-db-update-file filepath)
    note-id))

;;; Research Workflow

(defun codelahoma-bridge-research-workflow ()
  "Start a research workflow with integrated knowledge capture."
  (interactive)
  (let* ((topic (read-string "Research topic: "))
         (goal (read-string "Research goal: "))
         (deadline (org-read-date nil nil nil "Target completion: "))
         (project-id (org-id-new)))
    
    ;; Create research directory
    (unless (file-exists-p codelahoma-bridge-research-directory)
      (make-directory codelahoma-bridge-research-directory t))
    
    ;; Create research project
    (let ((project-file (expand-file-name 
                        (format "%s-research.org" 
                               (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" 
                                                       (downcase topic)))
                        codelahoma-bridge-research-directory)))
      (with-current-buffer (find-file-noselect project-file)
        (erase-buffer)
        (insert "#+TITLE: Research: " topic "\n")
        (insert "#+CREATED: " (format-time-string "[%Y-%m-%d %a]") "\n\n")
        (insert ":PROPERTIES:\n")
        (insert ":ID: " project-id "\n")
        (insert ":END:\n\n")
        
        (insert "* PROJECT Research: " topic "\n")
        (org-set-property "GOAL" goal)
        (org-deadline nil deadline)
        
        ;; Create research structure
        (insert "\n** Research Plan\n")
        (insert goal "\n\n")
        
        (insert "** TODO Literature Review\n")
        (insert "*** TODO Identify key sources\n")
        (insert "*** TODO Read primary materials\n")
        (insert "*** TODO Synthesize findings\n\n")
        
        (insert "** TODO Data Collection\n")
        (insert "*** TODO Define methodology\n")
        (insert "*** TODO Gather data\n")
        (insert "*** TODO Validate data\n\n")
        
        (insert "** TODO Analysis\n")
        (insert "*** TODO Analyze findings\n")
        (insert "*** TODO Draw conclusions\n")
        (insert "*** TODO Identify gaps\n\n")
        
        (insert "** TODO Documentation\n")
        (insert "*** TODO Write summary\n")
        (insert "*** TODO Create presentation\n")
        (insert "*** TODO Share findings\n\n")
        
        (insert "** Resources\n\n")
        (insert "** Notes\n")
        
        (save-buffer)))
    
    ;; Create knowledge hub
    (codelahoma-bridge-create-research-hub topic project-id)
    
    (message "Research project created: %s" topic)))

(defun codelahoma-bridge-create-research-hub (topic project-id)
  "Create a knowledge hub for research TOPIC linked to PROJECT-ID."
  (let* ((hub-title (format "Research Hub: %s" topic))
         (hub-id (org-id-new))
         (filename (format "%s-research-hub.org" 
                          (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" 
                                                  (downcase topic))))
         (filepath (expand-file-name filename org-roam-directory)))
    
    (with-temp-file filepath
      (insert "#+title: " hub-title "\n")
      (insert "#+created: " (format-time-string "[%Y-%m-%d %a]") "\n")
      (insert "#+roam_tags: research hub\n\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: " hub-id "\n")
      (insert ":END:\n\n")
      
      (insert "* Overview\n")
      (insert "Research project: [[id:" project-id "][" topic "]]\n\n")
      
      (insert "* Literature Notes\n\n")
      (insert "* Concept Map\n\n")
      (insert "* Key Findings\n\n")
      (insert "* Open Questions\n\n")
      (insert "* Synthesis\n"))
    
    (org-roam-db-update-file filepath)
    hub-id))

;;; Learning Workflow

(defun codelahoma-bridge-learning-project ()
  "Create a structured learning project."
  (interactive)
  (let* ((subject (read-string "What do you want to learn? "))
         (duration (read-string "Time frame (e.g., 30 days): "))
         (method (completing-read "Primary learning method: "
                                 '("course" "book" "practice" "mixed" "mentor")
                                 nil t))
         (goal (read-string "Specific goal: "))
         (project-id (org-id-new)))
    
    ;; Create learning directory
    (unless (file-exists-p codelahoma-bridge-learning-directory)
      (make-directory codelahoma-bridge-learning-directory t))
    
    ;; Create learning project
    (let ((project-file (expand-file-name 
                        (format "%s-learning.org" 
                               (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" 
                                                       (downcase subject)))
                        codelahoma-bridge-learning-directory)))
      (with-current-buffer (find-file-noselect project-file)
        (erase-buffer)
        (insert "#+TITLE: Learning: " subject "\n")
        (insert "#+CREATED: " (format-time-string "[%Y-%m-%d %a]") "\n\n")
        (insert ":PROPERTIES:\n")
        (insert ":ID: " project-id "\n")
        (insert ":END:\n\n")
        
        (insert "* PROJECT Learn: " subject "\n")
        (org-set-property "DURATION" duration)
        (org-set-property "METHOD" method)
        (org-set-property "GOAL" goal)
        
        ;; Create learning plan
        (codelahoma-bridge-create-learning-plan 
         subject duration method goal)
        
        (save-buffer)))
    
    ;; Create knowledge structure
    (codelahoma-bridge-create-learning-notes subject project-id)
    
    (message "Learning project created: %s" subject)))

(defun codelahoma-bridge-create-learning-plan (subject duration method goal)
  "Create a learning plan for SUBJECT over DURATION using METHOD to achieve GOAL."
  (insert "\n** Learning Plan\n")
  (insert "Goal: " goal "\n")
  (insert "Duration: " duration "\n")
  (insert "Method: " method "\n\n")
  
  ;; Method-specific plans
  (pcase method
    ("course"
     (insert "*** TODO Find and enroll in course\n")
     (insert "*** TODO Complete course modules\n")
     (insert "*** TODO Do exercises and projects\n")
     (insert "*** TODO Take final assessment\n"))
    
    ("book"
     (insert "*** TODO Select primary textbook\n")
     (insert "*** TODO Create reading schedule\n")
     (insert "*** TODO Complete exercises\n")
     (insert "*** TODO Apply knowledge in project\n"))
    
    ("practice"
     (insert "*** TODO Set up practice environment\n")
     (insert "*** TODO Daily practice sessions\n")
     (insert "*** TODO Build portfolio projects\n")
     (insert "*** TODO Get feedback\n"))
    
    ("mentor"
     (insert "*** TODO Find mentor/coach\n")
     (insert "*** TODO Schedule sessions\n")
     (insert "*** TODO Complete assignments\n")
     (insert "*** TODO Track progress\n"))
    
    (_
     (insert "*** TODO Design curriculum\n")
     (insert "*** TODO Gather resources\n")
     (insert "*** TODO Execute plan\n")
     (insert "*** TODO Assess progress\n")))
  
  (insert "\n** Resources\n")
  (insert "- [ ] Primary resource: \n")
  (insert "- [ ] Secondary resources: \n")
  (insert "- [ ] Practice materials: \n")
  
  (insert "\n** Progress Tracking\n")
  (insert "*** Week 1\n- [ ] \n")
  (insert "*** Week 2\n- [ ] \n")
  (insert "*** Week 3\n- [ ] \n")
  (insert "*** Week 4\n- [ ] \n"))

(defun codelahoma-bridge-create-learning-notes (subject project-id)
  "Create knowledge note structure for learning SUBJECT in PROJECT-ID."
  (let* ((notes-title (format "Learning Notes: %s" subject))
         (notes-id (org-id-new))
         (filename (format "%s-learning-notes.org" 
                          (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" 
                                                  (downcase subject))))
         (filepath (expand-file-name filename org-roam-directory)))
    
    (with-temp-file filepath
      (insert "#+title: " notes-title "\n")
      (insert "#+created: " (format-time-string "[%Y-%m-%d %a]") "\n")
      (insert "#+roam_tags: learning\n\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: " notes-id "\n")
      (insert ":END:\n\n")
      
      (insert "* Overview\n")
      (insert "Learning project: [[id:" project-id "][" subject "]]\n\n")
      
      (insert "* Core Concepts\n\n")
      (insert "* Practical Examples\n\n")
      (insert "* Common Pitfalls\n\n")
      (insert "* Practice Log\n\n")
      (insert "* Questions & Answers\n\n")
      (insert "* Resources & References\n"))
    
    (org-roam-db-update-file filepath)
    notes-id))

;;; Insight Capture

(defun codelahoma-bridge-insight-capture ()
  "Capture an insight and create follow-up tasks."
  (interactive)
  (let* ((insight (read-string "Insight (brief): "))
         (description (read-string "Elaborate (optional): "))
         (context-node (org-roam-node-read nil nil nil nil "Related to: "))
         (actionable (y-or-n-p "Is this actionable? "))
         (insight-id (org-id-new)))
    
    ;; Create insight note
    (let* ((filename (format "%s-insight-%s.org" 
                           (format-time-string "%Y%m%d%H%M")
                           (substring insight-id -4)))
           (filepath (expand-file-name filename org-roam-directory)))
      
      (with-temp-file filepath
        (insert "#+title: Insight: " insight "\n")
        (insert "#+created: " (format-time-string "[%Y-%m-%d %a %H:%M]") "\n")
        (insert "#+roam_tags: insight\n\n")
        (insert ":PROPERTIES:\n")
        (insert ":ID: " insight-id "\n")
        (insert ":END:\n\n")
        
        (insert "* Insight\n")
        (insert insight "\n\n")
        
        (when (not (string-empty-p description))
          (insert "* Description\n")
          (insert description "\n\n"))
        
        (when context-node
          (insert "* Context\n")
          (insert "Related to: [[id:" (org-roam-node-id context-node) "]["
                  (org-roam-node-title context-node) "]]\n\n"))
        
        (when actionable
          (insert "* Action Items\n")
          (insert "- [ ] \n")
          (insert "- [ ] \n\n"))
        
        (insert "* Implications\n\n")
        (insert "* Follow-up Questions\n"))
      
      (org-roam-db-update-file filepath)
      
      ;; Create tasks if actionable
      (when actionable
        (codelahoma-bridge-create-insight-tasks insight insight-id))
      
      (message "Insight captured%s" 
               (if actionable " with tasks" "")))))

(defun codelahoma-bridge-create-insight-tasks (insight insight-id)
  "Create tasks based on INSIGHT linked to INSIGHT-ID."
  (let ((num-tasks (string-to-number 
                   (read-string "How many action items? " "1"))))
    (with-current-buffer (find-file-noselect codelahoma-gtd-inbox-file)
      (goto-char (point-max))
      (dotimes (i num-tasks)
        (let ((task (read-string (format "Task %d: " (1+ i)))))
          (insert "\n* TODO " task "\n")
          (org-set-property codelahoma-bridge-link-property insight-id)
          (insert "From insight: [[id:" insight-id "][" insight "]]\n")
          (insert "Captured: " (format-time-string "[%Y-%m-%d %a %H:%M]") "\n")))
      (save-buffer))))

;;; Knowledge Review Integration

(defun codelahoma-bridge-knowledge-review ()
  "Review recent knowledge notes and extract actions."
  (interactive)
  (let* ((days (string-to-number 
               (read-string "Review notes from last N days: " "7")))
         (cutoff (time-subtract (current-time) (days-to-time days)))
         (recent-notes (codelahoma-bridge-get-recent-notes cutoff))
         (buffer (get-buffer-create "*Knowledge Review*")))
    
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Knowledge Review\n")
      (insert "#+DATE: " (format-time-string "[%Y-%m-%d %a]") "\n\n")
      
      (insert (format "* Recent Notes (last %d days)\n\n" days))
      
      (dolist (note recent-notes)
        (insert "** [[id:" (org-roam-node-id note) "]["
                (org-roam-node-title note) "]]\n")
        (insert "Created: " (org-roam-node-formatted-time note) "\n")
        (insert "Tags: " (string-join (org-roam-node-tags note) ", ") "\n")
        
        ;; Check for uncompleted tasks
        (let ((tasks (codelahoma-bridge-find-note-tasks note)))
          (when tasks
            (insert "\nPending tasks:\n")
            (dolist (task tasks)
              (insert "- [ ] " (plist-get task :heading) "\n"))))
        (insert "\n"))
      
      (insert "* Actions\n")
      (insert "- [ ] Review and process the above notes\n")
      (insert "- [ ] Extract any additional tasks\n")
      (insert "- [ ] Update knowledge connections\n"))
    
    (switch-to-buffer buffer)))

(defun codelahoma-bridge-get-recent-notes (cutoff-time)
  "Get org-roam notes created after CUTOFF-TIME."
  (seq-filter 
   (lambda (node)
     (let ((file-time (nth 5 (file-attributes (org-roam-node-file node)))))
       (time-less-p cutoff-time file-time)))
   (org-roam-node-list)))

(defun codelahoma-bridge-find-note-tasks (note)
  "Find uncompleted tasks in NOTE."
  (let (tasks)
    (with-temp-buffer
      (insert-file-contents (org-roam-node-file note))
      (org-mode)
      (org-map-entries
       (lambda ()
         (when (and (org-get-todo-state)
                   (not (member (org-get-todo-state) org-done-keywords)))
           (push (list :heading (org-get-heading t t t t)
                      :state (org-get-todo-state))
                 tasks)))
       nil 'file))
    tasks))

(provide 'codelahoma-bridge-workflows)
;;; codelahoma-bridge-workflows.el ends here