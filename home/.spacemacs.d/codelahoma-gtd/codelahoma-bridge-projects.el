;;; codelahoma-bridge-projects.el --- Project knowledge integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org-roam "2.0"))

;;; Commentary:
;; Connect GTD projects with their knowledge artifacts and references.
;; Provides project wikis, decision logs, and lessons learned capture.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)
(require 'codelahoma-bridge)
(require 'org-roam)

;;; Configuration

(defcustom codelahoma-bridge-project-wiki-template
  "* Overview
%s

* Goals & Objectives

* Key Decisions

* Resources & References

* Meeting Notes

* Progress Log

* Lessons Learned

* Archive"
  "Template for project wiki structure."
  :type 'string
  :group 'codelahoma-gtd)

(defcustom codelahoma-bridge-decision-template
  "** Decision: %s
:PROPERTIES:
:DECIDED: %s
:ID: %s
:END:

*** Context
%s

*** Options Considered
%s

*** Decision Rationale
%s

*** Implications
- [ ] 

*** Follow-up Actions
- [ ] "
  "Template for decision log entries."
  :type 'string
  :group 'codelahoma-gtd)

;;; Project Wiki Functions

(defun codelahoma-bridge-create-project-wiki ()
  "Create a knowledge wiki for current project."
  (interactive)
  (unless (codelahoma-gtd-project-p)
    (user-error "Not in a project"))
  (let* ((project-name (org-get-heading t t t t))
         (project-id (org-id-get-create))
         (project-file (buffer-file-name))
         (wiki-title (format "Wiki: %s" project-name))
         (wiki-id (org-id-new))
         (existing-wiki (org-entry-get nil "WIKI_ID")))
    
    ;; Check if wiki already exists
    (when existing-wiki
      (if (y-or-n-p "Wiki already exists. Open it? ")
          (org-roam-node-visit (org-roam-node-from-id existing-wiki))
        (user-error "Wiki already exists for this project")))
    
    ;; Create wiki
    (let* ((filename (format "%s-wiki-%s.org" 
                           (format-time-string "%Y%m%d")
                           (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" 
                                                   (downcase project-name))))
           (filepath (expand-file-name filename org-roam-directory)))
      
      (with-temp-file filepath
        (insert "#+title: " wiki-title "\n")
        (insert "#+created: " (format-time-string "[%Y-%m-%d %a]") "\n")
        (insert "#+roam_tags: project wiki\n\n")
        (insert ":PROPERTIES:\n")
        (insert ":ID: " wiki-id "\n")
        (insert ":PROJECT_ID: " project-id "\n")
        (insert ":PROJECT_FILE: " project-file "\n")
        (insert ":END:\n\n")
        
        ;; Add project link
        (insert "Project: [[id:" project-id "][" project-name "]]\n")
        (insert "File: [[file:" project-file "][" 
                (file-name-nondirectory project-file) "]]\n\n")
        
        ;; Insert wiki template
        (insert (format codelahoma-bridge-project-wiki-template
                       (or (org-entry-get nil "GOAL") 
                          "Project goals to be defined"))))
      
      ;; Update org-roam database
      (org-roam-db-update-file filepath)
      
      ;; Link project to wiki
      (org-set-property "WIKI_ID" wiki-id)
      (save-buffer)
      
      ;; Open wiki
      (find-file filepath)
      (message "Project wiki created: %s" wiki-title))))

(defun codelahoma-bridge-open-project-wiki ()
  "Open wiki for current project."
  (interactive)
  (let ((wiki-id (org-entry-get nil "WIKI_ID")))
    (if wiki-id
        (org-roam-node-visit (org-roam-node-from-id wiki-id))
      (when (y-or-n-p "No wiki found. Create one? ")
        (codelahoma-bridge-create-project-wiki)))))

;;; Reference Management

(defun codelahoma-bridge-add-project-reference ()
  "Add a reference to current project."
  (interactive)
  (unless (codelahoma-gtd-project-p)
    (user-error "Not in a project"))
  (let* ((ref-type (completing-read "Reference type: "
                                   '("document" "link" "person" 
                                     "tool" "decision" "risk" "assumption")
                                   nil t))
         (ref-title (read-string "Reference title: "))
         (ref-content (read-string "Description/URL: "))
         (ref-id (org-id-new)))
    
    ;; Find or create references section
    (save-excursion
      (org-back-to-heading)
      (let ((end (save-excursion (org-end-of-subtree t) (point))))
        (unless (re-search-forward "^\\*+ References" end t)
          (goto-char end)
          (insert "\n** References\n"))
        (org-end-of-subtree t)
        
        ;; Add reference entry
        (insert (format "\n*** [%s] %s\n" 
                       (upcase (substring ref-type 0 3))
                       ref-title))
        (org-set-property "REF_TYPE" ref-type)
        (org-set-property "ID" ref-id)
        (insert ref-content "\n")
        (insert "Added: " (format-time-string "[%Y-%m-%d %a]") "\n")))
    
    ;; Create knowledge note for important references
    (when (member ref-type '("decision" "risk" "assumption"))
      (codelahoma-bridge-create-reference-note 
       ref-type ref-title ref-content ref-id))
    
    (message "Reference added: %s" ref-title)))

(defun codelahoma-bridge-create-reference-note (ref-type ref-title ref-content ref-id)
  "Create a knowledge note for reference REF-TYPE with REF-TITLE and REF-CONTENT."
  (let* ((note-title (format "%s: %s" (capitalize ref-type) ref-title))
         (note-id (org-id-new))
         (project-name (org-get-heading t t t t))
         (project-id (org-id-get))
         (filename (format "%s-%s-%s.org" 
                          (format-time-string "%Y%m%d")
                          ref-type
                          (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" 
                                                  (downcase ref-title))))
         (filepath (expand-file-name filename org-roam-directory)))
    
    (with-temp-file filepath
      (insert "#+title: " note-title "\n")
      (insert "#+created: " (format-time-string "[%Y-%m-%d %a]") "\n")
      (insert "#+roam_tags: " ref-type " project-reference\n\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: " note-id "\n")
      (insert ":REF_ID: " ref-id "\n")
      (insert ":PROJECT_ID: " project-id "\n")
      (insert ":END:\n\n")
      
      (insert "* " (capitalize ref-type) "\n")
      (insert ref-content "\n\n")
      
      (insert "* Context\n")
      (insert "Project: [[id:" project-id "][" project-name "]]\n\n")
      
      (pcase ref-type
        ("decision"
         (insert "* Alternatives Considered\n\n")
         (insert "* Impact Analysis\n\n")
         (insert "* Review Date\n"))
        ("risk"
         (insert "* Probability\n\n")
         (insert "* Impact\n\n")
         (insert "* Mitigation Strategy\n\n")
         (insert "* Monitoring Plan\n"))
        ("assumption"
         (insert "* Validation Method\n\n")
         (insert "* Dependencies\n\n")
         (insert "* Review Schedule\n"))))
    
    (org-roam-db-update-file filepath)
    note-id))

;;; Decision Logging

(defun codelahoma-bridge-decision-log ()
  "Log a project decision with full context."
  (interactive)
  (unless (codelahoma-gtd-project-p)
    (user-error "Not in a project"))
  (let* ((decision (read-string "Decision: "))
         (context (read-string "Context/Problem: "))
         (options (read-string "Options considered (comma-separated): "))
         (rationale (read-string "Why this decision: "))
         (date (format-time-string "[%Y-%m-%d %a]"))
         (decision-id (org-id-new)))
    
    ;; Find or create decisions section
    (save-excursion
      (org-back-to-heading)
      (let ((end (save-excursion (org-end-of-subtree t) (point))))
        (unless (re-search-forward "^\\*+ Decision Log" end t)
          (goto-char end)
          (insert "\n** Decision Log\n"))
        (org-end-of-subtree t)
        
        ;; Add decision entry
        (insert (format codelahoma-bridge-decision-template
                       decision date decision-id
                       context options rationale))
        (save-buffer)))
    
    ;; Create decision note
    (let ((note-id (codelahoma-bridge-create-reference-note 
                   "decision" decision 
                   (format "Context: %s\nRationale: %s" context rationale)
                   decision-id)))
      
      ;; Optionally add to wiki
      (when (org-entry-get nil "WIKI_ID")
        (when (y-or-n-p "Add decision to project wiki? ")
          (codelahoma-bridge-add-to-wiki-section 
           "Key Decisions" 
           (format "- [[id:%s][%s]] - %s" note-id decision date)))))
    
    (message "Decision logged: %s" decision)))

(defun codelahoma-bridge-add-to-wiki-section (section content)
  "Add CONTENT to SECTION in project wiki."
  (when-let ((wiki-id (org-entry-get nil "WIKI_ID")))
    (let ((wiki-file (org-roam-node-file (org-roam-node-from-id wiki-id))))
      (with-current-buffer (find-file-noselect wiki-file)
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward (concat "^\\* " section) nil t)
            (org-end-of-subtree t)
            (insert "\n" content "\n")
            (save-buffer)))))))

;;; Lessons Learned

(defun codelahoma-bridge-project-lessons-learned ()
  "Capture lessons learned from project."
  (interactive)
  (unless (codelahoma-gtd-project-p)
    (user-error "Not in a project"))
  (let* ((project-name (org-get-heading t t t t))
         (project-id (org-id-get))
         (buffer-name (format "*Lessons Learned: %s*" project-name)))
    
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Lessons Learned - " project-name "\n")
      (insert "#+DATE: " (format-time-string "[%Y-%m-%d %a]") "\n")
      (insert "#+PROJECT_ID: " project-id "\n\n")
      
      (insert "* What Went Well\n")
      (insert "- \n- \n- \n\n")
      
      (insert "* What Could Be Improved\n")
      (insert "- \n- \n- \n\n")
      
      (insert "* Key Decisions & Outcomes\n")
      (codelahoma-bridge-insert-project-decisions project-id)
      (insert "\n")
      
      (insert "* Surprises & Discoveries\n")
      (insert "- \n- \n\n")
      
      (insert "* Recommendations for Future Projects\n")
      (insert "- \n- \n- \n\n")
      
      (insert "* Skills & Knowledge Gained\n")
      (insert "- \n- \n\n")
      
      (insert "* Resources to Remember\n")
      (insert "- \n- \n")
      
      (goto-char (point-min))
      (re-search-forward "^\\* What Went Well" nil t)
      (forward-line 1)
      (end-of-line))
    
    (switch-to-buffer buffer-name)))

(defun codelahoma-bridge-insert-project-decisions (project-id)
  "Insert summary of decisions for PROJECT-ID."
  (save-excursion
    (let ((decisions '()))
      ;; Find decisions in project
      (when-let ((project-node (org-roam-node-from-id project-id))
                 (project-file (org-roam-node-file project-node)))
        (with-temp-buffer
          (insert-file-contents project-file)
          (org-mode)
          (org-map-entries
           (lambda ()
             (when (string-match "^Decision:" (org-get-heading t t))
               (push (list :title (org-get-heading t t t t)
                          :date (org-entry-get nil "DECIDED")
                          :id (org-entry-get nil "ID"))
                     decisions)))
           nil 'file)))
      
      ;; Insert decisions
      (dolist (decision (nreverse decisions))
        (insert (format "- %s (%s)\n" 
                       (plist-get decision :title)
                       (or (plist-get decision :date) "undated")))))))

(defun codelahoma-bridge-save-lessons-learned ()
  "Save lessons learned to project wiki and knowledge base."
  (interactive)
  (unless (string-match "\\*Lessons Learned:" (buffer-name))
    (user-error "Not in a lessons learned buffer"))
  
  (let* ((project-id (save-excursion
                      (goto-char (point-min))
                      (when (re-search-forward "^#\\+PROJECT_ID: \\(.+\\)" nil t)
                        (match-string 1))))
         (content (buffer-string))
         (filename (format "%s-lessons-%s.org" 
                          (format-time-string "%Y%m%d")
                          (substring project-id -8)))
         (filepath (expand-file-name filename org-roam-directory)))
    
    ;; Save as knowledge note
    (with-temp-file filepath
      (insert content)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+TITLE:" nil t)
        (end-of-line)
        (insert "\n#+roam_tags: lessons-learned project")))
    
    (org-roam-db-update-file filepath)
    
    ;; Add to project wiki if exists
    (when project-id
      (save-excursion
        (when-let ((project-node (org-roam-node-from-id project-id))
                   (project-file (org-roam-node-file project-node)))
          (with-current-buffer (find-file-noselect project-file)
            (when-let ((wiki-id (org-entry-get nil "WIKI_ID")))
              (codelahoma-bridge-add-to-wiki-section 
               "Lessons Learned" 
               (format "[[file:%s][Captured %s]]" 
                      filepath 
                      (format-time-string "%Y-%m-%d"))))))))
    
    (message "Lessons learned saved to knowledge base")))

;;; Project Knowledge Dashboard

(defun codelahoma-bridge-project-knowledge-summary ()
  "Show knowledge summary for current project."
  (interactive)
  (unless (codelahoma-gtd-project-p)
    (user-error "Not in a project"))
  
  (let* ((project-name (org-get-heading t t t t))
         (project-id (org-id-get))
         (wiki-id (org-entry-get nil "WIKI_ID"))
         (buffer (get-buffer-create "*Project Knowledge Summary*")))
    
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Knowledge Summary - " project-name "\n\n")
      
      ;; Wiki status
      (insert "* Project Wiki\n")
      (if wiki-id
          (insert (format "[[id:%s][Open Wiki]]\n" wiki-id))
        (insert "No wiki created yet.\n"))
      
      ;; References
      (insert "\n* References\n")
      (codelahoma-bridge-list-project-references project-id)
      
      ;; Decisions
      (insert "\n* Decisions\n")
      (codelahoma-bridge-list-project-decisions project-id)
      
      ;; Linked knowledge
      (insert "\n* Linked Knowledge Notes\n")
      (codelahoma-bridge-list-project-knowledge project-id)
      
      ;; Metrics
      (insert "\n* Knowledge Metrics\n")
      (insert (format "- Wiki: %s\n" (if wiki-id "Yes" "No")))
      (insert (format "- References: %d\n" 
                     (codelahoma-bridge-count-project-references project-id)))
      (insert (format "- Decisions logged: %d\n" 
                     (codelahoma-bridge-count-project-decisions project-id)))
      (insert (format "- Knowledge notes: %d\n" 
                     (codelahoma-bridge-count-project-knowledge project-id)))
      
      (goto-char (point-min))
      (read-only-mode 1))
    
    (switch-to-buffer buffer)))

(defun codelahoma-bridge-list-project-references (project-id)
  "List references for PROJECT-ID."
  (let ((refs (codelahoma-bridge-get-project-references project-id)))
    (if refs
        (dolist (ref refs)
          (insert (format "- [%s] %s\n" 
                         (plist-get ref :type)
                         (plist-get ref :title))))
      (insert "No references added yet.\n"))))

(defun codelahoma-bridge-list-project-decisions (project-id)
  "List decisions for PROJECT-ID."
  (let ((decisions (codelahoma-bridge-get-project-decisions project-id)))
    (if decisions
        (dolist (decision decisions)
          (insert (format "- %s (%s)\n" 
                         (plist-get decision :title)
                         (plist-get decision :date))))
      (insert "No decisions logged yet.\n"))))

(defun codelahoma-bridge-list-project-knowledge (project-id)
  "List knowledge notes linked to PROJECT-ID."
  (let ((notes (codelahoma-bridge-get-project-knowledge project-id)))
    (if notes
        (dolist (note notes)
          (insert (format "- [[id:%s][%s]]\n" 
                         (org-roam-node-id note)
                         (org-roam-node-title note))))
      (insert "No linked knowledge notes yet.\n"))))

;;; Helper Functions

(defun codelahoma-bridge-get-project-references (project-id)
  "Get all references for PROJECT-ID."
  (let (refs)
    (when-let ((node (org-roam-node-from-id project-id))
               (file (org-roam-node-file node)))
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (org-map-entries
         (lambda ()
           (when (org-entry-get nil "REF_TYPE")
             (push (list :type (org-entry-get nil "REF_TYPE")
                        :title (org-get-heading t t t t)
                        :id (org-entry-get nil "ID"))
                   refs)))
         nil 'file)))
    (nreverse refs)))

(defun codelahoma-bridge-count-project-references (project-id)
  "Count references for PROJECT-ID."
  (length (codelahoma-bridge-get-project-references project-id)))

(defun codelahoma-bridge-get-project-decisions (project-id)
  "Get all decisions for PROJECT-ID."
  (let (decisions)
    (when-let ((node (org-roam-node-from-id project-id))
               (file (org-roam-node-file node)))
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (org-map-entries
         (lambda ()
           (when (and (org-entry-get nil "DECIDED")
                      (string-match "Decision:" (org-get-heading t t)))
             (push (list :title (replace-regexp-in-string 
                               "^Decision: " "" (org-get-heading t t t t))
                        :date (org-entry-get nil "DECIDED")
                        :id (org-entry-get nil "ID"))
                   decisions)))
         nil 'file)))
    (nreverse decisions)))

(defun codelahoma-bridge-count-project-decisions (project-id)
  "Count decisions for PROJECT-ID."
  (length (codelahoma-bridge-get-project-decisions project-id)))

(defun codelahoma-bridge-get-project-knowledge (project-id)
  "Get knowledge notes linked to PROJECT-ID."
  (org-roam-db-query
   [:select [nodes:id nodes:title]
    :from nodes
    :where (like properties (concat "%PROJECT_ID: " project-id "%"))]))

(defun codelahoma-bridge-count-project-knowledge (project-id)
  "Count knowledge notes for PROJECT-ID."
  (length (codelahoma-bridge-get-project-knowledge project-id)))

(provide 'codelahoma-bridge-projects)
;;; codelahoma-bridge-projects.el ends here