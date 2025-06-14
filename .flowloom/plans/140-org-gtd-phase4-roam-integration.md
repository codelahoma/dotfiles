# Org-GTD Phase 4: Roam-GTD Integration Implementation

## Overview

This document outlines a detailed implementation plan for Phase 4 of the Org-GTD system: Roam-GTD Integration. This phase creates bidirectional linking between the knowledge management system (org-roam) and the action management system (org-agenda), enabling seamless flow between thinking and doing.

## Purpose

This implementation aims to:

1. **Bridge Knowledge and Action** - Create functions to generate tasks from roam notes and link tasks to knowledge
2. **Enhance Project Management** - Link projects to their knowledge base and reference materials
3. **Enable Knowledge-Driven Workflows** - Process roam notes to extract actionable items

## Prerequisites

Before starting the implementation:

- [ ] Phase 1-3 are complete and tested
- [ ] Org-roam is installed and configured
- [ ] Roam database is initialized
- [ ] Understanding of org-roam node structure and APIs

## Implementation Plan

### Phase 4.1: Bidirectional Linking

#### Task 4.1.1: Create Tasks from Roam Nodes

**Status:** 📝 PLANNED

**Purpose:** Enable creation of GTD tasks directly from org-roam nodes, maintaining bidirectional links between knowledge and actions.

**Implementation Checklist:**
- [ ] Create function to generate task from current roam node
- [ ] Add backlink to source roam node in task
- [ ] Configure task properties with roam metadata
- [ ] Create bulk task creation from roam search
- [ ] Add interactive task type selection

**Reference Implementation:**
```elisp
(require 'org-roam)

(defun org-roam-create-task-from-node ()
  "Create a GTD task from the current org-roam node."
  (interactive)
  (unless (org-roam-node-at-point)
    (error "Not in an org-roam node"))
  
  (let* ((node (org-roam-node-at-point))
         (node-id (org-roam-node-id node))
         (node-title (org-roam-node-title node))
         (node-file (org-roam-node-file node))
         (task-type (completing-read "Task type: " 
                                     '("TODO" "NEXT" "PROJECT") nil t "TODO"))
         (context (completing-read "Context: " 
                                   '("work" "personal") nil t))
         (target-file (if (string= context "work")
                          (rk/org-file "work/gtd.org")
                        (rk/org-file "personal/gtd.org"))))
    
    ;; Capture the task with roam link
    (org-capture-string
     (format "* %s %s :@%s:roam:\n:PROPERTIES:\n:ROAM_ID: %s\n:END:\nSource: [[id:%s][%s]]\n\n%s"
             task-type
             (read-string "Task description: " node-title)
             context
             node-id
             node-id
             node-title
             (read-string "Additional notes: "))
     (format "%s" (if (string= context "work") "tw" "tp")))
    
    ;; Add forward link in roam node
    (save-excursion
      (goto-char (point-max))
      (insert (format "\n\n* Related Tasks\n- [[file:%s::*%s][GTD Task]]"
                      target-file node-title)))
    
    (message "Task created from roam node: %s" node-title)))

(defun org-roam-create-project-from-node ()
  "Create a GTD project from the current org-roam node."
  (interactive)
  (unless (org-roam-node-at-point)
    (error "Not in an org-roam node"))
  
  (let* ((node (org-roam-node-at-point))
         (node-id (org-roam-node-id node))
         (node-title (org-roam-node-title node))
         (context (completing-read "Context: " 
                                   '("work" "personal") nil t))
         (target-file (if (string= context "work")
                          (rk/org-file "work/projects.org")
                        (rk/org-file "personal/projects.org"))))
    
    ;; Create project with roam reference
    (with-current-buffer (find-file-noselect target-file)
      (goto-char (point-max))
      (insert (format "\n* TODO %s [/] :@%s:project:\n:PROPERTIES:\n:ROAM_REFS: [[id:%s]]\n:CATEGORY: roam-project\n:END:\n\n%s\n\n** TODO Define objectives\n** TODO Create plan\n** TODO Execute\n"
                      (read-string "Project name: " node-title)
                      context
                      node-id
                      (format "Knowledge Base: [[id:%s][%s]]" node-id node-title)))
      (save-buffer))
    
    (message "Project created from roam node: %s" node-title)))

(defun org-roam-batch-create-tasks ()
  "Create tasks from multiple roam nodes matching a filter."
  (interactive)
  (let* ((filter-tag (read-string "Filter by tag (or leave empty): "))
         (nodes (if (string-empty-p filter-tag)
                    (org-roam-node-list)
                  (org-roam-db-query
                   [:select [nodes:id nodes:title]
                    :from nodes
                    :left-join tags
                    :on (= nodes:id tags:node-id)
                    :where (= tags:tag $s1)]
                   filter-tag)))
         (selected-nodes (completing-read-multiple
                          "Select nodes to create tasks from: "
                          (mapcar (lambda (n) 
                                    (if (listp n)
                                        (cadr n)
                                      (org-roam-node-title n)))
                                  nodes)))
         (context (completing-read "Context for all tasks: " 
                                   '("work" "personal") nil t)))
    
    (dolist (node-title selected-nodes)
      (let ((node (car (org-roam-node-find-by-title node-title))))
        (when node
          ;; Create simplified task for batch operation
          (org-capture-string
           (format "* TODO Process: %s :@%s:roam:batch:\n:PROPERTIES:\n:ROAM_ID: %s\n:END:\n"
                   node-title
                   context
                   (org-roam-node-id node))
           (if (string= context "work") "tw" "tp")))))
    
    (message "Created %d tasks from roam nodes" (length selected-nodes))))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

#### Task 4.1.2: Navigate Between Tasks and Knowledge

**Status:** 📝 PLANNED

**Purpose:** Create navigation functions that allow jumping between agenda items and their related roam nodes.

**Implementation Checklist:**
- [ ] Create agenda-to-roam jump function
- [ ] Add roam backlinks in agenda view
- [ ] Create project-to-roam navigation
- [ ] Add roam node preview from agenda
- [ ] Configure bidirectional link display

**Reference Implementation:**
```elisp
(defun org-agenda-jump-to-roam-node ()
  "Jump from agenda item to its associated roam node."
  (interactive)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (let* ((marker (org-get-at-bol 'org-marker))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char pos)
        (let ((roam-id (org-entry-get nil "ROAM_ID"))
              (roam-refs (org-entry-get nil "ROAM_REFS")))
          (cond
           (roam-id
            (org-roam-node-visit (org-roam-node-from-id roam-id)))
           (roam-refs
            (if (string-match "\\[\\[id:\\([^]]+\\)\\]" roam-refs)
                (org-roam-node-visit (org-roam-node-from-id 
                                      (match-string 1 roam-refs)))
              (message "No valid roam reference found")))
           (t
            (message "No roam node associated with this task"))))))))

(defun org-agenda-show-roam-backlinks ()
  "Show roam backlinks for the current agenda item."
  (interactive)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (let* ((marker (org-get-at-bol 'org-marker))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char pos)
        (let ((roam-id (org-entry-get nil "ROAM_ID")))
          (if roam-id
              (progn
                (org-roam-buffer-toggle)
                (org-roam-buffer-display-dedicated (org-roam-node-from-id roam-id)))
            (message "No roam node associated with this task")))))))

;; Add roam indicators to agenda view
(defun rk/org-agenda-add-roam-indicator ()
  "Add indicator for tasks with roam links."
  (let* ((marker (org-get-at-bol 'org-marker))
         (buffer (when marker (marker-buffer marker)))
         (pos (when marker (marker-position marker))))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          (goto-char pos)
          (when (or (org-entry-get nil "ROAM_ID")
                    (org-entry-get nil "ROAM_REFS"))
            (save-excursion
              (beginning-of-line)
              (let ((inhibit-read-only t))
                (insert "🔗 ")))))))))

(add-hook 'org-agenda-finalize-hook 'rk/org-agenda-add-roam-indicator)

;; Quick preview of roam content
(defun org-agenda-preview-roam-node ()
  "Preview the roam node associated with current agenda item."
  (interactive)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (let* ((marker (org-get-at-bol 'org-marker))
         (buffer (marker-buffer marker))
         (pos (marker-position marker))
         (win (selected-window)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char pos)
        (let ((roam-id (org-entry-get nil "ROAM_ID")))
          (if roam-id
              (let ((node (org-roam-node-from-id roam-id)))
                (with-temp-buffer-window
                 "*Roam Preview*"
                 '(display-buffer-below-selected . ((window-height . 15)))
                 nil
                 (insert-file-contents (org-roam-node-file node))
                 (org-mode)
                 (goto-char (org-roam-node-point node))
                 (org-narrow-to-subtree)))
            (message "No roam node associated with this task")))))))

;; Keybindings for agenda-roam navigation
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "C-c r") 'org-agenda-jump-to-roam-node)
  (define-key org-agenda-mode-map (kbd "C-c R") 'org-agenda-show-roam-backlinks)
  (define-key org-agenda-mode-map (kbd "C-c p") 'org-agenda-preview-roam-node))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

#### Task 4.1.3: Process Actionable Notes

**Status:** 📝 PLANNED

**Purpose:** Create a systematic way to review roam notes and extract actionable items into the GTD system.

**Implementation Checklist:**
- [ ] Create actionable note detection function
- [ ] Build roam note processing interface
- [ ] Add bulk action extraction
- [ ] Configure actionable note tags
- [ ] Create processing workflow

**Reference Implementation:**
```elisp
(defun org-roam-process-actionable-notes ()
  "Process org-roam notes to extract actionable items."
  (interactive)
  (let* ((actionable-tag "actionable")
         (nodes (org-roam-db-query
                 [:select [nodes:id nodes:title nodes:file nodes:point]
                  :from nodes
                  :left-join tags
                  :on (= nodes:id tags:node-id)
                  :where (= tags:tag $s1)]
                 actionable-tag))
         (processed-count 0))
    
    (if (null nodes)
        (message "No actionable notes found")
      (dolist (node nodes)
        (let* ((id (car node))
               (title (cadr node))
               (file (caddr node))
               (point (cadddr node))
               (process (y-or-n-p (format "Process '%s'? " title))))
          (when process
            (with-current-buffer (find-file-noselect file)
              (goto-char point)
              (org-narrow-to-subtree)
              (let ((action-items (rk/extract-action-items)))
                (widen)
                (dolist (item action-items)
                  (org-capture-string
                   (format "* TODO %s :roam:actionable:\n:PROPERTIES:\n:ROAM_ID: %s\n:END:\nFrom: [[id:%s][%s]]"
                           item id id title)
                   "i"))
                (setq processed-count (1+ processed-count))
                ;; Remove actionable tag after processing
                (org-roam-tag-remove (list actionable-tag)))))))
      
      (message "Processed %d actionable notes" processed-count))))

(defun rk/extract-action-items ()
  "Extract action items from current narrowed subtree."
  (let ((items '())
        (regexp "^\\*+ \\(TODO\\|NEXT\\|ACTION\\|\\[ \\]\\) \\(.+\\)"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (match-string 2) items)))
    (nreverse items)))

(defun org-roam-mark-actionable ()
  "Mark current roam node as containing actionable items."
  (interactive)
  (when (org-roam-node-at-point)
    (org-roam-tag-add (list "actionable"))
    (message "Marked as actionable for later processing")))

(defun org-roam-quick-task ()
  "Quickly create a task from selected text in a roam node."
  (interactive)
  (let ((selected-text (if (use-region-p)
                           (buffer-substring-no-properties 
                            (region-beginning) (region-end))
                         (thing-at-point 'line t))))
    (when selected-text
      (let* ((task-text (string-trim selected-text))
             (node (org-roam-node-at-point))
             (context (completing-read "Context: " '("work" "personal") nil t)))
        (org-capture-string
         (format "* TODO %s :@%s:roam:\n:PROPERTIES:\n:ROAM_ID: %s\n:END:\nFrom: [[id:%s][%s]]"
                 task-text
                 context
                 (org-roam-node-id node)
                 (org-roam-node-id node)
                 (org-roam-node-title node))
         (if (string= context "work") "tw" "tp"))
        (message "Task created: %s" task-text)))))

;; Create processing dashboard
(defun org-roam-actionable-dashboard ()
  "Show dashboard of actionable roam notes."
  (interactive)
  (let ((buffer (get-buffer-create "*Roam Actionable Items*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "ACTIONABLE ROAM NOTES\n")
      (insert "=====================\n\n")
      
      (let ((nodes (org-roam-db-query
                    [:select [nodes:id nodes:title nodes:file]
                     :from nodes
                     :left-join tags
                     :on (= nodes:id tags:node-id)
                     :where (= tags:tag "actionable")])))
        (if (null nodes)
            (insert "No actionable notes found.\n")
          (dolist (node nodes)
            (let ((id (car node))
                  (title (cadr node)))
              (insert-button title
                             'action (lambda (_)
                                       (org-roam-node-visit 
                                        (org-roam-node-from-id id)))
                             'follow-link t)
              (insert "\n")))))
      
      (insert "\n\nKEYS:\n")
      (insert "RET - Visit node\n")
      (insert "p   - Process all actionable notes\n")
      (insert "q   - Quit\n")
      
      (special-mode)
      (local-set-key (kbd "p") 'org-roam-process-actionable-notes)
      (local-set-key (kbd "q") 'quit-window))
    
    (switch-to-buffer buffer)))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

### Phase 4.2: Enhanced Project Templates

#### Task 4.2.1: Roam-Enabled Project Templates

**Status:** 📝 PLANNED

**Purpose:** Create project templates that automatically establish bidirectional links with org-roam knowledge base.

**Implementation Checklist:**
- [ ] Enhance project templates with ROAM_REFS
- [ ] Create project knowledge base generator
- [ ] Add project-roam synchronization
- [ ] Configure project metadata integration
- [ ] Build project discovery from roam

**Reference Implementation:**
```elisp
(defun rk/create-project-with-roam ()
  "Create a new project with associated roam node."
  (interactive)
  (let* ((project-name (read-string "Project name: "))
         (context (completing-read "Context: " '("work" "personal") nil t))
         (project-file (if (string= context "work")
                           (rk/org-file "work/projects.org")
                         (rk/org-file "personal/projects.org")))
         (roam-title (format "Project: %s" project-name))
         (roam-node (org-roam-node-create :title roam-title)))
    
    ;; Create roam node for project
    (org-roam-capture-
     :node roam-node
     :templates '(("p" "project" plain 
                   "* Project Overview\n\n%?\n\n* Goals\n\n* Resources\n\n* Notes\n"
                   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                      "#+title: ${title}\n#+filetags: :project:\n"))))
    
    ;; Create GTD project with roam link
    (with-current-buffer (find-file-noselect project-file)
      (goto-char (point-max))
      (insert (format "\n* TODO %s [/] :@%s:project:\n:PROPERTIES:\n:ROAM_REFS: [[id:%s]]\n:CATEGORY: project\n:CREATED: %s\n:END:\n\n"
                      project-name
                      context
                      (org-roam-node-id roam-node)
                      (format-time-string "[%Y-%m-%d %a]")))
      
      ;; Add standard project structure
      (insert "** Project Setup\n")
      (insert "*** TODO Define success criteria\n")
      (insert "*** TODO Identify stakeholders\n")
      (insert "*** TODO Create project plan\n")
      (insert "*** TODO Set up communication channels\n\n")
      
      (insert "** Resources\n")
      (insert (format "- Knowledge Base: [[id:%s][%s]]\n\n"
                      (org-roam-node-id roam-node)
                      roam-title))
      
      (insert "** Milestones\n\n")
      (insert "** Tasks\n")
      
      (save-buffer))
    
    (message "Created project '%s' with roam node" project-name)))

(defun rk/sync-project-with-roam ()
  "Sync current project with its roam node."
  (interactive)
  (let ((roam-refs (org-entry-get nil "ROAM_REFS")))
    (if (and roam-refs (string-match "\\[\\[id:\\([^]]+\\)\\]" roam-refs))
        (let* ((roam-id (match-string 1 roam-refs))
               (node (org-roam-node-from-id roam-id))
               (project-name (org-get-heading t t t t))
               (project-state (org-get-todo-state))
               (completion (if (org-entry-is-done-p) 
                               (org-entry-get nil "CLOSED")
                             "In Progress")))
          
          ;; Update roam node with project status
          (with-current-buffer (find-file-noselect (org-roam-node-file node))
            (goto-char (org-roam-node-point node))
            (org-narrow-to-subtree)
            
            ;; Update or add status section
            (goto-char (point-max))
            (unless (search-backward "* Project Status" nil t)
              (goto-char (point-max))
              (insert "\n* Project Status\n"))
            
            (forward-line)
            (delete-region (point) (point-max))
            (insert (format "- State: %s\n" project-state))
            (insert (format "- Completion: %s\n" completion))
            (insert (format "- Last Sync: %s\n" 
                            (format-time-string "[%Y-%m-%d %a %H:%M]")))
            
            ;; Add task statistics
            (let ((stats (rk/calculate-project-stats)))
              (insert (format "- Total Tasks: %d\n" (plist-get stats :total)))
              (insert (format "- Completed: %d (%.0f%%)\n" 
                              (plist-get stats :done)
                              (plist-get stats :percent))))
            
            (widen)
            (save-buffer))
          
          (message "Synced project with roam node"))
      (message "No roam reference found for this project"))))

(defun rk/calculate-project-stats ()
  "Calculate statistics for current project."
  (save-excursion
    (org-back-to-heading)
    (let ((total 0)
          (done 0))
      (org-map-entries
       (lambda ()
         (when (member (org-get-todo-state) org-todo-keywords-1)
           (setq total (1+ total))
           (when (org-entry-is-done-p)
             (setq done (1+ done)))))
       nil 'tree)
      (list :total total 
            :done done 
            :percent (if (> total 0) 
                         (* 100.0 (/ done (float total))) 
                       0)))))

;; Auto-sync projects on state change
(defun rk/project-state-change-hook ()
  "Hook to sync project with roam on state change."
  (when (and (org-entry-get nil "ROAM_REFS")
             (member "project" (org-get-tags)))
    (rk/sync-project-with-roam)))

(add-hook 'org-after-todo-state-change-hook 'rk/project-state-change-hook)
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

## Implementation Process

Each task should follow this standard process:

1. **Development:**
   - Add code to `codelahoma-org.org` in the appropriate section
   - Ensure org-roam is properly required
   - Tangle the file to generate `codelahoma-org.el`
   - Reload the configuration or restart Spacemacs

2. **Testing:**
   - Test task creation from roam nodes
   - Verify bidirectional links work correctly
   - Test navigation between agenda and roam
   - Validate project-roam synchronization
   - Test bulk processing workflows

3. **Documentation:**
   - Document roam-GTD workflows
   - Create examples of linked tasks/projects
   - Add troubleshooting for common issues
   - Update keybinding reference

4. **Commit:**
   - Stage changes to both .org and .el files
   - Commit with descriptive message:
     ```
     feat: implement GTD-roam bidirectional integration
     
     - Create tasks and projects from roam nodes
     - Add navigation between agenda and knowledge base
     - Implement actionable note processing workflow
     - Enable project-roam synchronization
     ```

## Testing Strategy

### Manual Testing Checklist:

1. **Task Creation from Roam:**
   - [ ] Create task from roam node preserves link
   - [ ] Task appears in correct GTD file
   - [ ] Backlink added to roam node
   - [ ] Batch task creation works
   - [ ] Context selection functions properly

2. **Navigation:**
   - [ ] Jump from agenda to roam node works
   - [ ] Roam backlinks display in buffer
   - [ ] Preview function shows roam content
   - [ ] Roam indicators appear in agenda
   - [ ] All navigation preserves window configuration

3. **Processing Workflow:**
   - [ ] Actionable tag detection works
   - [ ] Processing dashboard displays correctly
   - [ ] Action items extracted accurately
   - [ ] Processed notes have tags removed
   - [ ] Quick task creation from selection

4. **Project Integration:**
   - [ ] Project creation generates roam node
   - [ ] Synchronization updates roam node
   - [ ] Statistics calculate correctly
   - [ ] State changes trigger sync
   - [ ] Project templates include roam links

## Rollback Plan

In case of issues:

1. Remove roam integration functions
2. Clear ROAM_ID and ROAM_REFS properties from tasks
3. Disable roam-related hooks
4. Remove roam indicators from agenda
5. Restart Spacemacs without roam integration

## Conclusion

This implementation creates a powerful integration between org-roam's knowledge management and GTD's action management. The bidirectional linking ensures that knowledge leads to action and actions contribute back to knowledge, creating a comprehensive personal information management system. The processing workflows help maintain the flow from thinking to doing, making the entire system more than the sum of its parts.