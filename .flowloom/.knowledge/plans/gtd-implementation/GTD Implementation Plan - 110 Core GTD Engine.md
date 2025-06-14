---
title: GTD Implementation Plan - 110 Core GTD Engine
type: note
permalink: plans/gtd-implementation/gtd-implementation-plan-110-core-gtd-engine
---

# GTD Implementation Plan - 110 Core GTD Engine

## Component Overview

Implement the core GTD engine with state management, file organization, and basic org-agenda integration. This forms the backbone of the GTD system.

## Subcomponents

### 110. State Management System

**TODO Keywords Configuration**:
```elisp
#+NAME: gtd-todo-keywords
#+BEGIN_SRC elisp
(defcustom gtd/todo-keywords
  '((sequence "INBOX(i)" "NEXT(n)" "TODO(t)" "WAITING(w@/!)" 
              "|" "DONE(d!)" "CANCELLED(c@)"))
  "TODO keywords for GTD workflow."
  :type '(repeat (choice symbol list))
  :group 'gtd)

(defun gtd/setup-todo-keywords ()
  "Configure org-mode TODO keywords for GTD."
  (setq org-todo-keywords gtd/todo-keywords)
  (setq org-todo-keyword-faces
        '(("INBOX" . org-warning)
          ("NEXT" . (:foreground "#FF6B6B" :weight bold))
          ("WAITING" . (:foreground "#4ECDC4" :weight bold))
          ("CANCELLED" . (:foreground "#95A5A6" :strike-through t)))))
#+END_SRC
```

**State Transition Functions**:
```elisp
#+NAME: gtd-state-transitions
#+BEGIN_SRC elisp
(defun gtd/mark-next-action ()
  "Mark current item as next action."
  (interactive)
  (org-todo "NEXT")
  (gtd/log "Marked as NEXT: %s" (org-get-heading t t)))

(defun gtd/mark-waiting ()
  "Mark current item as waiting with note."
  (interactive)
  (org-todo "WAITING")
  (org-add-note)
  (gtd/log "Marked as WAITING: %s" (org-get-heading t t)))

(defun gtd/inbox-to-next ()
  "Convert inbox item to next action."
  (interactive)
  (when (string= (org-get-todo-state) "INBOX")
    (org-todo "NEXT")
    (gtd/add-context-tags)
    (gtd/log "Processed inbox item: %s" (org-get-heading t t))))
#+END_SRC
```

### 111. File Organization System

**File Path Management**:
```elisp
#+NAME: gtd-file-paths
#+BEGIN_SRC elisp
(defconst gtd/core-files
  '((inbox . "inbox.org")
    (next-actions . "next-actions.org")
    (projects . "projects.org")
    (someday-maybe . "someday-maybe.org")
    (waiting-for . "waiting-for.org")
    (archive . "archive/archive.org"))
  "Core GTD files and their paths.")

(defun gtd/file-path (file-key)
  "Get full path for FILE-KEY."
  (expand-file-name 
   (cdr (assoc file-key gtd/core-files))
   gtd/directory))

(defun gtd/ensure-file-exists (file-key)
  "Ensure file for FILE-KEY exists with proper template."
  (let* ((file-path (gtd/file-path file-key))
         (template (cdr (assoc file-key gtd/file-templates))))
    (unless (file-exists-p file-path)
      (make-directory (file-name-directory file-path) t)
      (with-temp-file file-path
        (insert template))
      (gtd/log "Created file: %s" file-path))))
#+END_SRC
```

**File Creation Templates**:
```elisp
#+NAME: gtd-enhanced-templates
#+BEGIN_SRC elisp
(defconst gtd/enhanced-file-templates
  '((inbox . "#+TITLE: Inbox
#+FILETAGS: :inbox:
#+STARTUP: overview

* Instructions
This is your universal capture point. All new items start here.
Process regularly using =SPC o o p i=.

* Inbox Items
")
    (next-actions . "#+TITLE: Next Actions
#+FILETAGS: :actions:
#+STARTUP: content
#+COLUMNS: %TODO %ITEM %TAGS %EFFORT

* Context-Based Actions
** @computer
** @phone  
** @home
** @office
** @errands
")
    (projects . "#+TITLE: Projects
#+FILETAGS: :projects:
#+STARTUP: content

* Active Projects
* Someday/Maybe Projects
")
    (waiting-for . "#+TITLE: Waiting For
#+FILETAGS: :waiting:
#+STARTUP: content
#+COLUMNS: %TODO %ITEM %DEADLINE %TAGS

* Delegated Items
* External Dependencies
"))
  "Enhanced file templates with structure.")
#+END_SRC
```

### 112. Context Tag System

**Context Management**:
```elisp
#+NAME: gtd-context-system
#+BEGIN_SRC elisp
(defconst gtd/contexts
  '(("@computer" . "Tasks requiring computer")
    ("@phone" . "Phone calls to make")
    ("@home" . "Tasks at home")
    ("@office" . "Tasks at office")
    ("@errands" . "Errands to run")
    ("@anywhere" . "Tasks that can be done anywhere"))
  "Available contexts for GTD tasks.")

(defconst gtd/energy-levels
  '(("#high" . "High energy required")
    ("#medium" . "Medium energy required")
    ("#low" . "Low energy tasks"))
  "Energy level tags.")

(defconst gtd/time-estimates
  '(("#15min" . "15 minutes or less")
    ("#30min" . "30 minutes")
    ("#1hr" . "About 1 hour")
    ("#2hr+" . "2+ hours"))
  "Time estimate tags.")

(defun gtd/add-context-tags ()
  "Add context, energy, and time tags to current item."
  (interactive)
  (let* ((context (completing-read "Context: " 
                                   (mapcar #'car gtd/contexts)))
         (energy (completing-read "Energy: " 
                                  (mapcar #'car gtd/energy-levels)))
         (time (completing-read "Time: " 
                                (mapcar #'car gtd/time-estimates))))
    (org-set-tags (list context energy time))
    (gtd/log "Added tags: %s %s %s" context energy time)))
#+END_SRC
```

### 113. Agenda Integration

**GTD Agenda Views**:
```elisp
#+NAME: gtd-agenda-setup
#+BEGIN_SRC elisp
(defun gtd/setup-agenda ()
  "Configure org-agenda for GTD workflow."
  (setq org-agenda-files 
        (list (gtd/file-path 'inbox)
              (gtd/file-path 'next-actions)
              (gtd/file-path 'projects)
              (gtd/file-path 'waiting-for)))
  
  (add-to-list 'org-agenda-custom-commands
               '("g" . "GTD Views"))
  
  (add-to-list 'org-agenda-custom-commands
               '("gn" "Next Actions" 
                 ((tags-todo "+TODO=\"NEXT\""
                             ((org-agenda-overriding-header "Next Actions")
                              (org-agenda-sorting-strategy 
                               '(category-up priority-down)))))))
  
  (add-to-list 'org-agenda-custom-commands
               '("gw" "Waiting For"
                 ((tags-todo "+TODO=\"WAITING\""
                             ((org-agenda-overriding-header "Waiting For")
                              (org-agenda-sorting-strategy 
                               '(deadline-up category-up)))))))
  
  (add-to-list 'org-agenda-custom-commands
               '("gc" "Contexts"
                 ((tags-todo "@computer"
                             ((org-agenda-overriding-header "@Computer")))
                  (tags-todo "@phone"
                             ((org-agenda-overriding-header "@Phone")))
                  (tags-todo "@home"
                             ((org-agenda-overriding-header "@Home")))
                  (tags-todo "@office"
                             ((org-agenda-overriding-header "@Office")))
                  (tags-todo "@errands"
                             ((org-agenda-overriding-header "@Errands")))))))
#+END_SRC
```

### 114. Project Management

**Project Functions**:
```elisp
#+NAME: gtd-project-management
#+BEGIN_SRC elisp
(defun gtd/is-project-p ()
  "Check if current item is a project."
  (or (org-entry-get nil "PROJECT")
      (> (length (org-get-outline-path)) 0)))

(defun gtd/mark-as-project ()
  "Mark current item as project."
  (interactive)
  (org-set-property "PROJECT" "t")
  (org-todo "PROJ")
  (org-set-tags (org-get-tags nil t))
  (gtd/log "Marked as project: %s" (org-get-heading t t)))

(defun gtd/project-next-actions ()
  "Find next actions for current project."
  (interactive)
  (let* ((project-name (org-get-heading t t))
         (project-id (org-id-get-create))
         (search-term (format "PROJECT_ID:\"%s\"" project-id)))
    (org-search-view nil search-term)))

(defun gtd/link-to-project ()
  "Link current action to a project."
  (interactive)
  (let* ((projects (gtd/get-all-projects))
         (project (completing-read "Project: " projects))
         (project-id (gtd/get-project-id project)))
    (org-set-property "PROJECT_ID" project-id)
    (gtd/log "Linked to project: %s" project)))
#+END_SRC
```

### 115. Archive System

**Archiving Functions**:
```elisp
#+NAME: gtd-archive-system
#+BEGIN_SRC elisp
(defun gtd/setup-archive ()
  "Configure archiving for GTD system."
  (setq org-archive-location 
        (concat (gtd/file-path 'archive) "::* Archived %s"))
  (setq org-archive-save-context-info
        '(time file ltags itags todo category olpath)))

(defun gtd/archive-done-items ()
  "Archive all DONE items in current file."
  (interactive)
  (org-map-entries 
   (lambda ()
     (when (member (org-get-todo-state) '("DONE" "CANCELLED"))
       (org-archive-subtree)
       (setq org-map-continue-from (point))))
   "+TODO=\"DONE\"|+TODO=\"CANCELLED\"")
  (gtd/log "Archived completed items"))

(defun gtd/weekly-archive ()
  "Archive items completed more than a week ago."
  (interactive)
  (let ((cutoff-date (time-subtract (current-time) 
                                    (days-to-time 7))))
    (org-map-entries
     (lambda ()
       (when (and (member (org-get-todo-state) '("DONE" "CANCELLED"))
                  (let ((closed-time (org-entry-get nil "CLOSED")))
                    (and closed-time
                         (time-less-p (org-time-string-to-time closed-time)
                                      cutoff-date))))
         (org-archive-subtree)))
     "+TODO=\"DONE\"|+TODO=\"CANCELLED\"")))
#+END_SRC
```

### 116. Statistics and Metrics

**Basic Metrics**:
```elisp
#+NAME: gtd-metrics
#+BEGIN_SRC elisp
(defun gtd/count-items-by-state ()
  "Count items by TODO state."
  (interactive)
  (let ((counts '()))
    (dolist (file org-agenda-files)
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (let ((state (org-get-todo-state)))
             (when state
               (setq counts (plist-put counts 
                                       (intern state)
                                       (1+ (or (plist-get counts (intern state)) 0))))))))))
    (message "GTD Statistics: %s" counts)
    counts))

(defun gtd/productivity-report ()
  "Generate basic productivity report."
  (interactive)
  (let* ((stats (gtd/count-items-by-state))
         (total (apply #'+ (seq-filter #'numberp stats)))
         (completed (or (plist-get stats 'DONE) 0))
         (rate (if (> total 0) (/ (* 100.0 completed) total) 0)))
    (message "Completion rate: %.1f%% (%d/%d)" rate completed total)))
#+END_SRC
```

### 117. Configuration Integration

**Spacemacs Integration**:
```elisp
#+NAME: gtd-spacemacs-integration
#+BEGIN_SRC elisp
(defun gtd/integrate-with-spacemacs ()
  "Integrate GTD system with Spacemacs configuration."
  (with-eval-after-load 'org
    (gtd/setup-todo-keywords)
    (gtd/setup-agenda)
    (gtd/setup-archive))
  
  ;; Add to org-mode hook
  (add-hook 'org-mode-hook 
            (lambda ()
              (when (gtd/in-gtd-file-p)
                (gtd/setup-buffer-local-settings))))
  
  ;; Add to spacemacs org layer
  (spacemacs|add-toggle gtd-mode
    :status gtd-mode
    :on (gtd/enable-mode)
    :off (gtd/disable-mode)
    :documentation "Toggle GTD mode"))

(defun gtd/in-gtd-file-p ()
  "Check if current buffer is a GTD file."
  (and buffer-file-name
       (string-prefix-p gtd/directory buffer-file-name)))
#+END_SRC
```

### 118. Core System Assembly

**Main Initialization**:
```elisp
#+NAME: gtd-core-init
#+BEGIN_SRC elisp
(defun gtd/initialize-core ()
  "Initialize core GTD system."
  (gtd/check-dependencies)
  (gtd/setup-directories)
  
  ;; Ensure all core files exist
  (dolist (file-key '(inbox next-actions projects waiting-for))
    (gtd/ensure-file-exists file-key))
  
  ;; Setup org-mode integration
  (gtd/integrate-with-spacemacs)
  
  ;; Initialize agenda
  (gtd/setup-agenda)
  
  (gtd/log "GTD core system initialized")
  (message "GTD core system ready!"))

;; Auto-initialize if in interactive session
(when (called-interactively-p 'any)
  (gtd/initialize-core))
#+END_SRC
```

## Implementation Tasks

1. [ ] Implement TODO keyword configuration
2. [ ] Create state transition functions  
3. [ ] Set up file path management
4. [ ] Create enhanced file templates
5. [ ] Implement context tag system
6. [ ] Configure agenda integration
7. [ ] Build project management functions
8. [ ] Set up archive system
9. [ ] Create basic metrics functions
10. [ ] Integrate with Spacemacs
11. [ ] Test core initialization

## Testing Checklist

- [ ] TODO keywords display correctly
- [ ] State transitions work properly
- [ ] Files create with correct templates
- [ ] Context tags can be added/removed
- [ ] Agenda views show correct items
- [ ] Project linking functions work
- [ ] Archive system preserves context
- [ ] Metrics calculate accurately
- [ ] Spacemacs integration loads cleanly

## Success Criteria

1. All core files created automatically
2. TODO keywords configured and working
3. Context system operational
4. Agenda views populated correctly
5. No errors on initialization

---

*This detailed plan implements the core GTD engine that powers the entire system.*