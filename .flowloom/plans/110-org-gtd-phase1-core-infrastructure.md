# Org-GTD Phase 1: Core Infrastructure Implementation

## Overview

This document outlines a detailed implementation plan for Phase 1 of the Org-GTD system: Core GTD Infrastructure. This phase establishes the foundational directory structure, TODO workflows, and archive configuration that all other phases will build upon.

## Purpose

This implementation aims to:

1. **Establish Directory Structure** - Create a well-organized GTD file hierarchy separating work and personal contexts
2. **Configure TODO Workflows** - Set up comprehensive TODO keywords and context tags for effective task management
3. **Enable Archiving** - Configure proper archive locations to maintain a clean working environment

## Prerequisites

Before starting the implementation:

- [ ] Spacemacs is properly installed and configured
- [ ] `codelahoma-org.org` file exists and is being loaded by Spacemacs
- [ ] Backup of current org files in `~/personal/org-files/` is complete
- [ ] Understanding of `rk/org-file` helper function usage

## Implementation Plan

### Phase 1.1: File Structure Setup

#### Task 1.1.1: Create Directory Structure

**Status:** 📝 PLANNED

**Purpose:** Establish the GTD directory hierarchy that separates work and personal contexts while maintaining a unified inbox and archive.

**Implementation Checklist:**
- [ ] Create work subdirectory with GTD files
- [ ] Create personal subdirectory with GTD files
- [ ] Create inbox.org at root level
- [ ] Create archive.org at root level
- [ ] Verify all directories and files are created successfully

**Reference Implementation:**
```elisp
(defun rk/create-gtd-structure ()
  "Create the GTD directory structure with all necessary files."
  (interactive)
  (let ((org-root (expand-file-name rk/org-directory)))
    ;; Create directories
    (make-directory (concat org-root "/work") t)
    (make-directory (concat org-root "/personal") t)
    
    ;; Create work files
    (rk/ensure-org-file "work/gtd.org" 
                        "#+TITLE: Work GTD\n#+FILETAGS: @work\n\n* Tasks\n\n* Projects\n")
    (rk/ensure-org-file "work/projects.org" 
                        "#+TITLE: Work Projects\n#+FILETAGS: @work\n\n")
    (rk/ensure-org-file "work/someday.org" 
                        "#+TITLE: Work Someday/Maybe\n#+FILETAGS: @work\n\n")
    
    ;; Create personal files
    (rk/ensure-org-file "personal/gtd.org" 
                        "#+TITLE: Personal GTD\n#+FILETAGS: @personal\n\n* Tasks\n\n* Projects\n")
    (rk/ensure-org-file "personal/projects.org" 
                        "#+TITLE: Personal Projects\n#+FILETAGS: @personal\n\n")
    (rk/ensure-org-file "personal/someday.org" 
                        "#+TITLE: Personal Someday/Maybe\n#+FILETAGS: @personal\n\n")
    
    ;; Create root files
    (rk/ensure-org-file "inbox.org" 
                        "#+TITLE: Inbox\n#+FILETAGS: inbox\n\n")
    (rk/ensure-org-file "archive.org" 
                        "#+TITLE: Archive\n#+FILETAGS: archive\n\n")
    
    (message "GTD structure created successfully!")))

(defun rk/ensure-org-file (relative-path initial-content)
  "Ensure org file exists at RELATIVE-PATH with INITIAL-CONTENT if new."
  (let ((full-path (rk/org-file relative-path)))
    (unless (file-exists-p full-path)
      (with-temp-file full-path
        (insert initial-content)))))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->
<!-- For Emacs configs, this would be manual testing steps rather than unit tests -->

**Commit:** <!-- To be filled in after implementation -->

#### Task 1.1.2: Add Directory Helper Functions

**Status:** 📝 PLANNED

**Purpose:** Create helper functions that make it easy to work with the new directory structure using the existing `rk/org-file` pattern.

**Implementation Checklist:**
- [ ] Add function to get work directory path
- [ ] Add function to get personal directory path
- [ ] Add function to list all GTD files
- [ ] Add validation function to check structure integrity

**Reference Implementation:**
```elisp
(defun rk/org-work-dir ()
  "Return the path to the work org directory."
  (rk/org-file "work/"))

(defun rk/org-personal-dir ()
  "Return the path to the personal org directory."
  (rk/org-file "personal/"))

(defun rk/org-gtd-files ()
  "Return a list of all GTD org files."
  (append 
   (list (rk/org-file "inbox.org"))
   (file-expand-wildcards (concat (rk/org-work-dir) "*.org"))
   (file-expand-wildcards (concat (rk/org-personal-dir) "*.org"))))

(defun rk/validate-gtd-structure ()
  "Validate that all required GTD files exist."
  (interactive)
  (let ((required-files '("inbox.org" "archive.org"
                         "work/gtd.org" "work/projects.org" "work/someday.org"
                         "personal/gtd.org" "personal/projects.org" "personal/someday.org"))
        (missing-files '()))
    (dolist (file required-files)
      (unless (file-exists-p (rk/org-file file))
        (push file missing-files)))
    (if missing-files
        (message "Missing GTD files: %s" missing-files)
      (message "All GTD files present!"))))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

### Phase 1.2: TODO Keywords and Tags Configuration

#### Task 1.2.1: Configure TODO Keywords

**Status:** ✅ COMPLETED

**Purpose:** Set up the GTD-appropriate TODO keywords with proper state transitions and logging.

**Implementation Checklist:**
- [ ] Define TODO keyword sequences
- [ ] Add state transition triggers
- [ ] Configure logging for state changes
- [ ] Add fast selection keys

**Reference Implementation:**
```elisp
(setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
        (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")))

(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "#E35DBF" :weight bold))
        ("TODO" . (:foreground "#C678DD" :weight bold))
        ("WAITING" . (:foreground "#FE8019" :weight bold))
        ("DONE" . (:foreground "#67E480" :weight bold))
        ("CANCELLED" . (:foreground "#95A5A6" :weight bold :strike-through t))
        ("[-]" . (:foreground "#ECBE7B" :weight bold))
        ("[?]" . (:foreground "#FE8019" :weight bold))))

;; Configure state triggers
(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("WAITING" ("WAITING" . t) ("NEXT"))
        ("NEXT" ("WAITING") ("CANCELLED") ("NEXT" . t))
        (done ("WAITING") ("CANCELLED") ("NEXT"))
        ("TODO" ("WAITING") ("CANCELLED") ("NEXT"))))

;; Log state changes
(setq org-log-done 'time)
(setq org-log-into-drawer t)
```

**Implementation Notes:**
Added comprehensive GTD TODO keywords configuration to codelahoma-org.org on 2025-06-09:
- Configured NEXT, TODO, WAITING, DONE, CANCELLED, PROJECT, SOMEDAY keywords
- Set up state transition triggers with automatic tag management  
- Enabled logging for state changes with timestamps
- Added color-coded keyword faces for visual distinction

**Tests Added:** 
Manual testing completed - verified TODO keyword functionality and state transitions

**Commit:** Phase 1.2 GTD TODO keywords and context tags configuration

#### Task 1.2.2: Configure Context Tags

**Status:** ✅ COMPLETED

**Purpose:** Set up comprehensive context tags including locations, energy levels, and time estimates for effective filtering.

**Implementation Checklist:**
- [ ] Define complete tag alist with quick keys
- [ ] Configure tag faces for visual distinction
- [ ] Add tag groups for related contexts
- [ ] Set up tag inheritance rules

**Reference Implementation:**
```elisp
(setq org-tag-alist 
      '(;; Context tags
        ("@work" . ?w)
        ("@personal" . ?p)
        ("@home" . ?h)
        ("@office" . ?o)
        ("@phone" . ?c)
        ("@computer" . ?m)
        ("@errands" . ?e)
        ;; Energy tags
        ("@high_energy" . ?H)
        ("@low_energy" . ?L)
        ("@creative" . ?C)
        ("@routine" . ?R)
        ;; Time estimate tags
        ("@5min" . ?5)
        ("@15min" . ?1)
        ("@30min" . ?3)
        ("@1hr" . ?6)
        ("@deep" . ?D)))

;; Configure tag faces
(setq org-tag-faces
      '(("@work" . (:foreground "#3E4C5E" :weight bold))
        ("@personal" . (:foreground "#6A5ACD" :weight bold))
        ("@high_energy" . (:foreground "#FF6347" :weight bold))
        ("@low_energy" . (:foreground "#87CEEB" :weight bold))
        ("@5min" . (:foreground "#90EE90" :weight bold))
        ("@deep" . (:foreground "#4B0082" :weight bold))))

;; Configure tag groups
(setq org-tag-groups-alist
      '(("@work" (:startgroup))
        ("@office" . nil)
        ("@computer" . nil)
        ("@phone" . nil)
        ("@work" (:endgroup))
        ("@personal" (:startgroup))
        ("@home" . nil)
        ("@errands" . nil)
        ("@personal" (:endgroup))))
```

**Implementation Notes:**
Added comprehensive context tags configuration to codelahoma-org.org on 2025-06-09:
- Defined location contexts: @work, @home, @office, @phone, @computer, @errands
- Added energy level tags: @high_energy, @low_energy, @creative, @routine
- Set up time estimate tags: @5min, @15min, @30min, @1hr, @deep
- Configured tag faces for visual distinction
- Set up fast selection keys for efficient tagging

**Tests Added:** 
Manual testing completed - verified context tags functionality and fast selection

**Commit:** Phase 1.2 GTD TODO keywords and context tags configuration

### Phase 1.3: Archive Configuration

#### Task 1.3.1: Configure Archive Location and Behavior

**Status:** 📝 PLANNED

**Purpose:** Set up intelligent archiving that maintains context while keeping active files clean.

**Implementation Checklist:**
- [ ] Configure archive file location
- [ ] Set up archive hierarchy preservation
- [ ] Add archive timestamp properties
- [ ] Create quick archive functions

**Reference Implementation:**
```elisp
;; Configure archive location
(setq org-archive-location (concat (rk/org-file "archive.org") "::* From %s"))

;; Preserve hierarchy in archive
(setq org-archive-subtree-add-inherited-tags t)

;; Add timestamp when archiving
(defun rk/org-archive-add-timestamp ()
  "Add archive timestamp to entry."
  (org-set-property "ARCHIVED_AT" (format-time-string "[%Y-%m-%d %a %H:%M]")))

(add-hook 'org-archive-hook 'rk/org-archive-add-timestamp)

;; Quick archive functions
(defun rk/archive-done-tasks ()
  "Archive all DONE tasks in current buffer."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))

(defun rk/archive-old-tasks ()
  "Archive tasks completed more than 30 days ago."
  (interactive)
  (let ((cutoff (format-time-string 
                 "%Y-%m-%d" 
                 (time-subtract (current-time) (days-to-time 30)))))
    (org-map-entries
     (lambda ()
       (let ((closed (org-entry-get nil "CLOSED")))
         (when (and closed (string< closed cutoff))
           (org-archive-subtree)
           (setq org-map-continue-from 
                 (org-element-property :begin (org-element-at-point))))))
     "/DONE" 'file)))
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
   - Tangle the file to generate `codelahoma-org.el`
   - Reload the configuration or restart Spacemacs

2. **Testing:**
   - Test each function interactively
   - Verify file creation and structure
   - Test TODO state transitions
   - Validate tag filtering
   - Test archive operations

3. **Documentation:**
   - Add docstrings to all functions
   - Update this implementation plan with notes
   - Document any deviations from the plan

4. **Commit:**
   - Stage changes to both .org and .el files
   - Commit with descriptive message:
     ```
     feat: implement GTD core infrastructure phase 1
     
     - Create GTD directory structure with work/personal separation
     - Configure TODO keywords with state triggers
     - Set up comprehensive context tags
     - Configure intelligent archiving
     ```

## Testing Strategy

### Manual Testing Checklist:

1. **Directory Structure:**
   - [ ] Run `rk/create-gtd-structure` and verify all files created
   - [ ] Check file permissions are correct
   - [ ] Verify initial content in each file

2. **TODO Keywords:**
   - [ ] Create tasks with each TODO keyword
   - [ ] Test state transitions with `C-c C-t`
   - [ ] Verify logging works correctly
   - [ ] Check keyword faces display properly

3. **Context Tags:**
   - [ ] Test tag assignment with `C-c C-c`
   - [ ] Verify fast selection keys work
   - [ ] Test tag filtering in agenda views
   - [ ] Check tag inheritance

4. **Archive Operations:**
   - [ ] Archive individual tasks
   - [ ] Test bulk archive functions
   - [ ] Verify archive structure preservation
   - [ ] Check timestamp properties

## Rollback Plan

In case of issues:

1. Restore original org files from backup
2. Remove new directory structure
3. Comment out new configuration in `codelahoma-org.org`
4. Restart Spacemacs with original configuration

## Conclusion

This implementation plan provides the foundation for a robust GTD system in Spacemacs. Phase 1 establishes the core infrastructure that all subsequent phases will build upon, ensuring a solid base for task management and organization.