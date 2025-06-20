# Personal GTD-Zettelkasten Phase 1 Implementation Plan

## Overview

This document outlines the detailed implementation plan for Phase 1: Foundation Setup of the Personal GTD-Zettelkasten Hybrid System. This phase establishes the core infrastructure, directory structure, and development workflow for building a custom GTD engine integrated with org-roam.

## Purpose

This implementation aims to:

1. **Establish Project Structure** - Create the foundational directory and file organization
2. **Configure Org-Roam** - Set up and configure the knowledge management base
3. **Initialize Development Workflow** - Create testing and development infrastructure
4. **Set Up Keybinding Framework** - Establish the `SPC o o` namespace foundation

## Prerequisites

Before starting the implementation:

- [ ] Emacs 28+ with native compilation enabled
- [ ] Spacemacs configuration loaded and working
- [ ] Git repository ready for version control
- [ ] Backup of current org files completed

## Implementation Plan

### Phase 1: Foundation Setup (Week 1)

#### Task 1.1: Create Project Structure

**Status:** ✅ COMPLETE

**Purpose:** Establish the directory structure and core configuration files for the GTD-Zettelkasten system.

**Implementation Checklist:**
- [x] Create main elisp directory structure
- [x] Initialize core configuration files
- [x] Set up autoload paths
- [x] Create placeholder files for each module

**Reference Implementation:**
```elisp
;; Directory structure to create:
;; ~/.spacemacs.d/
;; ├── codelahoma-gtd/
;; │   ├── codelahoma-gtd-core.el
;; │   ├── codelahoma-gtd-capture.el
;; │   ├── codelahoma-gtd-process.el
;; │   ├── codelahoma-gtd-review.el
;; │   └── codelahoma-gtd-config.el
;; ├── codelahoma-bridge.el
;; └── codelahoma-ui.el

;; In codelahoma-gtd-config.el:
(defgroup codelahoma-gtd nil
  "Personal GTD system configuration."
  :group 'org
  :prefix "codelahoma-gtd-")

(defcustom codelahoma-gtd-directory "~/personal/org-files/gtd/"
  "Directory containing GTD org files."
  :type 'directory
  :group 'codelahoma-gtd)

(provide 'codelahoma-gtd-config)
```

**Implementation Notes:**
- Created all GTD modules as planned in ~/.spacemacs.d/codelahoma-gtd/
- Implemented error handling for missing modules (bridge is optional for Phase 1)
- Discovered homesick symlink requirement - modules must be linked from dotfiles repo
- Added file existence checks before loading to prevent errors
- Made codelahoma-bridge.el optional since it's a Phase 5 feature
- Loading configuration integrated into dotspacemacs.org with proper error handling

**Tests Added:** 
- Manual verification of module loading in Spacemacs
- Created test script to verify symlinks exist
- Tested reload functionality with SPC o o d r
- Verified keybindings appear in which-key under SPC o o

**Commit:** 
```
feat(gtd): Implement Phase 1 foundation - project structure and module loading

- Created core GTD modules: config, core, capture, process, review
- Set up directory structure in ~/.spacemacs.d/codelahoma-gtd/
- Integrated loading into dotspacemacs.org with error handling
- Made bridge module optional (Phase 5 feature)
- Added development reload commands
- Fixed homesick symlink requirements
```

#### Task 1.2: Install and Configure Org-Roam

**Status:** ✅ COMPLETE

**Purpose:** Set up org-roam as the knowledge management foundation with personal configuration.

**Implementation Checklist:**
- [x] Add org-roam to Spacemacs configuration
- [x] Configure org-roam directory paths
- [x] Set up capture templates for knowledge notes
- [x] Initialize org-roam database
- [x] Test basic org-roam functionality

**Reference Implementation:**
```elisp
;; In dotspacemacs/user-config:
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/personal/org-files/knowledge/")
  (org-roam-db-location "~/personal/org-files/.org-roam.db")
  (org-roam-completion-everywhere t)
  :config
  (setq org-roam-node-display-template 
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  
  ;; Personal capture templates
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+created: %U\n#+filetags: \n\n")
           :unnarrowed t)
          ("p" "permanent" plain "%?"
           :target (file+head "permanent/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+created: %U\n#+type: permanent\n#+filetags: \n\n")
           :unnarrowed t)
          ("l" "literature" plain "%?"
           :target (file+head "literature/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+author: %^{Author}\n#+created: %U\n#+type: literature\n#+filetags: \n\n")
           :unnarrowed t))))
```

**Implementation Notes:**
- Created codelahoma-gtd-roam.el module for org-roam configuration
- Integrated org-roam setup into GTD initialization process
- Added capture templates for permanent, literature, reference, and project notes
- Configured daily notes with morning pages template
- Added quick capture functions and search helpers
- Extended keybindings under SPC o o z for all org-roam features
- Updated symlink setup script to include roam module

**Tests Added:** 
- Created test-org-roam.el for verification
- Tests check org-roam loading, directories, database, and keybindings
- Manual testing of capture templates and node creation

**Commit:** 
```
feat(gtd): Implement org-roam configuration for Phase 1

- Created codelahoma-gtd-roam.el with full org-roam setup
- Added capture templates for different note types
- Configured knowledge directory structure
- Extended keybindings for Zettelkasten features
- Updated setup script for complete module linking
```

#### Task 1.3: Verify Directory Structure

**Status:** ✅ COMPLETE

**Purpose:** Ensure all GTD and knowledge directories exist with proper permissions.

**Implementation Checklist:**
- [x] Create directory initialization function
- [x] Verify all GTD subdirectories exist
- [x] Set up archive directories
- [x] Create initial placeholder files
- [x] Add directory validation on startup

**Reference Implementation:**
```elisp
;; In codelahoma-gtd-core.el:
(defun codelahoma-gtd-ensure-directories ()
  "Ensure all GTD directories exist."
  (let ((dirs '("gtd" "gtd/archive" "gtd/reviews"
                "knowledge" "knowledge/permanent" 
                "knowledge/literature" "knowledge/references"
                "knowledge/projects" "knowledge/daily"
                "areas" "resources" "resources/templates"
                "resources/checklists")))
    (dolist (dir dirs)
      (let ((full-path (expand-file-name dir "~/personal/org-files/")))
        (unless (file-exists-p full-path)
          (make-directory full-path t)
          (message "Created directory: %s" full-path))))))

(defun codelahoma-gtd-verify-files ()
  "Verify all required GTD files exist."
  (let ((files '("inbox.org" "projects.org" "next-actions.org"
                 "waiting-for.org" "someday.org" "calendar.org" 
                 "media.org")))
    (dolist (file files)
      (let ((full-path (expand-file-name 
                        (concat "gtd/" file) 
                        "~/personal/org-files/")))
        (unless (file-exists-p full-path)
          (with-temp-buffer
            (insert (format "#+TITLE: %s\n#+FILETAGS: :gtd:\n\n" 
                           (capitalize (replace-regexp-in-string 
                                       "-" " " 
                                       (file-name-sans-extension file)))))
            (write-file full-path))
          (message "Created file: %s" full-path))))))
```

**Implementation Notes:**
- Enhanced directory validation with comprehensive checking
- Added startup validation hook to ensure structure on every load
- Created validation function that reports specific missing items
- Added template files for weekly review and project planning
- Created travel checklist as example resource
- All directories confirmed to exist in ~/personal/org-files/
- Added manual validation command: SPC o o d v

**Tests Added:** 
- Created verify-gtd-structure.el for comprehensive testing
- Manual validation shows all directories and files exist
- Startup hook ensures validation on each Spacemacs load
- Permission checks confirm read/write access

**Commit:** 
```
feat(gtd): Complete directory structure verification for Phase 1

- Enhanced validation with detailed reporting
- Added startup hook for automatic structure checking
- Created template files (weekly review, project, travel checklist)
- Added manual validation command (SPC o o d v)
- All required directories confirmed present
```

#### Task 1.4: Initialize Keybinding Namespace

**Status:** 📝 PLANNED

**Purpose:** Set up the foundational `SPC o o` keybinding namespace for all GTD-Zettelkasten commands.

**Implementation Checklist:**
- [ ] Create base keybinding structure
- [ ] Set up menu system framework
- [ ] Add placeholder bindings for future commands
- [ ] Integrate with Spacemacs which-key
- [ ] Test keybinding conflicts

**Reference Implementation:**
```elisp
;; In codelahoma-ui.el:
(defun codelahoma-gtd-setup-keybindings ()
  "Set up GTD keybindings under SPC o o."
  (spacemacs/declare-prefix "o o" "GTD/Zettelkasten")
  
  ;; Capture submenu
  (spacemacs/declare-prefix "o o c" "capture")
  (spacemacs/set-leader-keys
    "o o c i" (lambda () (interactive) (message "Inbox capture - coming soon"))
    "o o c c" (lambda () (interactive) (message "Generic capture - coming soon")))
  
  ;; Process submenu
  (spacemacs/declare-prefix "o o p" "process")
  (spacemacs/set-leader-keys
    "o o p i" (lambda () (interactive) (message "Process inbox - coming soon")))
  
  ;; Navigate submenu
  (spacemacs/declare-prefix "o o n" "navigate")
  (spacemacs/set-leader-keys
    "o o n i" (lambda () (interactive) (find-file "~/personal/org-files/gtd/inbox.org"))
    "o o n p" (lambda () (interactive) (find-file "~/personal/org-files/gtd/projects.org")))
  
  ;; Review submenu
  (spacemacs/declare-prefix "o o r" "review")
  
  ;; Agenda submenu
  (spacemacs/declare-prefix "o o a" "agenda")
  
  ;; Zettelkasten submenu
  (spacemacs/declare-prefix "o o z" "zettelkasten")
  (spacemacs/set-leader-keys
    "o o z n" 'org-roam-node-find
    "o o z i" 'org-roam-node-insert
    "o o z c" 'org-roam-capture)
  
  ;; Save command
  (spacemacs/set-leader-keys
    "o o s" 'org-save-all-org-buffers))

;; Add to Spacemacs configuration
(with-eval-after-load 'org
  (codelahoma-gtd-setup-keybindings))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

#### Task 1.5: Create Development and Testing Framework

**Status:** 📝 PLANNED

**Purpose:** Establish a simple testing and development workflow for iterating on the GTD system.

**Implementation Checklist:**
- [ ] Create test configuration loader
- [ ] Set up isolated test environment
- [ ] Create performance benchmarking utilities
- [ ] Add configuration reload commands
- [ ] Document development workflow

**Reference Implementation:**
```elisp
;; In codelahoma-gtd-dev.el:
(defun codelahoma-gtd-reload ()
  "Reload all GTD configuration files."
  (interactive)
  (dolist (feature '(codelahoma-gtd-config
                     codelahoma-gtd-core
                     codelahoma-gtd-capture
                     codelahoma-gtd-process
                     codelahoma-gtd-review
                     codelahoma-bridge
                     codelahoma-ui))
    (unload-feature feature t))
  (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-config.el")
  (load-file "~/.spacemacs.d/codelahoma-gtd/codelahoma-gtd-core.el")
  (message "GTD system reloaded"))

(defun codelahoma-gtd-benchmark-capture ()
  "Benchmark capture performance."
  (interactive)
  (let ((start-time (current-time)))
    ;; Simulate capture operation
    (org-capture nil "i")
    (insert "Test capture item")
    (org-capture-finalize)
    (message "Capture completed in %.3f seconds"
             (float-time (time-subtract (current-time) start-time)))))

;; Add development keybindings
(spacemacs/set-leader-keys
  "o o d r" 'codelahoma-gtd-reload
  "o o d b" 'codelahoma-gtd-benchmark-capture)
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

## Implementation Process

Each task should follow this standard process:

1. **Development:**
   - Create new files in `~/.spacemacs.d/` directory
   - Implement functionality incrementally
   - Test in running Spacemacs instance

2. **Testing:**
   - Use `codelahoma-gtd-reload` to test changes
   - Verify keybindings don't conflict
   - Test with sample data

3. **Documentation:**
   - Update implementation notes with decisions made
   - Document any deviations from plan
   - Note performance characteristics

4. **Commit:**
   - Stage changes to dotfiles repository
   - Commit with descriptive message:
     ```
     feat(gtd): Implement Phase 1 foundation - [specific component]
     
     - [What was implemented]
     - [Key decisions made]
     - [Any notable changes from plan]
     ```

## Testing Strategy

For Phase 1, testing will be primarily manual:

1. **Directory Creation:** Verify all directories are created correctly
2. **File Initialization:** Ensure placeholder files have correct structure
3. **Org-Roam Integration:** Test node creation and linking
4. **Keybinding Verification:** Check all bindings work and show in which-key
5. **Performance Baseline:** Establish capture/load time benchmarks

## Rollback Plan

Since this is foundation setup:

1. Keep backup of current `~/.spacemacs.d/` configuration
2. Document any changes to `dotspacemacs/user-config`
3. Org-roam can be disabled by commenting out configuration
4. Keybindings can be removed without affecting core Spacemacs

## Success Criteria

Phase 1 is complete when:

- [ ] All directory structures exist and are accessible
- [ ] Org-roam is installed and creating notes successfully
- [ ] Basic keybinding structure shows in which-key
- [ ] Development reload workflow is functional
- [ ] Initial performance benchmarks are recorded

## Next Phase Preview

Phase 2 will build the Custom GTD Engine Core:
- Task state management
- Capture template system
- Basic inbox operations
- Project structure definition

This foundation phase ensures a solid base for all subsequent development.