# Org-GTD Phase 3: Context Switching and Keybindings Implementation

## Overview

This document outlines a detailed implementation plan for Phase 3 of the Org-GTD system: Context Switching and Keybindings. This phase creates the intelligent context-aware system that allows seamless switching between work and personal modes, along with comprehensive keybindings for all GTD operations.

## Purpose

This implementation aims to:

1. **Enable Context Awareness** - Create functions that switch between work, personal, and unified contexts
2. **Streamline Workflow Access** - Implement comprehensive keybindings under a unified prefix
3. **Enhance User Experience** - Provide visual indicators and which-key descriptions for all operations

## Prerequisites

Before starting the implementation:

- [ ] Phase 1 (Core Infrastructure) is complete with directory structure created
- [ ] Phase 2 (Capture and Agenda) is implemented and tested
- [ ] Understanding of Spacemacs keybinding conventions
- [ ] Which-key package is available for keybinding descriptions

## Implementation Plan

### Phase 3.1: Context Functions

#### Task 3.1.1: Core Context Mode Functions

**Status:** 📝 PLANNED

**Purpose:** Create the fundamental mode-switching functions that control which files are included in agenda views and capture operations.

**Implementation Checklist:**
- [ ] Create org-work-mode function
- [ ] Create org-personal-mode function
- [ ] Create org-unified-mode function
- [ ] Add mode state tracking variable
- [ ] Configure mode persistence across sessions

**Reference Implementation:**
```elisp
;; Context state variable
(defvar rk/org-context-mode 'unified
  "Current org context mode: 'work, 'personal, or 'unified")

;; Persist context mode across sessions
(defun rk/save-org-context-mode ()
  "Save current org context mode."
  (with-temp-file (rk/org-file ".org-context-mode")
    (prin1 rk/org-context-mode (current-buffer))))

(defun rk/load-org-context-mode ()
  "Load saved org context mode."
  (let ((context-file (rk/org-file ".org-context-mode")))
    (when (file-exists-p context-file)
      (with-temp-buffer
        (insert-file-contents context-file)
        (setq rk/org-context-mode (read (current-buffer)))))))

;; Load context on startup
(add-hook 'after-init-hook 'rk/load-org-context-mode)

;; Core mode functions
(defun org-work-mode ()
  "Switch to work-only org mode."
  (interactive)
  (setq rk/org-context-mode 'work)
  (setq org-agenda-files 
        (list (rk/org-file "inbox.org")
              (rk/org-file "work/gtd.org")
              (rk/org-file "work/projects.org")
              (rk/org-file "work/someday.org")))
  (rk/update-capture-templates)
  (rk/save-org-context-mode)
  (message "Org Work Mode activated")
  (rk/show-context-indicator))

(defun org-personal-mode ()
  "Switch to personal-only org mode."
  (interactive)
  (setq rk/org-context-mode 'personal)
  (setq org-agenda-files 
        (list (rk/org-file "inbox.org")
              (rk/org-file "personal/gtd.org")
              (rk/org-file "personal/projects.org")
              (rk/org-file "personal/someday.org")))
  (rk/update-capture-templates)
  (rk/save-org-context-mode)
  (message "Org Personal Mode activated")
  (rk/show-context-indicator))

(defun org-unified-mode ()
  "Switch to unified org mode (both work and personal)."
  (interactive)
  (setq rk/org-context-mode 'unified)
  (setq org-agenda-files (rk/org-gtd-files))
  (rk/update-capture-templates)
  (rk/save-org-context-mode)
  (message "Org Unified Mode activated")
  (rk/show-context-indicator))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

#### Task 3.1.2: Context Helper Functions

**Status:** 📝 PLANNED

**Purpose:** Create supporting functions that update capture templates, provide visual feedback, and integrate with the modeline.

**Implementation Checklist:**
- [ ] Create capture template updater
- [ ] Add visual context indicator
- [ ] Create modeline integration
- [ ] Add context quick-switch hydra
- [ ] Create context status reporter

**Reference Implementation:**
```elisp
;; Update capture templates based on context
(defun rk/update-capture-templates ()
  "Update capture templates based on current context mode."
  (setq org-capture-templates
        (append
         ;; Universal templates (always available)
         '(("i" "Inbox" entry (file+headline org-default-notes-file "Inbox")
            "* TODO %?\n  CAPTURED: %U\n  %i"
            :empty-lines 1)
           ("n" "Quick Note" entry (file+headline org-default-notes-file "Notes")
            "* %? :note:\n  CAPTURED: %U\n  %i"
            :empty-lines 1
            :immediate-finish t))
         
         ;; Context-specific templates
         (cond
          ((eq rk/org-context-mode 'work)
           '(("t" "Work Task" entry (file+headline (rk/org-file "work/gtd.org") "Tasks")
              "* TODO %? :@work:\n  SCHEDULED: %t\n  CAPTURED: %U\n  %i"
              :empty-lines 1)
             ("p" "Work Project" entry (file+headline (rk/org-file "work/projects.org") "Projects")
              "* TODO %^{Project Name} [/] :@work:project:\n  %^{Description}\n** TODO %?"
              :empty-lines 1)
             ("m" "Meeting" entry (file+headline (rk/org-file "work/gtd.org") "Meetings")
              "* MEETING %^{Meeting Title} :@work:meeting:\n  SCHEDULED: %^T"
              :empty-lines 1)))
          
          ((eq rk/org-context-mode 'personal)
           '(("t" "Personal Task" entry (file+headline (rk/org-file "personal/gtd.org") "Tasks")
              "* TODO %? :@personal:\n  SCHEDULED: %t\n  CAPTURED: %U\n  %i"
              :empty-lines 1)
             ("p" "Personal Project" entry (file+headline (rk/org-file "personal/projects.org") "Projects")
              "* TODO %^{Project Name} [/] :@personal:project:\n  %^{Description}\n** TODO %?"
              :empty-lines 1)))
          
          ((eq rk/org-context-mode 'unified)
           '(("t" "Task")
             ("tw" "Work Task" entry (file+headline (rk/org-file "work/gtd.org") "Tasks")
              "* TODO %? :@work:\n  SCHEDULED: %t\n  CAPTURED: %U\n  %i"
              :empty-lines 1)
             ("tp" "Personal Task" entry (file+headline (rk/org-file "personal/gtd.org") "Tasks")
              "* TODO %? :@personal:\n  SCHEDULED: %t\n  CAPTURED: %U\n  %i"
              :empty-lines 1)
             ("p" "Project")
             ("pw" "Work Project" entry (file+headline (rk/org-file "work/projects.org") "Projects")
              "* TODO %^{Project Name} [/] :@work:project:\n  %^{Description}\n** TODO %?"
              :empty-lines 1)
             ("pp" "Personal Project" entry (file+headline (rk/org-file "personal/projects.org") "Projects")
              "* TODO %^{Project Name} [/] :@personal:project:\n  %^{Description}\n** TODO %?"
              :empty-lines 1))))))

;; Visual context indicator
(defun rk/show-context-indicator ()
  "Show current context in a temporary buffer."
  (let ((indicator (cond
                    ((eq rk/org-context-mode 'work) "🏢 WORK MODE")
                    ((eq rk/org-context-mode 'personal) "🏠 PERSONAL MODE")
                    ((eq rk/org-context-mode 'unified) "🌐 UNIFIED MODE"))))
    (momentary-string-display 
     (concat "\n\n    " indicator " \n\n") 
     (window-start) 
     1)))

;; Modeline integration
(defun rk/org-context-modeline-string ()
  "Return a string representing current org context for modeline."
  (cond
   ((eq rk/org-context-mode 'work) " [W]")
   ((eq rk/org-context-mode 'personal) " [P]")
   ((eq rk/org-context-mode 'unified) " [U]")
   (t "")))

;; Add to modeline
(with-eval-after-load 'spaceline
  (spaceline-define-segment org-context
    "Show current org context mode."
    (when (derived-mode-p 'org-mode 'org-agenda-mode)
      (rk/org-context-modeline-string)))
  
  (spaceline-spacemacs-theme 'org-context))

;; Context switching hydra
(with-eval-after-load 'hydra
  (defhydra rk/org-context-hydra (:hint nil :exit t)
    "
Org Context: %(rk/org-context-modeline-string)
_w_: Work Mode    _p_: Personal Mode    _u_: Unified Mode    _q_: quit
"
    ("w" org-work-mode)
    ("p" org-personal-mode)
    ("u" org-unified-mode)
    ("q" nil)))
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

### Phase 3.2: Complete Keybinding Setup

#### Task 3.2.1: Core GTD Keybindings

**Status:** 📝 PLANNED

**Purpose:** Implement the comprehensive keybinding structure under the `SPC o o` prefix for all GTD operations.

**Implementation Checklist:**
- [ ] Define main org prefix key
- [ ] Add capture keybindings
- [ ] Add agenda keybindings
- [ ] Add file navigation bindings
- [ ] Add mode switching bindings
- [ ] Configure which-key descriptions

**Reference Implementation:**
```elisp
;; Main org prefix
(spacemacs/declare-prefix "o" "org")
(spacemacs/declare-prefix "oo" "org-gtd")

;; Capture bindings
(spacemacs/declare-prefix "ooc" "capture")
(spacemacs/set-leader-keys
  "ooc" 'org-capture
  "ooci" (lambda () (interactive) (org-capture nil "i"))
  "oocn" (lambda () (interactive) (org-capture nil "n")))

;; Context-aware capture bindings
(defun rk/context-capture-task ()
  "Capture task based on current context."
  (interactive)
  (cond
   ((eq rk/org-context-mode 'work) (org-capture nil "t"))
   ((eq rk/org-context-mode 'personal) (org-capture nil "t"))
   ((eq rk/org-context-mode 'unified) (org-capture nil "tw"))))

(defun rk/context-capture-project ()
  "Capture project based on current context."
  (interactive)
  (cond
   ((eq rk/org-context-mode 'work) (org-capture nil "p"))
   ((eq rk/org-context-mode 'personal) (org-capture nil "p"))
   ((eq rk/org-context-mode 'unified) (org-capture nil "pw"))))

(spacemacs/set-leader-keys
  "ooct" 'rk/context-capture-task
  "oocp" 'rk/context-capture-project)

;; Work-specific captures (only in unified mode)
(when (eq rk/org-context-mode 'unified)
  (spacemacs/set-leader-keys
    "oocw" (lambda () (interactive) (org-capture nil "tw"))
    "oocW" (lambda () (interactive) (org-capture nil "pw"))))

;; Personal-specific captures (only in unified mode)
(when (eq rk/org-context-mode 'unified)
  (spacemacs/set-leader-keys
    "oocp" (lambda () (interactive) (org-capture nil "tp"))
    "oocP" (lambda () (interactive) (org-capture nil "pp"))))

;; Agenda bindings
(spacemacs/declare-prefix "ooa" "agenda")
(spacemacs/set-leader-keys
  "ooa" 'org-agenda
  "ooaw" (lambda () (interactive) (org-agenda nil "w"))
  "ooap" (lambda () (interactive) (org-agenda nil "p"))
  "ooau" (lambda () (interactive) (org-agenda nil "u"))
  "ooaf" (lambda () (interactive) (org-agenda nil "f"))
  "ooae" 'rk/org-energy-agenda-hydra
  "ooas" (lambda () (interactive) (org-agenda nil "s")))

;; File navigation
(spacemacs/declare-prefix "oog" "go-to-file")
(spacemacs/set-leader-keys
  "oogi" (lambda () (interactive) (find-file (rk/org-file "inbox.org")))
  "ooga" (lambda () (interactive) (find-file (rk/org-file "archive.org"))))

;; Context-aware file navigation
(defun rk/goto-gtd ()
  "Go to GTD file based on current context."
  (interactive)
  (find-file
   (cond
    ((eq rk/org-context-mode 'work) (rk/org-file "work/gtd.org"))
    ((eq rk/org-context-mode 'personal) (rk/org-file "personal/gtd.org"))
    (t (rk/org-file "work/gtd.org")))))

(defun rk/goto-projects ()
  "Go to projects file based on current context."
  (interactive)
  (find-file
   (cond
    ((eq rk/org-context-mode 'work) (rk/org-file "work/projects.org"))
    ((eq rk/org-context-mode 'personal) (rk/org-file "personal/projects.org"))
    (t (rk/org-file "work/projects.org")))))

(spacemacs/set-leader-keys
  "oogg" 'rk/goto-gtd
  "oogp" 'rk/goto-projects
  "oogw" (lambda () (interactive) (find-file (rk/org-file "work/gtd.org")))
  "oogW" (lambda () (interactive) (find-file (rk/org-file "work/projects.org")))
  "oogP" (lambda () (interactive) (find-file (rk/org-file "personal/projects.org"))))

;; Mode switching
(spacemacs/declare-prefix "oom" "mode")
(spacemacs/set-leader-keys
  "oomw" 'org-work-mode
  "oomp" 'org-personal-mode
  "oomu" 'org-unified-mode
  "oomm" 'rk/org-context-hydra/body)
```

**Implementation Notes:**
<!-- To be filled in after implementation -->

**Tests Added:** 
<!-- To be filled in after implementation -->

**Commit:** <!-- To be filled in after implementation -->

#### Task 3.2.2: Extended Keybindings and Integration

**Status:** 📝 PLANNED

**Purpose:** Add keybindings for refile, clocking, reviews, and integrate with existing extension system.

**Implementation Checklist:**
- [ ] Add refile keybindings
- [ ] Add clocking keybindings
- [ ] Add review keybindings
- [ ] Integrate with codelahoma-org extensions
- [ ] Create comprehensive keybinding cheatsheet
- [ ] Add keybinding discovery functions

**Reference Implementation:**
```elisp
;; Refile operations
(spacemacs/declare-prefix "oor" "refile")
(spacemacs/set-leader-keys
  "oor" 'org-refile
  "oorr" 'rk/org-refile-hydra/body
  "oorw" 'rk/refile-to-work-gtd
  "oorp" 'rk/refile-to-personal-gtd
  "oorc" 'rk/refile-to-current-clock
  "oora" 'org-archive-subtree)

;; Clocking operations
(spacemacs/declare-prefix "ook" "clock")
(spacemacs/set-leader-keys
  "ooki" 'org-clock-in
  "ooko" 'org-clock-out
  "ookj" 'org-clock-goto
  "ookl" 'org-clock-in-last
  "ookr" 'org-clock-report
  "ookd" 'org-clock-display)

;; Review operations
(spacemacs/declare-prefix "ooR" "review")
(spacemacs/set-leader-keys
  "ooRw" 'rk/weekly-review
  "ooRd" 'rk/daily-review
  "ooRp" 'rk/process-inbox
  "ooRs" (lambda () (interactive) (org-agenda nil "s")))

;; Archive operations
(spacemacs/declare-prefix "ooA" "archive")
(spacemacs/set-leader-keys
  "ooAa" 'org-archive-subtree
  "ooAd" 'rk/archive-done-tasks
  "ooAo" 'rk/archive-old-tasks
  "ooAf" (lambda () (interactive) (find-file (rk/org-file "archive.org"))))

;; Extension integration
(spacemacs/declare-prefix "oox" "extensions")
(with-eval-after-load 'codelahoma-org
  ;; Extensions will add their bindings here
  (spacemacs/set-leader-keys
    "ooxt" 'rk/tangle-codelahoma-org
    "ooxr" 'rk/reload-codelahoma-org))

;; Keybinding cheatsheet
(defun rk/org-gtd-cheatsheet ()
  "Display org-gtd keybinding cheatsheet."
  (interactive)
  (let ((cheatsheet "
ORG-GTD KEYBINDINGS (SPC o o ...)

CAPTURE (c):            AGENDA (a):           GO TO FILE (g):
  c   - capture           a   - agenda list     i   - inbox
  c i - inbox            w   - work view       g   - gtd (context)
  c t - task (context)   p   - personal view   p   - projects (context)
  c p - project          u   - unified view    w   - work gtd
  c n - quick note       f   - focus (top 3)   W   - work projects
                         s   - stalled items   P   - personal projects

MODE (m):              REFILE (r):           CLOCK (k):
  w   - work mode        r   - refile          i   - clock in
  p   - personal mode    r r - refile hydra    o   - clock out
  u   - unified mode     w   - to work         j   - jump to clock
  m   - mode hydra       p   - to personal     l   - clock in last
                         c   - to clock        r   - clock report

REVIEW (R):            ARCHIVE (A):          EXTENSIONS (x):
  w   - weekly review    a   - archive task    t   - tangle config
  d   - daily review     d   - archive done    r   - reload config
  p   - process inbox    o   - archive old     
  s   - stalled items    f   - go to archive   

Current Mode: %s
"))
    (with-output-to-temp-buffer "*Org-GTD Cheatsheet*"
      (princ (format cheatsheet 
                     (upcase (symbol-name rk/org-context-mode)))))))

(spacemacs/set-leader-keys
  "ooh" 'rk/org-gtd-cheatsheet
  "oo?" 'rk/org-gtd-cheatsheet)

;; Which-key descriptions
(which-key-add-key-based-replacements
  "SPC o o" "org-gtd"
  "SPC o o c" "capture"
  "SPC o o a" "agenda"
  "SPC o o g" "go-to"
  "SPC o o m" "mode"
  "SPC o o r" "refile"
  "SPC o o k" "clock"
  "SPC o o R" "review"
  "SPC o o A" "archive"
  "SPC o o x" "extensions")
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
   - Test mode switching between work/personal/unified
   - Verify agenda files update correctly
   - Test all keybindings in each mode
   - Verify capture templates adapt to context
   - Test modeline indicator displays

3. **Documentation:**
   - Update user guide with context modes
   - Document all keybindings
   - Create mode-switching workflow guide
   - Add troubleshooting for common issues

4. **Commit:**
   - Stage changes to both .org and .el files
   - Commit with descriptive message:
     ```
     feat: implement GTD context switching and keybindings
     
     - Add work/personal/unified context modes
     - Implement comprehensive SPC o o keybinding structure
     - Create context-aware capture and navigation
     - Add visual indicators and which-key integration
     ```

## Testing Strategy

### Manual Testing Checklist:

1. **Context Switching:**
   - [ ] Switch to work mode and verify only work files in agenda
   - [ ] Switch to personal mode and verify only personal files
   - [ ] Switch to unified mode and verify all files included
   - [ ] Verify mode persists across Spacemacs restart
   - [ ] Check modeline indicator updates

2. **Keybindings:**
   - [ ] Test all capture keybindings in each mode
   - [ ] Verify agenda shortcuts open correct views
   - [ ] Test file navigation goes to context-appropriate files
   - [ ] Verify refile shortcuts work correctly
   - [ ] Test which-key shows descriptions

3. **Integration:**
   - [ ] Capture templates update when switching modes
   - [ ] Agenda views reflect current mode
   - [ ] Refile targets adapt to context
   - [ ] Extensions keybindings remain accessible

4. **User Experience:**
   - [ ] Visual feedback appears when switching modes
   - [ ] Cheatsheet displays current mode
   - [ ] Keybindings feel intuitive and consistent

## Rollback Plan

In case of issues:

1. Remove context mode functions from configuration
2. Reset org-agenda-files to original static list
3. Remove new keybindings from `codelahoma-org.org`
4. Delete .org-context-mode file
5. Restart Spacemacs with original bindings

## Conclusion

This implementation creates a powerful context-aware GTD system that adapts to your current focus. The comprehensive keybinding structure under `SPC o o` provides quick access to all GTD operations while maintaining the flexibility to work in focused work or personal modes. The visual indicators and which-key integration ensure the system remains discoverable and user-friendly.