---
title: GTD Implementation Plan - 120 Keybinding System
type: note
permalink: plans/gtd-implementation/gtd-implementation-plan-120-keybinding-system
---

# GTD Implementation Plan - 120 Keybinding System

## Component Overview

Implement the hierarchical keybinding system using `SPC o o` namespace with which-key integration for discoverable GTD workflows.

## Subcomponents

### 120. Base Keybinding Infrastructure

**Namespace Declaration**:
```elisp
#+NAME: gtd-keybinding-base
#+BEGIN_SRC elisp
(defun gtd/setup-keybindings ()
  "Set up GTD keybinding hierarchy."
  ;; Main GTD namespace
  (spacemacs/declare-prefix "o o" "gtd")
  
  ;; Primary categories
  (spacemacs/declare-prefix "o o c" "capture")
  (spacemacs/declare-prefix "o o p" "process")  
  (spacemacs/declare-prefix "o o r" "review")
  (spacemacs/declare-prefix "o o f" "focus")
  (spacemacs/declare-prefix "o o q" "quick")
  
  ;; Development (temporary)
  (spacemacs/declare-prefix "o o d" "dev")
  
  (gtd/log "GTD keybindings configured"))
#+END_SRC
```

### 121. Capture Keybindings

**Capture Hierarchy**:
```elisp
#+NAME: gtd-capture-keybindings
#+BEGIN_SRC elisp
(defun gtd/setup-capture-keys ()
  "Configure capture keybindings."
  (spacemacs/set-leader-keys
    "o o c i" 'gtd/capture-inbox
    "o o c t" 'gtd/capture-task
    "o o c p" 'gtd/capture-project
    "o o c n" 'gtd/capture-note
    "o o c r" 'gtd/capture-reference
    "o o c j" 'gtd/capture-journal)
  
  ;; Capture menu descriptions
  (which-key-add-key-based-replacements
    "SPC o o c i" "inbox (universal)"
    "SPC o o c t" "task (context-aware)"
    "SPC o o c p" "project"
    "SPC o o c n" "note"
    "SPC o o c r" "reference"
    "SPC o o c j" "journal"))
#+END_SRC
```

### 122. Processing Keybindings

**Processing Workflow**:
```elisp
#+NAME: gtd-processing-keybindings
#+BEGIN_SRC elisp
(defun gtd/setup-processing-keys ()
  "Configure processing keybindings."
  (spacemacs/set-leader-keys
    "o o p i" 'gtd/process-inbox
    "o o p c" 'gtd/clarify-item
    "o o p o" 'gtd/organize-item
    "o o p n" 'gtd/next-inbox-item
    "o o p a" 'gtd/archive-completed)
  
  ;; Processing descriptions
  (which-key-add-key-based-replacements
    "SPC o o p i" "process inbox"
    "SPC o o p c" "clarify current"
    "SPC o o p o" "organize current"
    "SPC o o p n" "next item"
    "SPC o o p a" "archive done"))
#+END_SRC
```

### 123. Review Keybindings

**Review Cycles**:
```elisp
#+NAME: gtd-review-keybindings
#+BEGIN_SRC elisp
(defun gtd/setup-review-keys ()
  "Configure review keybindings."
  (spacemacs/set-leader-keys
    "o o r d" 'gtd/daily-review
    "o o r w" 'gtd/weekly-review
    "o o r m" 'gtd/monthly-review
    "o o r s" 'gtd/review-summary
    "o o r t" 'gtd/review-templates)
  
  ;; Review descriptions
  (which-key-add-key-based-replacements
    "SPC o o r d" "daily review"
    "SPC o o r w" "weekly review"
    "SPC o o r m" "monthly review"
    "SPC o o r s" "review summary"
    "SPC o o r t" "review templates"))
#+END_SRC
```

### 124. Focus View Keybindings

**Context and Focus Views**:
```elisp
#+NAME: gtd-focus-keybindings
#+BEGIN_SRC elisp
(defun gtd/setup-focus-keys ()
  "Configure focus view keybindings."
  (spacemacs/set-leader-keys
    "o o f n" 'gtd/view-next-actions
    "o o f p" 'gtd/view-projects
    "o o f w" 'gtd/view-waiting
    "o o f c" 'gtd/view-contexts
    "o o f e" 'gtd/view-energy-levels
    "o o f t" 'gtd/view-time-blocks)
  
  ;; Focus view descriptions
  (which-key-add-key-based-replacements
    "SPC o o f n" "next actions"
    "SPC o o f p" "projects"
    "SPC o o f w" "waiting for"
    "SPC o o f c" "by context"
    "SPC o o f e" "by energy"
    "SPC o o f t" "by time"))
#+END_SRC
```

### 125. Quick Action Keybindings

**Rapid Operations**:
```elisp
#+NAME: gtd-quick-keybindings
#+BEGIN_SRC elisp
(defun gtd/setup-quick-keys ()
  "Configure quick action keybindings."
  (spacemacs/set-leader-keys
    "o o q t" 'gtd/toggle-context
    "o o q s" 'gtd/schedule-item
    "o o q d" 'gtd/deadline-item
    "o o q m" 'gtd/mark-next-action
    "o o q w" 'gtd/mark-waiting
    "o o q x" 'gtd/mark-done)
  
  ;; Quick action descriptions
  (which-key-add-key-based-replacements
    "SPC o o q t" "toggle context"
    "SPC o o q s" "schedule"
    "SPC o o q d" "deadline"
    "SPC o o q m" "mark next"
    "SPC o o q w" "mark waiting"
    "SPC o o q x" "mark done"))
#+END_SRC
```

### 126. Major Mode Keybindings

**Org-Mode Integration**:
```elisp
#+NAME: gtd-major-mode-keys
#+BEGIN_SRC elisp
(defun gtd/setup-major-mode-keys ()
  "Configure GTD keybindings for org-mode."
  (spacemacs/declare-prefix-for-mode 'org-mode "m g" "gtd")
  
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "g c" 'gtd/add-context-tags
    "g e" 'gtd/set-energy-level  
    "g t" 'gtd/set-time-estimate
    "g s" 'gtd/change-state
    "g p" 'gtd/convert-to-project
    "g l" 'gtd/link-to-project
    "g n" 'gtd/create-next-action)
  
  ;; Major mode descriptions
  (which-key-add-major-mode-key-based-replacements 'org-mode
    "SPC m g c" "add context"
    "SPC m g e" "set energy"
    "SPC m g t" "set time"
    "SPC m g s" "change state"
    "SPC m g p" "→ project"
    "SPC m g l" "link project"
    "SPC m g n" "create next"))
#+END_SRC
```

### 127. Context-Sensitive Keybindings

**Dynamic Bindings**:
```elisp
#+NAME: gtd-context-sensitive-keys
#+BEGIN_SRC elisp
(defun gtd/setup-context-keys ()
  "Set up context-sensitive keybindings."
  ;; Context-specific bindings
  (spacemacs/declare-prefix "o o f c" "contexts")
  
  (spacemacs/set-leader-keys
    "o o f c c" 'gtd/focus-computer
    "o o f c p" 'gtd/focus-phone
    "o o f c h" 'gtd/focus-home
    "o o f c o" 'gtd/focus-office
    "o o f c e" 'gtd/focus-errands
    "o o f c a" 'gtd/focus-anywhere)
  
  ;; Context descriptions
  (which-key-add-key-based-replacements
    "SPC o o f c c" "@computer"
    "SPC o o f c p" "@phone"
    "SPC o o f c h" "@home"
    "SPC o o f c o" "@office"  
    "SPC o o f c e" "@errands"
    "SPC o o f c a" "@anywhere"))

(defun gtd/setup-energy-keys ()
  "Set up energy-based keybindings."
  (spacemacs/declare-prefix "o o f e" "energy")
  
  (spacemacs/set-leader-keys
    "o o f e h" 'gtd/focus-high-energy
    "o o f e m" 'gtd/focus-medium-energy
    "o o f e l" 'gtd/focus-low-energy)
  
  (which-key-add-key-based-replacements
    "SPC o o f e h" "high energy"
    "SPC o o f e m" "medium energy"
    "SPC o o f e l" "low energy"))
#+END_SRC
```

### 128. Development and Debug Keys

**Development Support**:
```elisp
#+NAME: gtd-dev-keybindings
#+BEGIN_SRC elisp
(defun gtd/setup-dev-keys ()
  "Set up development keybindings (temporary)."
  (spacemacs/set-leader-keys
    "o o d t" 'org-babel-tangle
    "o o d e" 'org-babel-execute-buffer
    "o o d c" 'org-babel-remove-result
    "o o d r" 'gtd/reload-system
    "o o d s" 'gtd/system-status
    "o o d l" 'gtd/toggle-logging)
  
  ;; Dev descriptions
  (which-key-add-key-based-replacements
    "SPC o o d t" "tangle"
    "SPC o o d e" "execute all"
    "SPC o o d c" "clear results"
    "SPC o o d r" "reload system"
    "SPC o o d s" "system status"
    "SPC o o d l" "toggle logging"))
#+END_SRC
```

### 129. Global Keybinding Assembly

**Master Setup Function**:
```elisp
#+NAME: gtd-keybindings-init
#+BEGIN_SRC elisp
(defun gtd/initialize-keybindings ()
  "Initialize all GTD keybindings."
  (gtd/setup-keybindings)
  (gtd/setup-capture-keys)
  (gtd/setup-processing-keys)
  (gtd/setup-review-keys)
  (gtd/setup-focus-keys)
  (gtd/setup-quick-keys)
  (gtd/setup-major-mode-keys)
  (gtd/setup-context-keys)
  (gtd/setup-energy-keys)
  (gtd/setup-dev-keys)
  
  (gtd/log "All GTD keybindings initialized")
  (message "GTD keybindings ready! Use SPC o o to start."))

;; Integration with spacemacs
(with-eval-after-load 'spacemacs-keys
  (gtd/initialize-keybindings))
#+END_SRC
```

## Implementation Tasks

1. [ ] Set up base keybinding infrastructure
2. [ ] Implement capture keybindings hierarchy
3. [ ] Create processing workflow bindings
4. [ ] Configure review cycle bindings
5. [ ] Set up focus view bindings
6. [ ] Create quick action bindings
7. [ ] Implement major mode integration
8. [ ] Add context-sensitive bindings
9. [ ] Set up development bindings
10. [ ] Test all keybinding categories
11. [ ] Verify which-key descriptions

## Testing Checklist

- [ ] `SPC o o` shows main GTD menu
- [ ] All submenu prefixes display correctly
- [ ] Which-key descriptions are helpful
- [ ] Major mode bindings work in org files
- [ ] Context-specific bindings function
- [ ] Development bindings aid workflow
- [ ] No keybinding conflicts detected
- [ ] All functions are properly bound

## Success Criteria

1. Complete hierarchical keybinding system
2. Intuitive mnemonic organization
3. Helpful which-key descriptions
4. No conflicts with existing bindings
5. Fast access to common operations

---

*This component creates an ergonomic and discoverable keybinding system for the GTD workflow.*