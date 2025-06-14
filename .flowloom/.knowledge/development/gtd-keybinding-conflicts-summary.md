# GTD Keybinding Conflicts and Action Items

## Critical Conflicts Requiring Immediate Resolution

### 1. oocp - Capture Binding Conflict
**Current State**: 
- Planned: `rk/context-capture-project` (context-aware project capture)
- Implemented: `rk/capture-personal-task` (personal task capture)

**Impact**: This is a major inconsistency - the key suggests "capture project" but it captures a personal task instead.

**Recommended Fix**:
```elisp
;; Change oocp to be context-aware project capture
"oocp" 'rk/context-capture-project  ; Currently points to personal task

;; Move personal task capture to a different binding
"oocP" 'rk/capture-personal-task    ; Capital P for Personal task
```

### 2. oogp - Navigation Binding Conflict  
**Current State**:
- Planned: `rk/goto-projects` (context-aware projects file)
- Implemented: Direct navigation to personal GTD file

**Impact**: Breaks the context-aware navigation pattern.

**Recommended Fix**:
```elisp
;; Remove the incorrect binding
;; "oogp" (lambda () (interactive) (find-file (rk/org-file "gtd.org" "personal")))

;; The correct context-aware projects navigation is already at oogj
;; Consider swapping them:
"oogp" 'rk/org-goto-context-projects  ; Move from oogj
"oogj" nil  ; Remove or repurpose
```

### 3. ooae - Energy Agenda Implementation Difference
**Current State**:
- Planned: `rk/org-energy-agenda-hydra` (hydra for energy-based views)
- Implemented: Direct high-energy agenda view

**Impact**: Minor - functionality exists but without the hydra interface.

**Recommended Fix**:
```elisp
;; Either implement the hydra or document the simpler approach
;; Current implementation is actually cleaner for single view
```

## Missing Essential Bindings

### Must Add:
1. **ooc** - Standard `org-capture` (users expect this)
2. **oor** - Standard `org-refile` (currently at oord)
3. **oorc** - `rk/refile-to-current-clock` (useful feature)

### Should Consider:
1. **oocW** - Work project capture template
2. **oocP** - Personal project capture template  
3. **oomm** - Mode switching hydra (exists at oomh)

## Naming Inconsistencies

### Function Prefix Issues:
- Some functions use `rk/org-*` pattern
- Others use `rk/*` pattern
- Context functions are mixed:
  - `rk/org-context-aware-capture`
  - `rk/org-work-mode`
  - `rk/capture-inbox` (no org prefix)

### Recommendation:
Standardize on `rk/org-*` for all org-mode related functions:
- `rk/capture-inbox` → `rk/org-capture-inbox`
- `rk/capture-work-task` → `rk/org-capture-work-task`
- etc.

## Duplicate/Redundant Bindings

1. **ooa** and **ooaa** - Both bound to `org-agenda`
2. **Archive operations** appear in multiple places:
   - ooRa - Archive done tasks (Review)
   - ooAd - Archive done tasks (Archive)
   - oorv - Archive subtree (should be in Archive section)

## Quick Fixes Script

Here's a script to fix the most critical issues:

```elisp
;; Fix capture conflict
(spacemacs/set-leader-keys
  "oocp" 'rk/org-context-aware-project-capture  ; Fix: was personal task
  "oocP" 'rk/capture-personal-task              ; New: moved from oocp
  "ooc"  'org-capture)                           ; Add: standard capture

;; Fix navigation conflict  
(spacemacs/set-leader-keys
  "oogp" 'rk/org-goto-context-projects          ; Fix: was personal gtd
  "oogj" nil)                                   ; Remove duplicate

;; Fix refile operations
(spacemacs/set-leader-keys
  "oor"  'org-refile                            ; Add: standard refile
  "oord" 'rk/smart-refile                       ; Change: make it smart
  "oorc" 'rk/refile-to-current-clock)          ; Add: missing function

;; Remove duplicates
(spacemacs/set-leader-keys
  "ooaa" nil)                                   ; Remove: duplicate of ooa
```

## Priority Order for Fixes

1. **URGENT**: Fix oocp conflict - it's misleading users
2. **HIGH**: Add missing ooc (org-capture) - standard expectation
3. **HIGH**: Fix oogp to use context-aware function
4. **MEDIUM**: Standardize function naming convention
5. **LOW**: Clean up duplicates and redundancies
6. **LOW**: Consider implementing missing hydras

## Testing After Changes

```elisp
;; Test context capture
(when (fboundp 'rk/org-context-aware-project-capture)
  (message "Testing context project capture..."))

;; Test navigation
(when (fboundp 'rk/org-goto-context-projects)
  (message "Testing context project navigation..."))

;; Verify no conflicts
(let ((bindings '()))
  (mapatoms (lambda (sym)
              (when (and (boundp sym)
                         (string-match "^spacemacs/leader-keys" (symbol-name sym)))
                (push (cons sym (symbol-value sym)) bindings))))
  (message "Total leader key bindings: %d" (length bindings)))
```