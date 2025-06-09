# Org-GTD Implementation Plan

## Overview
This plan bridges the gap between the current implementation and the unified Org-Roam + GTD system design, incorporating the enhancements from the improvements document.

## Phase 1: Core GTD Infrastructure (Week 1)

### 1.1 File Structure Setup
- [ ] Create GTD directory structure under `rk/org-directory` (currently `~/personal/org-files/`)
  ```
  ~/personal/org-files/
  ├── inbox.org
  ├── work/
  │   ├── projects.org
  │   ├── gtd.org
  │   └── someday.org
  ├── personal/
  │   ├── projects.org
  │   ├── gtd.org
  │   └── someday.org
  └── archive.org
  ```
- [ ] Add directory creation function to `codelahoma-org.org` using `rk/org-file` helper
- [ ] Ensure all file references use `(rk/org-file "filename")` for portability

### 1.2 TODO Keywords and Tags
- [ ] Add TODO keyword configuration:
  ```elisp
  (setq org-todo-keywords
        '((sequence "NEXT(n)" "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  ```
- [ ] Add comprehensive tag alist with contexts and energy levels:
  ```elisp
  (setq org-tag-alist '(("@work" . ?w)
                        ("@personal" . ?p)
                        ("@home" . ?h)
                        ("@office" . ?o)
                        ("@phone" . ?c)
                        ("@computer" . ?m)
                        ("@errands" . ?e)
                        ("@high_energy" . ?H)
                        ("@low_energy" . ?L)
                        ("@creative" . ?C)
                        ("@routine" . ?R)
                        ("@5min" . ?5)
                        ("@15min" . ?1)
                        ("@30min" . ?3)
                        ("@1hr" . ?6)
                        ("@deep" . ?D)))
  ```
- [ ] Add visual TODO keyword faces

### 1.3 Archive Configuration
- [ ] Set up archive location and behavior using `rk/org-file`:
  ```elisp
  (setq org-archive-location (concat (rk/org-file "archive.org") "::* From %s"))
  ```

## Phase 2: Capture Templates and Agenda Views (Week 1-2)

### 2.1 Enhanced Capture Templates
- [ ] Implement all basic capture templates from the design
- [ ] Add enhanced templates from improvements document:
  - Work project with metadata
  - Quick capture with energy context
  - Meeting with automatic roam note
  - Voice note capture
  - Email task capture
  - Interruption log
- [ ] Ensure capture functions in `codelahoma-org.org` work with new templates

### 2.2 Custom Agenda Commands
- [ ] Implement three core dashboards (work, personal, unified)
- [ ] Add energy-based views
- [ ] Add focus mode (top 3 actions)
- [ ] Add project overview
- [ ] Add stalled items view
- [ ] Configure agenda files based on context

### 2.3 Refile Configuration
- [ ] Set up intelligent refile targets
- [ ] Add quick refile functions (to clock, to projects)
- [ ] Configure refile behavior

## Phase 3: Context Switching and Keybindings (Week 2)

### 3.1 Context Functions
- [ ] Implement `org-work-mode`, `org-personal-mode`, `org-unified-mode`
- [ ] Update functions to use `rk/org-file` for agenda file paths:
  ```elisp
  (setq org-agenda-files (list (rk/org-file "work/") 
                               (rk/org-file "inbox.org")))
  ```
- [ ] Add helper functions for mode indication
- [ ] Test context switching with agenda files

### 3.2 Complete Keybinding Setup
- [ ] Add all core GTD keybindings under `SPC o o`
- [ ] Integrate with existing extension keybindings
- [ ] Add which-key descriptions for all bindings
- [ ] Create keybinding cheatsheet

## Phase 4: Roam-GTD Integration (Week 3)

### 4.1 Bidirectional Linking
- [ ] Implement `org-roam-create-task-from-node`
- [ ] Add `org-agenda-jump-to-roam-node`
- [ ] Create `org-roam-process-actionable-notes`
- [ ] Add roam backlinks in agenda view

### 4.2 Enhanced Project Templates
- [ ] Add ROAM_REFS property to project templates
- [ ] Create project-roam linking functions
- [ ] Add roam note creation for new projects

## Phase 5: Advanced Features (Week 3-4)

### 5.1 Review System
- [ ] Implement comprehensive weekly review function
- [ ] Add project health checks (stalled projects)
- [ ] Add waiting items review
- [ ] Create review templates

### 5.2 Time Tracking
- [ ] Configure org-clock settings
- [ ] Add pomodoro integration
- [ ] Create daily time report function
- [ ] Add clock-in helpers

### 5.3 Batch Processing
- [ ] Implement interactive inbox processing
- [ ] Add bulk tag operations
- [ ] Create bulk scheduling functions

### 5.4 Advanced Enhancements
- [ ] Mobile capture integration
- [ ] Email integration improvements
- [ ] Advanced filtering and search

## Implementation Strategy

### Testing Approach
1. Create test org files for each context
2. Test each feature in isolation
3. Test context switching thoroughly
4. Validate capture templates
5. Ensure backward compatibility

### Migration Path
1. Backup current org files in `~/personal/org-files/`
2. Create new subdirectory structure within existing `rk/org-directory`
3. Reorganize files into work/personal subdirectories
4. All functions already use `rk/org-file` helper - no path updates needed
5. Test all functions with new structure

### Configuration Management
1. Keep all changes in `codelahoma-org.org`
2. Maintain backward compatibility where possible
3. Document all new functions
4. Update keybinding documentation

## Success Criteria
- [ ] All capture templates work correctly
- [ ] Context switching changes agenda views properly
- [ ] Roam-GTD integration is seamless
- [ ] Weekly review process is smooth
- [ ] Time tracking works across contexts
- [ ] All keybindings are intuitive
- [ ] Migration preserves all existing data

## Timeline
- **Week 1**: Core infrastructure and basic capture/agenda
- **Week 2**: Context switching and keybindings
- **Week 3**: Roam integration and review system
- **Week 4**: Advanced features and polish

## Notes
- Preserve all visual customizations (colors, bullets)
- Maintain existing Claude AI integration
- Keep helper functions working
- Document all changes thoroughly
- Create user guide for new features