# Spacemacs Keybinding System Research

## Executive Summary

This document provides comprehensive research on the Spacemacs keybinding system architecture, specifically for implementing a GTD (Getting Things Done) system in org-mode. The research covers the core architecture, implementation patterns, and best practices for creating elegant, discoverable keybinding hierarchies.

## Table of Contents

1. [Spacemacs Keybinding Architecture](#spacemacs-keybinding-architecture)
2. [Evil-Leader and Which-Key Integration](#evil-leader-and-which-key-integration)
3. [Org-Mode Specific Keybindings](#org-mode-specific-keybindings)
4. [Custom Keybinding Implementation](#custom-keybinding-implementation)
5. [Which-Key Menu Organization](#which-key-menu-organization)
6. [GTD-Specific Patterns](#gtd-specific-patterns)
7. [Implementation Examples](#implementation-examples)
8. [Best Practices](#best-practices)

## Spacemacs Keybinding Architecture

### Core Leader Key System

Spacemacs uses a leader key system with the following hierarchy:

- **Primary Leader Key**: `SPC` (Space) - controls all global commands
- **Major Mode Leader Key**: `,` (comma) - shortcut for `SPC m` where major-mode specific commands are bound
- **Reserved User Space**: `SPC o` - guaranteed to be available for user customization

### Architecture Components

1. **Leader Key Variables**:
   ```elisp
   ;; Primary leader key (default: SPC)
   dotspacemacs-leader-key "SPC"
   
   ;; Major mode leader key (default: ,)
   dotspacemacs-major-mode-leader-key ","
   ```

2. **Key Binding Hierarchy**:
   - `SPC` → Global commands
   - `SPC m` or `,` → Major mode commands
   - `SPC o` → User customization space
   - `SPC m o` → Major mode user customization

### Major Mode Leader Key Mechanics

The major mode leader key (`,`) is a shortcut that automatically maps to `SPC m` for the current major mode. This is implemented through:

- A hook that activates on every major mode change
- Local binding of comma key to the submap of the main leader map bound to `m`
- Uses `emacs-bind-map` rather than `evil-leader` directly

## Evil-Leader and Which-Key Integration

### Evolution of Leader Key Systems

Spacemacs has evolved through several leader key implementations:

1. **evil-leader** (legacy): Original Vim-like leader key implementation
2. **bind-map** (current): More sophisticated key binding system
3. **general.el** (modern alternative): Advanced key binding system

### Which-Key Integration

Which-key provides visual feedback for available key bindings:

```elisp
;; Which-key configuration
(setq dotspacemacs-which-key-delay 0.4)  ; Delay in seconds before showing help
```

Features:
- Automatic display of available key bindings
- Hierarchical menu organization
- Mnemonic prefix descriptions
- Context-sensitive help

### Technical Implementation

```elisp
;; Basic which-key integration pattern
(spacemacs/declare-prefix "prefix" "description")
(spacemacs/set-leader-keys "key" 'command)
```

The `:which-key` keyword in configuration provides descriptions:
```elisp
(general-define-key
  :prefix "SPC"
  "t" '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme"))
```

## Org-Mode Specific Keybindings

### Spacemacs Org Layer Structure

The Spacemacs org layer follows this file structure:
```
org/
├── config.el          # Layer configuration
├── funcs.el           # Helper functions
├── packages.el        # Package definitions and configuration
├── keybindings.el     # Key binding definitions
├── README.org         # Documentation
└── local/             # Local packages
```

### Standard Org-Mode Keybindings

Key prefixes in Spacemacs org-mode:

- `SPC a o` → Org agenda commands
- `SPC m` or `,` → Org major mode commands
- `SPC m d` → Deadline setting
- `SPC m s` → Schedule setting
- `SPC m t` → TODO state cycling
- `SPC m A` → Archive item
- `SPC m R` → Refile item

### Org Layer Keybinding Categories

The org layer organizes keybindings into logical categories:

1. **Tree manipulation** (`SPC m h`): Heading operations
2. **Element insertion** (`SPC m i`): Insert various elements
3. **Links** (`SPC m l`): Link management
4. **Emphasis** (`SPC m x`): Text formatting
5. **Tagging** (`SPC m :`): Tag operations
6. **Tables** (`SPC m t`): Table operations
7. **Export** (`SPC m e`): Export functions
8. **Agenda** (`SPC a o`): Agenda operations

## Custom Keybinding Implementation

### Basic Implementation Pattern

```elisp
;; 1. Declare prefix (creates which-key description)
(spacemacs/declare-prefix "prefix" "description")

;; 2. Set leader keys
(spacemacs/set-leader-keys "key" 'command)

;; 3. For major-mode specific bindings
(spacemacs/declare-prefix-for-mode 'mode "prefix" "description")
(spacemacs/set-leader-keys-for-major-mode 'mode "key" 'command)
```

### Global Custom Keybindings

```elisp
;; Using the reserved "o" prefix for customization
(spacemacs/declare-prefix "o" "custom")
(spacemacs/set-leader-keys "oc" 'org-capture)
```

### Major Mode Custom Keybindings

```elisp
;; Org-mode specific custom bindings
(spacemacs/declare-prefix-for-mode 'org-mode "mo" "custom")
(spacemacs/set-leader-keys-for-major-mode 'org-mode "oi" 'org-id-get-create)
```

### Configuration Location

Place custom keybindings in the `dotspacemacs/user-config` function:

```elisp
(defun dotspacemacs/user-config ()
  "Configuration function for user code."
  
  ;; Org-mode specific configurations
  (with-eval-after-load 'org
    ;; Org configuration here
    )
  
  ;; Custom leader key bindings
  (spacemacs/declare-prefix "o" "custom")
  (spacemacs/set-leader-keys "oc" 'org-capture)
  
  ;; Major mode specific bindings
  (spacemacs/declare-prefix-for-mode 'org-mode "mo" "custom")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "oi" 'org-id-get-create))
```

## Which-Key Menu Organization

### Hierarchical Menu Design

Which-key automatically organizes keybindings into discoverable hierarchies:

1. **Mnemonic Prefixes**: Use logical abbreviations
   - `b` → buffer operations
   - `f` → file operations  
   - `p` → project operations
   - `s` → search operations
   - `w` → window operations

2. **Nested Hierarchies**: Create logical groupings
   ```elisp
   ;; First level
   (spacemacs/declare-prefix "og" "gtd")
   
   ;; Second level
   (spacemacs/declare-prefix "ogc" "capture")
   (spacemacs/declare-prefix "oga" "agenda")
   (spacemacs/declare-prefix "ogr" "review")
   ```

### Menu Organization Best Practices

1. **Group Related Functions**: Keep similar operations together
2. **Use Consistent Naming**: Follow established patterns
3. **Limit Depth**: Avoid deeply nested hierarchies (max 3-4 levels)
4. **Provide Descriptions**: Always use meaningful which-key descriptions

### Example Menu Structure

```elisp
;; GTD menu hierarchy
(spacemacs/declare-prefix "og" "gtd")
(spacemacs/declare-prefix "ogc" "capture")
(spacemacs/declare-prefix "oga" "agenda")
(spacemacs/declare-prefix "ogr" "review")
(spacemacs/declare-prefix "ogp" "process")

;; Capture submenu
(spacemacs/set-leader-keys 
  "ogci" 'gtd-capture-inbox
  "ogct" 'gtd-capture-task
  "ogcn" 'gtd-capture-note)

;; Agenda submenu
(spacemacs/set-leader-keys
  "ogad" 'gtd-agenda-daily
  "ogar" 'gtd-agenda-review
  "ogaw" 'gtd-agenda-weekly)
```

## GTD-Specific Patterns

### GTD Workflow Keybinding Categories

Based on GTD methodology, organize keybindings around workflow stages:

1. **Capture** (`SPC o g c`): Collecting inputs
2. **Clarify** (`SPC o g l`): Processing inbox items  
3. **Organize** (`SPC o g o`): Filing and categorizing
4. **Reflect** (`SPC o g r`): Review and planning
5. **Engage** (`SPC o g e`): Doing work

### Common GTD Operations

```elisp
;; GTD Core Operations
(spacemacs/declare-prefix "og" "gtd")

;; Capture
(spacemacs/declare-prefix "ogc" "capture")
(spacemacs/set-leader-keys
  "ogci" 'gtd-capture-inbox
  "ogct" 'gtd-capture-task
  "ogcn" 'gtd-capture-note
  "ogcm" 'gtd-capture-meeting)

;; Process
(spacemacs/declare-prefix "ogp" "process")
(spacemacs/set-leader-keys
  "ogpi" 'gtd-process-inbox
  "ogpr" 'gtd-process-refile
  "ogps" 'gtd-process-schedule)

;; Review
(spacemacs/declare-prefix "ogr" "review")
(spacemacs/set-leader-keys
  "ogrd" 'gtd-review-daily
  "ogrw" 'gtd-review-weekly
  "ogrm" 'gtd-review-monthly)
```

### Context-Based Organization

```elisp
;; Context-based GTD organization
(spacemacs/declare-prefix "ogx" "contexts")
(spacemacs/set-leader-keys
  "ogxc" 'gtd-context-calls
  "ogxe" 'gtd-context-errands
  "ogxh" 'gtd-context-home
  "ogxo" 'gtd-context-office)
```

## Implementation Examples

### Complete GTD Keybinding Implementation

```elisp
(defun dotspacemacs/user-config ()
  "GTD keybinding configuration"
  
  ;; Main GTD prefix
  (spacemacs/declare-prefix "og" "gtd")
  
  ;; Capture system
  (spacemacs/declare-prefix "ogc" "capture")
  (spacemacs/set-leader-keys
    "ogci" 'org-capture-inbox
    "ogct" 'org-capture-task
    "ogcn" 'org-capture-note
    "ogcj" 'org-capture-journal
    "ogcm" 'org-capture-meeting)
  
  ;; Agenda views
  (spacemacs/declare-prefix "oga" "agenda")
  (spacemacs/set-leader-keys
    "ogad" 'gtd-agenda-daily
    "ogaw" 'gtd-agenda-weekly
    "ogaa" 'org-agenda
    "ogat" 'gtd-agenda-tasks
    "ogap" 'gtd-agenda-projects)
  
  ;; Processing
  (spacemacs/declare-prefix "ogp" "process")
  (spacemacs/set-leader-keys
    "ogpi" 'gtd-process-inbox
    "ogpr" 'org-refile
    "ogps" 'org-schedule
    "ogpd" 'org-deadline)
  
  ;; Review system
  (spacemacs/declare-prefix "ogr" "review")
  (spacemacs/set-leader-keys
    "ogrd" 'gtd-daily-review
    "ogrw" 'gtd-weekly-review
    "ogrm" 'gtd-monthly-review
    "ogrs" 'gtd-stuck-projects)
  
  ;; Contexts
  (spacemacs/declare-prefix "ogx" "contexts")
  (spacemacs/set-leader-keys
    "ogxc" 'gtd-context-calls
    "ogxe" 'gtd-context-errands
    "ogxh" 'gtd-context-home
    "ogxo" 'gtd-context-office
    "ogxw" 'gtd-context-waiting)
  
  ;; Major mode specific bindings for org-mode
  (spacemacs/declare-prefix-for-mode 'org-mode "mg" "gtd")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "gn" 'gtd-next-action
    "gs" 'gtd-set-project-status
    "gc" 'gtd-clock-in
    "gC" 'gtd-clock-out
    "gr" 'gtd-refile-quick
    "gt" 'gtd-set-tags))
```

### Modern Alternative with General.el

For comparison, here's how the same system would look using general.el:

```elisp
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer gtd-leader
    :keymaps '(normal insert visual emacs)
    :prefix "SPC o g"
    :global-prefix "C-SPC o g"))

(gtd-leader
  "" '(:ignore t :which-key "gtd")
  "c" '(:ignore t :which-key "capture")
  "ci" '(org-capture-inbox :which-key "inbox")
  "ct" '(org-capture-task :which-key "task")
  "a" '(:ignore t :which-key "agenda")
  "ad" '(gtd-agenda-daily :which-key "daily")
  "aw" '(gtd-agenda-weekly :which-key "weekly"))
```

## Best Practices

### Design Principles

1. **Mnemonic Organization**: Use logical, memorable key combinations
2. **Consistent Hierarchy**: Follow established Spacemacs patterns
3. **Discoverable Structure**: Leverage which-key for exploration
4. **Workflow-Oriented**: Organize around GTD process stages
5. **Context Awareness**: Separate global and mode-specific bindings

### Implementation Guidelines

1. **Use Reserved Prefixes**: Stick to `SPC o` for global customization
2. **Declare Prefixes First**: Always declare prefixes before setting keys
3. **Group Related Functions**: Keep similar operations together
4. **Provide Descriptions**: Use meaningful which-key descriptions
5. **Test Incrementally**: Build keybinding hierarchies gradually

### Common Pitfalls to Avoid

1. **Don't Override System Keys**: Avoid conflicting with Spacemacs defaults
2. **Don't Create Deep Hierarchies**: Limit to 3-4 levels maximum
3. **Don't Skip Prefix Declarations**: Always declare prefixes for which-key
4. **Don't Mix Global and Mode-Specific**: Keep bindings appropriately scoped
5. **Don't Forget Error Handling**: Test keybindings thoroughly

### Configuration File Organization

```elisp
(defun dotspacemacs/user-config ()
  "User configuration function."
  
  ;; Org-mode configuration (defer loading)
  (with-eval-after-load 'org
    ;; Org-specific settings
    (setq org-directory "~/org/")
    (setq org-agenda-files '("~/org/gtd/")))
  
  ;; Global GTD keybindings
  (spacemacs/declare-prefix "og" "gtd")
  ;; ... global bindings
  
  ;; Org-mode specific keybindings
  (spacemacs/declare-prefix-for-mode 'org-mode "mg" "gtd")
  ;; ... mode-specific bindings
  
  ;; Hook-based configuration
  (add-hook 'org-mode-hook 'my-org-gtd-setup))
```

## Integration with Existing Spacemacs Patterns

### Following Spacemacs Conventions

1. **Prefix Consistency**: Use established prefix patterns
   - `a` → applications
   - `b` → buffers
   - `f` → files
   - `g` → git/version control
   - `h` → help
   - `p` → projects
   - `s` → search
   - `t` → toggles
   - `w` → windows

2. **Major Mode Integration**: Extend existing org-mode bindings
   ```elisp
   ;; Extend existing org-mode structure
   (spacemacs/declare-prefix-for-mode 'org-mode "mg" "gtd")
   ;; This adds to the existing SPC m hierarchy
   ```

3. **Which-Key Descriptions**: Follow naming conventions
   - Use lowercase descriptions
   - Keep descriptions concise
   - Use consistent terminology

### Layer Development

For comprehensive GTD systems, consider creating a custom layer:

```
gtd-layer/
├── config.el          # GTD configuration
├── funcs.el           # Helper functions
├── packages.el        # Package definitions
├── keybindings.el     # Key bindings
└── README.org         # Documentation
```

This provides better organization and reusability while following Spacemacs architecture patterns.

## Conclusion

The Spacemacs keybinding system provides a powerful, discoverable framework for implementing GTD workflows. Key success factors include:

1. **Understanding the Architecture**: Leverage leader keys, which-key, and prefix declarations
2. **Following Conventions**: Use established patterns and reserved spaces
3. **Workflow-Oriented Design**: Organize around GTD methodology stages
4. **Incremental Development**: Build and test keybinding hierarchies gradually
5. **Documentation**: Maintain clear descriptions for discoverability

This research provides the foundation for implementing an elegant, Spacemacs-native GTD keybinding system that integrates seamlessly with the existing org-mode layer while providing intuitive, discoverable workflows for productivity.

---

*Research completed: 2025-06-12*  
*Focus: GTD system implementation in Spacemacs org-mode*  
*Next steps: Design and implement custom GTD keybinding hierarchy*