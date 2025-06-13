---
title: Elisp Fundamentals for Org-Mode Customization
type: note
permalink: research/elisp-fundamentals-for-org-mode-customization
tags:
- '#elisp'
- '#org-mode'
- '#emacs'
- '#programming'
- '#research'
---

# Elisp Fundamentals for Org-Mode Customization

## Core Elisp Concepts

### 1. Variables and Assignment

#### setq - Set Quoted
```elisp
;; Basic assignment
(setq variable-name value)

;; Multiple assignments
(setq a 10
      b 20
      c "Emacs")

;; Global variable
(setq org-default-notes-file "~/org/notes.org")
```

#### setq-local - Buffer-Local Variables
```elisp
;; Sets variable only in current buffer
(setq-local indent-line-function 'org-indent-line)
(setq-local indent-region-function 'org-indent-region)
```

#### let - Local Bindings
```elisp
;; Temporary variable bindings
(let ((x 1)
      (y 10))
  (+ (* 4 x) (* 5 y)))  ; Returns 54

;; Nested let bindings
(let ((name "Task"))
  (let ((full-name (concat "TODO " name)))
    full-name))  ; Returns "TODO Task"
```

### 2. Functions

#### defun - Define Function
```elisp
;; Basic function
(defun my-function (arg1 arg2)
  "Documentation string"
  (+ arg1 arg2))

;; Function with optional arguments
(defun my-func (required &optional opt1 opt2)
  (list required opt1 opt2))
```

#### interactive - User-Callable Functions
```elisp
;; Simple interactive function
(defun my-command ()
  "Description for M-x"
  (interactive)
  (message "Hello!"))

;; Interactive with arguments
(defun my-prompt-command (name)
  "Prompts for NAME."
  (interactive "sEnter name: ")
  (message "Hello, %s!" name))

;; Common interactive codes:
;; "s" - String prompt
;; "n" - Number prompt
;; "r" - Region (start and end)
;; "p" - Prefix argument
```

### 3. Hooks - Customization Points

```elisp
;; Add function to hook
(add-hook 'org-mode-hook 'my-org-setup-function)

;; Add lambda to hook
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local line-spacing 0.2)
            (variable-pitch-mode 1)))

;; Remove from hook
(remove-hook 'org-mode-hook 'my-function)

;; Common org-mode hooks:
;; - org-mode-hook (when org-mode starts)
;; - org-capture-mode-hook (in capture buffer)
;; - org-agenda-mode-hook (in agenda buffer)
;; - org-archive-hook (after archiving)
```

### 4. Control Structures

#### if/when/unless
```elisp
;; if - with else clause
(if (> x 5)
    (message "Greater than 5")
  (message "Less than or equal to 5"))

;; when - no else clause
(when (org-at-heading-p)
  (org-set-property "CREATED" (format-time-string "%Y-%m-%d")))

;; unless - negative condition
(unless (file-exists-p "~/org/")
  (make-directory "~/org/"))
```

#### cond - Multiple Conditions
```elisp
(cond
 ((eq major-mode 'org-mode)
  (message "In org-mode"))
 ((eq major-mode 'emacs-lisp-mode)
  (message "In elisp-mode"))
 (t
  (message "In some other mode")))
```

### 5. Lists and Property Lists

```elisp
;; Creating lists
(setq my-list '(a b c d))
(setq my-list (list 1 2 3 4))

;; Property lists (plists)
(setq my-plist '(:name "John" :age 30 :city "NYC"))

;; Accessing plist values
(plist-get my-plist :name)  ; Returns "John"
(plist-put my-plist :age 31)  ; Updates age
```

### 6. Working with Buffers

```elisp
;; Current buffer operations
(buffer-name)  ; Get current buffer name
(buffer-file-name)  ; Get file path

;; Switch buffers temporarily
(with-current-buffer "buffer-name"
  ;; Code executes in specified buffer
  (point-max))

;; Save excursion (preserve point/mark)
(save-excursion
  (goto-char (point-min))
  (search-forward "TODO"))
```

## Org-Mode Specific Patterns

### 1. Org Element Access
```elisp
;; Check if at heading
(org-at-heading-p)

;; Get heading text
(org-get-heading t t t t)

;; Get/set properties
(org-entry-get nil "PROPERTY-NAME")
(org-set-property "PROPERTY-NAME" "value")
```

### 2. Custom Link Types
```elisp
(org-link-set-parameters "mylink"
  :follow (lambda (path) (browse-url (concat "https://example.com/" path)))
  :export (lambda (path desc format)
            (cond
             ((eq format 'html) (format "<a href='https://example.com/%s'>%s</a>" path desc))
             (t path))))
```

### 3. Capture Template Functions
```elisp
;; Function returning template
(defun my-capture-template ()
  (concat "* TODO " 
          (read-string "Task: ")
          "\n  SCHEDULED: %t\n  %?"))

;; Use in capture templates
(setq org-capture-templates
      `(("d" "Dynamic" entry (file "~/org/tasks.org")
         (function my-capture-template))))
```

### 4. Agenda Custom Commands
```elisp
(setq org-agenda-custom-commands
      '(("h" "High Energy Tasks"
         ((tags-todo "+ENERGY=\"high\""))
         ((org-agenda-overriding-header "High Energy Tasks")))))
```

## Best Practices

1. **Use descriptive names**: Prefix custom functions with your initials or project name
2. **Add docstrings**: Document what your functions do
3. **Handle errors gracefully**: Use `condition-case` for error handling
4. **Test interactively**: Use `M-x ielm` for interactive Elisp testing
5. **Avoid global state**: Prefer `let` bindings over `setq` when possible
6. **Use hooks wisely**: Don't add slow operations to frequently-run hooks

## Common Pitfalls

1. **Quote vs Backquote**: Remember `'` prevents all evaluation, `` ` `` allows selective evaluation
2. **Hook naming**: Hook variables end with `-hook`, functions added to hooks don't
3. **Buffer locality**: Some variables are buffer-local, changes don't affect other buffers
4. **Load order**: Ensure required features are loaded before customization
5. **Side effects**: Many org functions move point, use `save-excursion`

## Debugging Tips

```elisp
;; Enable debugging
(setq debug-on-error t)

;; Trace function calls
(trace-function 'my-function)

;; Print debugging info
(message "Debug: variable = %s" my-variable)

;; Use edebug for step-through debugging
;; Place cursor on defun and press C-u C-M-x
```