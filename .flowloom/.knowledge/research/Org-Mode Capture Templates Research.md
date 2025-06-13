---
title: Org-Mode Capture Templates Research
type: note
permalink: research/org-mode-capture-templates-research
tags:
- '#org-mode'
- '#gtd'
- '#capture-templates'
- '#emacs'
- '#research'
---

# Org-Mode Capture Templates Research

## Basic Template Structure

```elisp
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))
```

## Key Template Expansion Elements

### Basic Expansions
- `%?` - Position cursor here after template expansion
- `%i` - Initial content from the active region
- `%a` - Link to the location from where capture was called
- `%U` - Inactive timestamp
- `%t` - Timestamp, date only
- `%T` - Timestamp, with date and time
- `%u` - Inactive timestamp, date only

### Interactive Prompts
- `%^{prompt}` - Prompt for a string
- `%^{prompt|default}` - Prompt with default value
- `%^{prompt|choice1|choice2|choice3}` - Prompt with choices
- `%^{Birthday}t` - Prompt for a date
- `%\N` - Insert text from Nth %^{PROMPT} (N starts at 1)

### Dynamic Content
- `%(elisp-expression)` - Evaluate elisp and insert result (must return string)
- `%:keyword` - Access properties from org-store-link-plist
- `%[file]` - Insert contents of file

## Advanced Template Examples

### Dynamic Template with Clock Tracking
```elisp
(setq org-capture-templates
  '(("t" "todo" entry (file (concat org-directory "/gtd.org"))
     "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
    ("n" "note" entry (file (concat org-directory "/gtd.org"))
     "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)))
```

### Custom Function Integration
```elisp
(defun v-i-or-nothing ()
  (let ((v-i (plist-get org-store-link-plist :initial)))
    (if (equal v-i "") "" (concat v-i "\n"))))

("w" "capture" entry (file "~/refile.org")
 "* [[%:link][%:description]] :NOTE:\n%(v-i-or-nothing)%U\n"
 :immediate-finish t)
```

### Energy-Based Template (Conceptual)
```elisp
("e" "Energy Task" entry (file+headline "~/org/gtd.org" "Tasks")
 "* TODO %^{Task} :@%^{Context|work|home|errands}:
  :PROPERTIES:
  :ENERGY: %^{Energy Level|high|medium|low}
  :EFFORT: %^{Effort Estimate|5m|15m|30m|1h|2h}
  :END:
  %?
  Created: %U")
```

## GTD Best Practices

### File Structure
- `inbox.org` - Quick capture dumping ground
- `gtd.org` - Main file for active projects
- `work.org` / `home.org` - Context-specific files
- `journal.org` - Daily notes and diary
- `someday.org` - Someday/maybe items
- `archive.org` - Completed/cancelled items

### Context Tags
- `@home`
- `@office`
- `@phone`
- `@email`
- `@errands`
- `@computer`

### Task States
- `TODO` - Task to be done
- `NEXT` - Next action (active)
- `WAITING` - Waiting on someone else
- `DONE` - Completed
- `CANCELLED` - No longer needed

## Template Design Considerations

1. **Speed**: Templates should be fast to invoke and fill out
2. **Context**: Capture relevant context automatically (location, time, link)
3. **Flexibility**: Allow for quick dumps and detailed entries
4. **Refiling**: Design with easy refiling in mind
5. **Properties**: Use properties for metadata that supports filtering/sorting

## Advanced Features to Explore

1. **Function Templates**: Use `(function FUNCTION-NAME)` for dynamic template generation
2. **Template Selection**: Use `%^{prompt}` to dynamically select sub-templates
3. **Context Access**: Use `(org-capture-get :original-buffer)` to access capture context
4. **Conditional Logic**: Use elisp functions to conditionally include content
5. **Auto-tagging**: Based on time of day, location, or other context

## Function Quoting in Templates

### Backquote Syntax
When including elisp functions in templates, use backquote (`` ` ``) with comma (`,`) for evaluation:

```elisp
(setq org-capture-templates
  `(("p" "Plan" entry 
     (file+function "~/org/todo.org" 
                    ,(my-org-goto-last-headline "\\*\\* Plan"))
     "* TODO %i%?")))
```

### Expansion Order (Critical!)
1. `%[file]` - File contents inserted first
2. `%(elisp)` - Elisp expressions evaluated second
3. `%a`, `%i`, etc. - Simple expansions last

**Important**: This means `%(my-function "%a")` will pass the literal string "%a" to the function, NOT the expanded link!

### Correct Ways to Access Context
```elisp
;; Access original buffer
(buffer-file-name (org-capture-get :original-buffer))

;; Access stored link properties
(plist-get org-store-link-plist :description)
```

### Quote vs Backquote
- `'(...)` - Quote: prevents ALL evaluation
- `` `(...) `` - Backquote: allows selective evaluation with `,`
- `,expression` - Evaluate expression within backquoted list
- `,@list` - Splice list contents into backquoted list

## Resources
- [Org Manual - Capture Templates](https://orgmode.org/manual/Capture-templates.html)
- [Template Expansion Reference](https://orgmode.org/manual/Template-expansion.html)
- [Org GTD Worg Page](https://orgmode.org/worg/org-gtd-etc.html)
- [Elisp Backquote Reference](http://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html)