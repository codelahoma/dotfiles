;;; codelahoma-command-palette.el --- Command palette for GTD-Zettelkasten -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Unified command palette providing quick access to all GTD-Zettelkasten commands
;; with fuzzy search, categories, and usage tracking.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)
(require 'codelahoma-ui)
(require 'cl-lib)

;;; Configuration

(defcustom codelahoma-command-palette-max-recent 10
  "Maximum number of recent commands to track."
  :type 'integer
  :group 'codelahoma-gtd)

(defcustom codelahoma-command-palette-show-keys t
  "Show keybindings in command palette."
  :type 'boolean
  :group 'codelahoma-gtd)

(defcustom codelahoma-command-palette-categories
  '("Capture" "Process" "Navigate" "Review" "Knowledge" 
    "Projects" "Analytics" "Quick" "Settings")
  "Categories for organizing commands."
  :type '(repeat string)
  :group 'codelahoma-gtd)

;;; Command Registry

(defvar codelahoma-command-registry nil
  "Registry of all GTD-Zettelkasten commands.")

(defvar codelahoma-command-usage-history nil
  "History of command usage for frequency sorting.")

(defvar codelahoma-command-recent nil
  "Recently used commands.")

(defun codelahoma-command-palette ()
  "Open the command palette."
  (interactive)
  (let* ((commands (codelahoma-command-get-all))
         (formatted (codelahoma-command-format-for-selection commands))
         (choice (completing-read "Command: " formatted nil t)))
    (when choice
      (let* ((cmd-name (codelahoma-command-extract-name choice))
             (command (alist-get cmd-name commands nil nil #'string=)))
        (when command
          (codelahoma-command-record-usage cmd-name)
          (call-interactively (plist-get command :function)))))))

(defun codelahoma-command-register (name &rest props)
  "Register a command with NAME and PROPS.
PROPS should include:
  :function - the function to call
  :category - command category
  :description - brief description
  :keybinding - optional keybinding"
  (let ((existing (assoc name codelahoma-command-registry)))
    (if existing
        (setcdr existing props)
      (push (cons name props) codelahoma-command-registry))))

(defun codelahoma-command-register-all ()
  "Register all GTD-Zettelkasten commands."
  ;; Capture commands
  (codelahoma-command-register 
   "Capture to Inbox"
   :function 'codelahoma-gtd-capture-inbox
   :category "Capture"
   :description "Quick capture to inbox"
   :keybinding "SPC o o c i")
  
  (codelahoma-command-register 
   "Capture Task"
   :function 'codelahoma-gtd-capture-task
   :category "Capture"
   :description "Capture a new task"
   :keybinding "SPC o o c t")
  
  (codelahoma-command-register 
   "Capture Note"
   :function 'org-roam-capture
   :category "Capture"
   :description "Create a new knowledge note"
   :keybinding "SPC o o z c")
  
  (codelahoma-command-register 
   "Capture Meeting"
   :function 'codelahoma-gtd-capture-meeting
   :category "Capture"
   :description "Capture meeting notes"
   :keybinding "SPC o o c m")
  
  ;; Process commands
  (codelahoma-command-register 
   "Process Inbox"
   :function 'codelahoma-gtd-process-inbox
   :category "Process"
   :description "Process inbox items one by one"
   :keybinding "SPC o o p i")
  
  (codelahoma-command-register 
   "Bulk Process"
   :function 'codelahoma-gtd-bulk-process
   :category "Process"
   :description "Process multiple items at once"
   :keybinding "SPC o o p b")
  
  (codelahoma-command-register 
   "Decision Tree"
   :function 'codelahoma-gtd-decision-tree
   :category "Process"
   :description "Use decision tree for processing"
   :keybinding "SPC o o p d")
  
  ;; Navigate commands
  (codelahoma-command-register 
   "Open Dashboard"
   :function 'codelahoma-dashboard
   :category "Navigate"
   :description "Open unified dashboard"
   :keybinding "SPC o o SPC")
  
  (codelahoma-command-register 
   "Open Inbox"
   :function 'codelahoma-gtd-open-inbox
   :category "Navigate"
   :description "Navigate to inbox"
   :keybinding "SPC o o n i")
  
  (codelahoma-command-register 
   "Open Projects"
   :function 'codelahoma-gtd-open-projects
   :category "Navigate"
   :description "Navigate to projects"
   :keybinding "SPC o o n p")
  
  (codelahoma-command-register 
   "Find Knowledge Note"
   :function 'org-roam-node-find
   :category "Navigate"
   :description "Find or create knowledge note"
   :keybinding "SPC o o z n")
  
  ;; Review commands
  (codelahoma-command-register 
   "Daily Review"
   :function 'codelahoma-gtd-daily-review
   :category "Review"
   :description "Perform daily review"
   :keybinding "SPC o o r d")
  
  (codelahoma-command-register 
   "Weekly Review"
   :function 'codelahoma-gtd-weekly-review
   :category "Review"
   :description "Perform weekly review"
   :keybinding "SPC o o r w")
  
  (codelahoma-command-register 
   "Morning Review"
   :function 'codelahoma-gtd-morning-review
   :category "Review"
   :description "Morning planning session"
   :keybinding "SPC o o r M")
  
  ;; Knowledge commands
  (codelahoma-command-register 
   "Link Task to Note"
   :function 'codelahoma-bridge-link-task-to-note
   :category "Knowledge"
   :description "Connect task with knowledge"
   :keybinding "SPC o o i l")
  
  (codelahoma-command-register 
   "Suggest Knowledge"
   :function 'codelahoma-bridge-suggest-related-knowledge
   :category "Knowledge"
   :description "Get smart knowledge suggestions"
   :keybinding "SPC o o S s")
  
  (codelahoma-command-register 
   "Knowledge Dashboard"
   :function 'codelahoma-bridge-knowledge-dashboard
   :category "Knowledge"
   :description "View knowledge metrics"
   :keybinding "SPC o o m d")
  
  ;; Project commands
  (codelahoma-command-register 
   "New Project"
   :function 'codelahoma-gtd-new-project
   :category "Projects"
   :description "Create a new project"
   :keybinding "SPC o o j n")
  
  (codelahoma-command-register 
   "Project Template"
   :function 'codelahoma-gtd-new-project-from-template
   :category "Projects"
   :description "Create project from template"
   :keybinding "SPC o o j t")
  
  (codelahoma-command-register 
   "Project Wiki"
   :function 'codelahoma-bridge-create-project-wiki
   :category "Projects"
   :description "Create project knowledge wiki"
   :keybinding "SPC o o j w")
  
  ;; Quick commands
  (codelahoma-command-register 
   "Quick Navigation"
   :function 'codelahoma-gtd-quick-nav/body
   :category "Quick"
   :description "Quick navigation hydra"
   :keybinding "SPC o o q q")
  
  (codelahoma-command-register 
   "Quick Status"
   :function 'codelahoma-gtd-quick-status
   :category "Quick"
   :description "Show quick status"
   :keybinding "SPC o o q s")
  
  (codelahoma-command-register 
   "Quick Today"
   :function 'codelahoma-gtd-quick-today
   :category "Quick"
   :description "Today's tasks at a glance"
   :keybinding "SPC o o q t")
  
  ;; Settings commands
  (codelahoma-command-register 
   "Reload System"
   :function 'codelahoma-gtd-reload
   :category "Settings"
   :description "Reload all GTD modules"
   :keybinding "SPC o o d r")
  
  (codelahoma-command-register 
   "Enable Auto-Save"
   :function 'codelahoma-gtd-enable-auto-save
   :category "Settings"
   :description "Enable automatic saving"
   :keybinding "SPC o o b e")
  
  (codelahoma-command-register 
   "Help"
   :function 'codelahoma-gtd-help
   :category "Settings"
   :description "Show GTD help"
   :keybinding "SPC o o h"))

;;; Command Retrieval

(defun codelahoma-command-get-all ()
  "Get all registered commands with metadata."
  (let ((commands (copy-alist codelahoma-command-registry)))
    ;; Add dynamic commands
    (setq commands (append commands 
                          (codelahoma-command-get-capture-templates)
                          (codelahoma-command-get-agenda-views)
                          (codelahoma-command-get-recent-files)))
    ;; Sort by usage frequency and recency
    (codelahoma-command-sort-by-relevance commands)))

(defun codelahoma-command-get-capture-templates ()
  "Get capture templates as commands."
  (let ((templates '()))
    (dolist (template org-capture-templates)
      (let ((key (car template))
            (desc (nth 1 template)))
        (push (cons (format "Capture: %s" desc)
                   (list :function (lambda () 
                                    (interactive)
                                    (org-capture nil key))
                         :category "Capture"
                         :description (format "Template: %s" desc)))
              templates)))
    templates))

(defun codelahoma-command-get-agenda-views ()
  "Get custom agenda views as commands."
  (let ((views '()))
    (dolist (view org-agenda-custom-commands)
      (let ((key (car view))
            (desc (nth 1 view)))
        (when (stringp desc)
          (push (cons (format "Agenda: %s" desc)
                     (list :function (lambda ()
                                      (interactive)
                                      (org-agenda nil key))
                           :category "Navigate"
                           :description (format "Agenda view: %s" desc)))
                views))))
    views))

(defun codelahoma-command-get-recent-files ()
  "Get recent GTD files as commands."
  (let ((files '()))
    (dolist (file codelahoma-gtd-files)
      (when (file-exists-p file)
        (push (cons (format "Open: %s" (file-name-nondirectory file))
                   (list :function (lambda ()
                                    (interactive)
                                    (find-file file))
                         :category "Navigate"
                         :description (format "Open %s" file)))
              files)))
    files))

;;; Command Formatting

(defun codelahoma-command-format-for-selection (commands)
  "Format COMMANDS for display in selection."
  (mapcar (lambda (cmd)
            (let* ((name (car cmd))
                   (props (cdr cmd))
                   (category (plist-get props :category))
                   (key (plist-get props :keybinding))
                   (desc (plist-get props :description)))
              (concat
               (when category
                 (propertize (format "[%s] " category) 
                            'face 'font-lock-type-face))
               name
               (when desc
                 (propertize (format " - %s" desc)
                            'face 'font-lock-comment-face))
               (when (and codelahoma-command-palette-show-keys key)
                 (propertize (format " (%s)" key)
                            'face 'font-lock-keyword-face)))))
          commands))

(defun codelahoma-command-extract-name (formatted)
  "Extract command name from FORMATTED string."
  (if (string-match "\\(?:\\[[^]]+\\] \\)?\\([^-]+\\)" formatted)
      (string-trim (match-string 1 formatted))
    formatted))

;;; Sorting and Filtering

(defun codelahoma-command-sort-by-relevance (commands)
  "Sort COMMANDS by relevance (usage frequency and recency)."
  (sort commands
        (lambda (a b)
          (let ((score-a (codelahoma-command-calculate-score (car a)))
                (score-b (codelahoma-command-calculate-score (car b))))
            (> score-a score-b)))))

(defun codelahoma-command-calculate-score (name)
  "Calculate relevance score for command NAME."
  (let ((usage-count (codelahoma-command-get-usage-count name))
        (recency (codelahoma-command-get-recency name)))
    (+ (* usage-count 2) recency)))

(defun codelahoma-command-get-usage-count (name)
  "Get usage count for command NAME."
  (or (alist-get name codelahoma-command-usage-history 0 nil #'string=) 0))

(defun codelahoma-command-get-recency (name)
  "Get recency score for command NAME."
  (let ((pos (cl-position name codelahoma-command-recent :test #'string=)))
    (if pos
        (- codelahoma-command-palette-max-recent pos)
      0)))

;;; Usage Tracking

(defun codelahoma-command-record-usage (name)
  "Record usage of command NAME."
  ;; Update usage count
  (let ((count (codelahoma-command-get-usage-count name)))
    (setf (alist-get name codelahoma-command-usage-history nil nil #'string=)
          (1+ count)))
  ;; Update recent list
  (setq codelahoma-command-recent
        (cons name (delete name codelahoma-command-recent)))
  (when (> (length codelahoma-command-recent) codelahoma-command-palette-max-recent)
    (setq codelahoma-command-recent
          (seq-take codelahoma-command-recent codelahoma-command-palette-max-recent)))
  ;; Save history
  (codelahoma-command-save-history))

(defun codelahoma-command-save-history ()
  "Save command history to file."
  (let ((file (expand-file-name "command-history.el" codelahoma-gtd-directory)))
    (with-temp-file file
      (print `(setq codelahoma-command-usage-history ',codelahoma-command-usage-history
                   codelahoma-command-recent ',codelahoma-command-recent)
             (current-buffer)))))

(defun codelahoma-command-load-history ()
  "Load command history from file."
  (let ((file (expand-file-name "command-history.el" codelahoma-gtd-directory)))
    (when (file-exists-p file)
      (load file nil t))))

;;; Category Browsing

(defun codelahoma-command-by-category ()
  "Browse commands by category."
  (interactive)
  (let* ((category (completing-read "Category: " 
                                   codelahoma-command-palette-categories))
         (commands (codelahoma-command-get-by-category category))
         (formatted (codelahoma-command-format-for-selection commands))
         (choice (completing-read (format "%s command: " category) 
                                 formatted nil t)))
    (when choice
      (let* ((cmd-name (codelahoma-command-extract-name choice))
             (command (alist-get cmd-name commands nil nil #'string=)))
        (when command
          (codelahoma-command-record-usage cmd-name)
          (call-interactively (plist-get command :function)))))))

(defun codelahoma-command-get-by-category (category)
  "Get all commands in CATEGORY."
  (let ((commands '()))
    (dolist (cmd codelahoma-command-registry)
      (when (string= (plist-get (cdr cmd) :category) category)
        (push cmd commands)))
    (nreverse commands)))

;;; Fuzzy Search

(defun codelahoma-command-fuzzy-search ()
  "Fuzzy search through commands."
  (interactive)
  (require 'ivy nil t)
  (if (fboundp 'ivy-read)
      (let* ((commands (codelahoma-command-get-all))
             (formatted (codelahoma-command-format-for-selection commands))
             (ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
        (ivy-read "Command (fuzzy): " formatted
                  :action (lambda (x)
                           (let* ((cmd-name (codelahoma-command-extract-name x))
                                  (command (alist-get cmd-name commands nil nil #'string=)))
                             (when command
                               (codelahoma-command-record-usage cmd-name)
                               (call-interactively (plist-get command :function)))))
                  :caller 'codelahoma-command-fuzzy-search))
    (message "Ivy not available. Using standard completion.")
    (codelahoma-command-palette)))

;;; Recent Commands

(defun codelahoma-command-recent ()
  "Show and execute recent commands."
  (interactive)
  (if codelahoma-command-recent
      (let* ((commands (codelahoma-command-get-all))
             (recent-cmds (cl-remove-if-not 
                          (lambda (cmd) 
                            (member (car cmd) codelahoma-command-recent))
                          commands))
             (formatted (codelahoma-command-format-for-selection recent-cmds))
             (choice (completing-read "Recent command: " formatted nil t)))
        (when choice
          (let* ((cmd-name (codelahoma-command-extract-name choice))
                 (command (alist-get cmd-name commands nil nil #'string=)))
            (when command
              (codelahoma-command-record-usage cmd-name)
              (call-interactively (plist-get command :function))))))
    (message "No recent commands. Use the main command palette first.")))

;;; Help System

(defun codelahoma-command-help ()
  "Show help for command palette."
  (interactive)
  (with-output-to-temp-buffer "*Command Palette Help*"
    (princ "GTD-Zettelkasten Command Palette\n")
    (princ "================================\n\n")
    (princ "Access Methods:\n")
    (princ "  M-x codelahoma-command-palette    - Main palette\n")
    (princ "  M-x codelahoma-command-fuzzy-search - Fuzzy search\n")
    (princ "  M-x codelahoma-command-by-category  - Browse by category\n")
    (princ "  M-x codelahoma-command-recent      - Recent commands\n\n")
    (princ "Categories:\n")
    (dolist (cat codelahoma-command-palette-categories)
      (princ (format "  • %s\n" cat)))
    (princ "\nTips:\n")
    (princ "  - Commands are sorted by usage frequency\n")
    (princ "  - Recently used commands appear first\n")
    (princ "  - Keybindings shown when available\n")))

;;; Initialize

(defun codelahoma-command-palette-initialize ()
  "Initialize the command palette."
  (codelahoma-command-register-all)
  (codelahoma-command-load-history))

;; Load history on startup
(add-hook 'after-init-hook #'codelahoma-command-palette-initialize)

(provide 'codelahoma-command-palette)
;;; codelahoma-command-palette.el ends here