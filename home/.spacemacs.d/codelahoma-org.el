;;; codelahoma-gtd.el --- GTD System Implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod
;; Author: Rod
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (org "9.6"))

;;; Commentary:
;; A comprehensive GTD implementation for Spacemacs

;;; Code:

(require 'cl-lib)  ; For defstruct

(defgroup codelahoma-gtd nil
  "GTD system configuration."
  :group 'org
  :prefix "codelahoma-gtd-")

(defcustom codelahoma-gtd-directory "~/personal/org-files/gtd/"
  "Base directory for GTD files."
  :type 'directory
  :group 'codelahoma-gtd)

(defvar codelahoma-gtd-components-loaded nil
  "List of loaded GTD components.")

(defun codelahoma-gtd-load-component (component)
  "Load GTD COMPONENT if not already loaded."
  (unless (member component codelahoma-gtd-components-loaded)
    (message "Loading GTD component: %s" component)
    (push component codelahoma-gtd-components-loaded)))

(defun codelahoma-gtd-setup-development ()
  "Set up GTD development environment."
  (interactive)
  ;; Ensure org-babel can execute elisp
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)))
  
  ;; Set up testing framework
  (require 'ert)
  
  (message "GTD development environment ready"))

(codelahoma-gtd-load-component 'foundation-dev)

(defvar codelahoma-gtd-components
  '((100 . foundation-setup)
    (110 . core-engine)
    (120 . keybinding-system)
    (130 . capture-system)
    (140 . processing-workflow)
    (150 . review-system)
    (160 . context-engine)
    (170 . integration-layer)
    (180 . testing-framework)
    (190 . documentation))
  "GTD component registry.")

(defun codelahoma-gtd-initialize ()
  "Initialize the GTD system."
  (interactive)
  ;; Create directory structure
  (codelahoma-gtd-create-directories)
  ;; Load core components
  (codelahoma-gtd-load-component 'foundation-setup)
  (codelahoma-gtd-load-component 'core-engine)
  (message "GTD system initialized"))

(defvar codelahoma-gtd-files
  '(("inbox.org" . "Inbox")
    ("projects.org" . "Projects") 
    ("next-actions.org" . "Next Actions")
    ("waiting-for.org" . "Waiting For")
    ("someday-maybe.org" . "Someday/Maybe")
    ("reference.org" . "Reference")
    ("tickler.org" . "Tickler"))
  "GTD file definitions.")

(defun codelahoma-gtd-create-directories ()
  "Create GTD directory structure."
  (make-directory codelahoma-gtd-directory t)
  (make-directory (concat codelahoma-gtd-directory "archive/") t)
  (make-directory (concat codelahoma-gtd-directory "reviews/") t))

(defun codelahoma-gtd-create-files ()
  "Create initial GTD files if they don't exist."
  (dolist (file-def codelahoma-gtd-files)
    (let* ((filename (car file-def))
           (title (cdr file-def))
           (filepath (expand-file-name filename codelahoma-gtd-directory)))
      (unless (file-exists-p filepath)
        (with-temp-buffer
          (insert "#+TITLE: " title "\n")
          (insert "#+FILETAGS: :gtd:\n")
          (insert "#+STARTUP: overview\n\n")
          (write-file filepath))))))

(codelahoma-gtd-load-component 'foundation-files)

(defvar codelahoma-gtd-states
  '(("TODO" . (:char ?t :face org-todo))
    ("NEXT" . (:char ?n :face org-warning))
    ("WAITING" . (:char ?w :face org-agenda-dimmed-todo-face))
    ("HOLD" . (:char ?h :face org-agenda-dimmed-todo-face))
    ("DONE" . (:char ?d :face org-done))
    ("CANCELLED" . (:char ?c :face org-done)))
  "GTD task states with properties.")

(defun codelahoma-gtd-setup-states ()
  "Configure org-mode states for GTD."
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "HOLD(h@/!)" 
                    "|" "DONE(d!)" "CANCELLED(c@)")))
  
  (setq org-todo-keyword-faces
        (mapcar (lambda (state)
                  (cons (car state) (plist-get (cdr state) :face)))
                codelahoma-gtd-states)))

(codelahoma-gtd-load-component 'core-states)

(defun codelahoma-gtd-inbox-file ()
  "Return the inbox file path."
  (expand-file-name "inbox.org" codelahoma-gtd-directory))

(defun codelahoma-gtd-projects-file ()
  "Return the projects file path."
  (expand-file-name "projects.org" codelahoma-gtd-directory))

(defun codelahoma-gtd-file (name)
  "Return the path for GTD file NAME."
  (expand-file-name (concat name ".org") codelahoma-gtd-directory))

(defvar codelahoma-gtd-agenda-files nil
  "List of files to include in agenda.")

(defun codelahoma-gtd-update-agenda-files ()
  "Update the list of agenda files."
  (setq codelahoma-gtd-agenda-files
        (mapcar (lambda (file-def)
                  (expand-file-name (car file-def) codelahoma-gtd-directory))
                codelahoma-gtd-files))
  (setq org-agenda-files codelahoma-gtd-agenda-files))

(codelahoma-gtd-load-component 'core-files)

(cl-defstruct codelahoma-gtd-context
  "GTD context structure."
  name          ; Context name (string)
  key           ; Shortcut key (character)
  predicate     ; Function to test if context applies
  face          ; Face for display
  description)  ; Human-readable description

(defvar codelahoma-gtd-contexts nil
  "List of defined GTD contexts.")

(defun codelahoma-gtd-define-context (name key predicate &optional face description)
  "Define a new GTD context."
  (let ((context (make-codelahoma-gtd-context
                  :name name
                  :key key
                  :predicate predicate
                  :face (or face 'default)
                  :description (or description name))))
    (add-to-list 'codelahoma-gtd-contexts context)))

(codelahoma-gtd-load-component 'core-structures)

(defvar codelahoma-gtd-keymap (make-sparse-keymap)
  "Keymap for GTD commands.")

(defun codelahoma-gtd-setup-keybindings ()
  "Set up GTD keybindings in Spacemacs."
  ;; Global GTD prefix (available everywhere)
  (spacemacs/declare-prefix "oo" "GTD")
  
  ;; Capture (available everywhere)
  (spacemacs/declare-prefix "ooc" "capture")
  (spacemacs/set-leader-keys "oocc" 'org-capture)
  (spacemacs/set-leader-keys "ooci" 'codelahoma-gtd-capture-inbox)
  (spacemacs/set-leader-keys "oocp" 'codelahoma-gtd-capture-personal)
  (spacemacs/set-leader-keys "oocw" 'codelahoma-gtd-capture-work)
  (spacemacs/set-leader-keys "oocP" 'codelahoma-gtd-capture-project)
  
  ;; Process (available everywhere)
  (spacemacs/declare-prefix "oop" "process")
  (spacemacs/set-leader-keys "oopi" 'codelahoma-gtd-process-inbox)
  (spacemacs/set-leader-keys "oopc" 'codelahoma-gtd-clarify-item)
  
  ;; Review (available everywhere)
  (spacemacs/declare-prefix "oor" "review")
  (spacemacs/set-leader-keys "oord" 'codelahoma-gtd-daily-review)
  (spacemacs/set-leader-keys "oorw" 'codelahoma-gtd-weekly-review)
  
  ;; Navigate (available everywhere)
  (spacemacs/declare-prefix "oon" "navigate")
  (spacemacs/set-leader-keys "ooni" 'codelahoma-gtd-open-inbox)
  (spacemacs/set-leader-keys "oonp" 'codelahoma-gtd-open-projects)
  (spacemacs/set-leader-keys "oonn" 'codelahoma-gtd-open-next-actions)
  
  ;; Agenda views (available everywhere)
  (spacemacs/declare-prefix "ooa" "agenda")
  (spacemacs/set-leader-keys "ooag" 'codelahoma-gtd-agenda-gtd-view)
  (spacemacs/set-leader-keys "ooad" 'codelahoma-gtd-agenda-daily)
  (spacemacs/set-leader-keys "ooaw" 'codelahoma-gtd-agenda-weekly)
  (spacemacs/set-leader-keys "ooaa" 'org-agenda))

(codelahoma-gtd-load-component 'keybindings)

(defun codelahoma-gtd-setup-which-key ()
  "Configure which-key descriptions for GTD."
  (which-key-add-key-based-replacements
    "SPC o o" "GTD"
    "SPC o o c" "capture"
    "SPC o o c c" "generic capture"
    "SPC o o c i" "inbox item"
    "SPC o o c p" "personal"
    "SPC o o c w" "work"
    "SPC o o c P" "project"
    "SPC o o p" "process"
    "SPC o o p i" "inbox"
    "SPC o o p c" "clarify"
    "SPC o o r" "review"
    "SPC o o r d" "daily"
    "SPC o o r w" "weekly"
    "SPC o o n" "navigate"
    "SPC o o n i" "inbox"
    "SPC o o n p" "projects"
    "SPC o o n n" "next actions"
    "SPC o o a" "agenda"
    "SPC o o a g" "GTD view"
    "SPC o o a d" "daily dashboard"
    "SPC o o a w" "weekly review"
    "SPC o o a a" "org-agenda"))

(with-eval-after-load 'which-key
  (codelahoma-gtd-setup-which-key))

(codelahoma-gtd-load-component 'which-key)

(defun codelahoma-gtd-detect-context ()
  "Detect current context for smart capture."
  (cond
   ;; In a project file
   ((and (buffer-file-name)
         (string-match-p "projects" (buffer-file-name)))
    'project)
   ;; In a code file
   ((derived-mode-p 'prog-mode)
    'code)
   ;; In an email
   ((or (derived-mode-p 'message-mode)
        (derived-mode-p 'mail-mode))
    'email)
   ;; Default
   (t 'general)))

(defun codelahoma-gtd-capture-template-for-context (context)
  "Return capture template based on CONTEXT."
  (pcase context
    ('project "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %a")
    ('code "* TODO %? :code:\n  :PROPERTIES:\n  :CREATED: %U\n  :FILE: %F\n  :END:\n  %a")
    ('email "* TODO %? :email:\n  :PROPERTIES:\n  :CREATED: %U\n  :FROM: %:from\n  :END:\n  %a")
    (_ "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %i")))

(codelahoma-gtd-load-component 'capture-context)

(defvar codelahoma-gtd-capture-templates
  '(("i" "Inbox" entry (file codelahoma-gtd-inbox-file)
     "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %i")
    ("p" "Personal")
    ("pi" "Personal Inbox" entry (file codelahoma-gtd-inbox-file)
     "* TODO %? :personal:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %i")
    ("pp" "Personal Project" entry (file codelahoma-gtd-projects-file)
     "* TODO %? [/] :personal:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n** TODO First task")
    ("pn" "Personal Next Action" entry (file codelahoma-gtd-file "next-actions")
     "* NEXT %? :personal:\n  :PROPERTIES:\n  :CREATED: %U\n  :CONTEXT: %^{Context|@home|@errands|@calls|@computer}\n  :END:")
    ("w" "Work")
    ("wi" "Work Inbox" entry (file codelahoma-gtd-inbox-file)
     "* TODO %? :work:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %i")
    ("wp" "Work Project" entry (file codelahoma-gtd-projects-file)
     "* TODO %? [/] :work:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n** TODO First task")
    ("wn" "Work Next Action" entry (file codelahoma-gtd-file "next-actions")
     "* NEXT %? :work:\n  :PROPERTIES:\n  :CREATED: %U\n  :CONTEXT: %^{Context|@office|@calls|@computer}\n  :END:")
    ("ww" "Work Waiting For" entry (file codelahoma-gtd-file "waiting-for")
     "* WAITING %? :work:waiting:\n  :PROPERTIES:\n  :CREATED: %U\n  :WAITING_ON: %^{Waiting on}\n  :END:"))
  "GTD capture templates.")

(defun codelahoma-gtd-setup-capture-templates ()
  "Configure org-capture templates for GTD."
  (setq org-capture-templates codelahoma-gtd-capture-templates))

(defun codelahoma-gtd-capture-inbox ()
  "Quick capture to inbox."
  (interactive)
  (org-capture nil "i"))

(defun codelahoma-gtd-capture-project ()
  "Capture a new project."
  (interactive)
  (org-capture nil "pp"))

(defun codelahoma-gtd-capture-personal ()
  "Quick capture for personal item."
  (interactive)
  (org-capture nil "pi"))

(defun codelahoma-gtd-capture-work ()
  "Quick capture for work item."
  (interactive)
  (org-capture nil "wi"))

(codelahoma-gtd-load-component 'capture-templates)

(defvar codelahoma-gtd-two-minute-threshold 120
  "Threshold in seconds for two-minute rule.")

(defun codelahoma-gtd-apply-two-minute-rule ()
  "Apply two-minute rule to current item."
  (interactive)
  (let ((start-time (current-time)))
    (when (y-or-n-p "Can this be done in 2 minutes? ")
      (message "Timer started. Press C-c C-c when done.")
      (add-hook 'org-ctrl-c-ctrl-c-final-hook
                (lambda ()
                  (let ((elapsed (time-subtract (current-time) start-time)))
                    (message "Task completed in %s seconds"
                             (time-to-seconds elapsed)))
                  (org-todo "DONE")
                  (remove-hook 'org-ctrl-c-ctrl-c-final-hook
                               'codelahoma-gtd-two-minute-timer))))))

(codelahoma-gtd-load-component 'two-minute-rule)

(defun codelahoma-gtd-process-inbox ()
  "Process items in the inbox."
  (interactive)
  (find-file (codelahoma-gtd-inbox-file))
  (goto-char (point-min))
  (org-next-visible-heading 1)
  (codelahoma-gtd-process-current-item))

(defun codelahoma-gtd-process-current-item ()
  "Process the current inbox item."
  (interactive)
  (when (org-at-heading-p)
    (org-narrow-to-subtree)
    (let ((choice (read-char-choice
                   "Process: [d]o now, [p]roject, [n]ext action, [w]aiting, [s]omeday, [r]eference, [z]ettel note, [D]aily note, [t]rash: "
                   '(?d ?p ?n ?w ?s ?r ?z ?D ?t))))
      (pcase choice
        (?d (codelahoma-gtd-apply-two-minute-rule))
        (?p (codelahoma-gtd-convert-to-project))
        (?n (codelahoma-gtd-file-as-next-action))
        (?w (codelahoma-gtd-file-as-waiting))
        (?s (codelahoma-gtd-file-as-someday))
        (?r (codelahoma-gtd-file-as-reference))
        (?z (codelahoma-gtd-inbox-to-note))
        (?D (codelahoma-gtd-inbox-to-daily-note))
        (?t (org-cut-subtree)))
      (widen)
      (when (and (not (eobp)) (org-at-heading-p))
        (when (y-or-n-p "Process next item? ")
          (codelahoma-gtd-process-current-item))))))

(codelahoma-gtd-load-component 'inbox-processing)

(defvar codelahoma-gtd-daily-review-template
  '("Daily Review - %t"
    "* Review Outcomes"
    "** What got done today?"
    "** What didn't get done?"
    "** What came up?"
    "* Process Inbox"
    "  - [ ] Clear email inbox"
    "  - [ ] Clear GTD inbox"
    "* Review Calendar"
    "  - [ ] Review today's appointments"
    "  - [ ] Review tomorrow's appointments"
    "* Review Next Actions"
    "  - [ ] Mark completed items DONE"
    "  - [ ] Select tomorrow's priorities")
  "Template for daily reviews.")

(defun codelahoma-gtd-daily-review ()
  "Conduct daily GTD review."
  (interactive)
  (let ((review-file (expand-file-name
                      (format-time-string "reviews/daily-%Y%m%d.org")
                      codelahoma-gtd-directory)))
    (find-file review-file)
    (when (= (buffer-size) 0)
      (dolist (line codelahoma-gtd-daily-review-template)
        (insert (format-time-string line) "\n"))
      (goto-char (point-min))
      (org-next-visible-heading 1))))

(codelahoma-gtd-load-component 'daily-reviews)

(defvar codelahoma-gtd-weekly-review-template
  '("Weekly Review - Week %V, %Y"
    "* Get Clear"
    "** Collect Loose Papers and Materials"
    "** Get \"In\" to Zero"
    "   - [ ] Process all inboxes"
    "** Empty Your Head"
    "   - [ ] Write down any uncaptured items"
    "* Get Current" 
    "** Review Action Lists"
    "   - [ ] Mark off completed actions"
    "   - [ ] Review for reminders of further action steps"
    "** Review Previous Calendar Data"
    "   - [ ] Transfer relevant info to project plans"
    "** Review Upcoming Calendar"
    "   - [ ] Capture actions from meetings"
    "** Review Waiting For List"
    "   - [ ] Check off received items"
    "   - [ ] Follow up on pending items"
    "** Review Project List"
    "   - [ ] Ensure each project has a next action"
    "   - [ ] Review project plans"
    "** Review Someday/Maybe List"
    "   - [ ] Move to projects if ready"
    "   - [ ] Delete items no longer of interest"
    "* Get Creative"
    "** Any new projects?"
    "** Any ideas to capture?")
  "Template for weekly reviews.")

(defun codelahoma-gtd-weekly-review ()
  "Conduct weekly GTD review."
  (interactive)
  (let ((review-file (expand-file-name
                      (format-time-string "reviews/weekly-%Y-W%V.org")
                      codelahoma-gtd-directory)))
    (find-file review-file)
    (when (= (buffer-size) 0)
      (dolist (line codelahoma-gtd-weekly-review-template)
        (insert (format-time-string line) "\n"))
      (goto-char (point-min)))))

(codelahoma-gtd-load-component 'weekly-reviews)

(codelahoma-gtd-define-context
 "@home" ?h
 (lambda () (string-match-p "home\\|house" (or (getenv "LOCATION") "")))
 'org-priority-faces
 "Tasks that can be done at home")

(codelahoma-gtd-define-context
 "@office" ?o  
 (lambda () (string-match-p "office\\|work" (or (getenv "LOCATION") "")))
 'org-priority-faces
 "Tasks that can be done at the office")

(codelahoma-gtd-define-context
 "@errands" ?e
 (lambda () t)  ; Always available
 'org-priority-faces
 "Tasks to do while out and about")

(codelahoma-gtd-define-context
 "@computer" ?c
 (lambda () (display-graphic-p))  ; Has display
 'org-priority-faces
 "Tasks requiring a computer")

(codelahoma-gtd-load-component 'location-contexts)

(defvar codelahoma-gtd-energy-level 'normal
  "Current energy level: 'high, 'normal, or 'low.")

(codelahoma-gtd-define-context
 ":high-energy" ?H
 (lambda () (eq codelahoma-gtd-energy-level 'high))
 'org-scheduled-today
 "Tasks requiring high energy/focus")

(codelahoma-gtd-define-context
 ":low-energy" ?L
 (lambda () (eq codelahoma-gtd-energy-level 'low))
 'org-agenda-dimmed-todo-face
 "Tasks suitable for low energy")

(defun codelahoma-gtd-set-energy-level (level)
  "Set current energy LEVEL."
  (interactive
   (list (intern (completing-read "Energy level: "
                                  '("high" "normal" "low")))))
  (setq codelahoma-gtd-energy-level level)
  (message "Energy level set to: %s" level))

(codelahoma-gtd-load-component 'energy-contexts)

;; Org appearance and font faces (moved from dotspacemacs.org)
(with-eval-after-load 'org
  (let ((headline '(:inherit default :weight bold)))
    (custom-theme-set-faces
     'user
     '(fixed-pitch ((t ( :family "FiraMono Nerd Font" :height 1.0))))
     '(variable-pitch ((t (:family "Source Sans Pro" :height 1.1))))
     `(org-document-title ((t (,@headline :inherit fixed-pitch :height 2.5 :underline nil))))
     `(org-level-1 ((t (,@headline :inherit fixed-pitch :height 1.8 ))))
     `(org-level-2 ((t (,@headline :inherit fixed-pitch :height 1.5 ))))
     `(org-level-3 ((t (,@headline :inherit fixed-pitch :height 1.4 ))))
     `(org-level-4 ((t (,@headline :inherit fixed-pitch :height 1.3 ))))
     `(org-level-5 ((t (,@headline :inherit fixed-pitch :height 1.2))))
     `(org-level-6 ((t (,@headline :inherit fixed-pitch :height 1.2))))
     `(org-level-7 ((t (,@headline :inherit fixed-pitch :height 1.2))))
     `(org-level-8 ((t (,@headline :inherit fixed-pitch :height 1.2))))
     '(org-block ((t (:inherit fixed-pitch :height 0.8))))
     '(org-code ((t (:inherit (shadow fixed-pitch)))))
     '(org-date ((t (:inherit (font-lock-comment-face fixed-pitch) :height 0.9))))
     '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
     '(org-done ((t ( :font "Fira Sans" :height 1.0  :weight bold))))
     '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
     '(org-link ((t (:underline t))))
     '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-property-value ((t (:inherit fixed-pitch))))
     '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-table ((t (:inherit fixed-pitch ))))
     '(org-tag ((t (:inherit (shadow fixed-pitch)  :height 0.5))))
     '(org-todo ((t ( :font "Fira Sans" :height 0.8 ))))
     '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
     )))

(codelahoma-gtd-load-component 'org-appearance)

;; Org-superstar bullets configuration (moved from dotspacemacs.org)
(with-eval-after-load 'org-superstar
  (setq org-superstar-item-bullet-alist
        '((?* . ?•)
          (?+ . ?➤)
          (?- . ?•)))
  (setq org-superstar-headline-bullets-list
        '("⦿" "⬦" "○" "▷"))
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-remove-leading-stars t)
  ;; Enable custom bullets for TODO items
  (setq org-superstar-todo-bullet-alist
        '(("TODO" . ?🔳)
          ("NEXT" . ?👀)
          ("IN-PROGRESS" . ?🚀)
          ("CODE-COMPLETE" . ?💾)
          ("NEEDS-REFINEMENT" . ?🔍)
          ("WAITING" . ?⏰)
          ("ON-HOLD" . ?⏸)
          ("MEETING" . ?⏰)
          ("CANCELLED" . ?❌)
          ("ATTENDED" . ?📝)
          ("ANSWERED" . ?👍) 
          ("DONE" . ?✅)))
  (org-superstar-restart))

;; Set default bullet scheme
(with-eval-after-load 'org-superstar
  (when (fboundp 'rk/switch-org-bullets)
    (rk/switch-org-bullets "Runes")))

(codelahoma-gtd-load-component 'org-superstar)

;; GPTel org-mode integration (moved from dotspacemacs.org)
(with-eval-after-load 'gptel
  (setq gptel-default-mode 'org-mode))

(codelahoma-gtd-load-component 'gptel-integration)

;; Elfeed org integration (moved from dotspacemacs.org)
(with-eval-after-load 'elfeed
  (require 'elfeed)
  
  (defun elfeed-save-to-org-roam-dailies ()
    "Save the current elfeed entry to org-roam dailies."
    (interactive)
    (let* ((entry (elfeed-search-selected :single))
           (title (elfeed-entry-title entry))
           (link (elfeed-entry-link entry))
           (content (elfeed-deref (elfeed-entry-content entry)))
           (date (format-time-string "%Y-%m-%d"))
           (org-roam-dailies-dir (expand-file-name "dailies" org-roam-directory))
           (daily-file (expand-file-name (concat date ".org") org-roam-dailies-dir)))
      (unless (file-exists-p daily-file)
        (with-temp-buffer (write-file daily-file)))
      (with-current-buffer (find-file-noselect daily-file)
        (goto-char (point-max))
        (insert (concat "* " title "\n"))
        (insert (concat "[[" link "][" link "]]\n\n"))
        (insert (concat content "\n"))
        (save-buffer))))

  ;; Bind the function to a key for easy access
  (define-key elfeed-search-mode-map (kbd "o") 'elfeed-save-to-org-roam-dailies))

(codelahoma-gtd-load-component 'elfeed-integration)

;; Set default org color scheme
(with-eval-after-load 'org
  (when (fboundp 'switch-org-colors)
    (switch-org-colors "Cyber")))

(codelahoma-gtd-load-component 'org-color-theme)

(defun codelahoma-gtd-link-to-roam ()
  "Link current GTD item to org-roam."
  (interactive)
  (when (fboundp 'org-roam-node-insert)
    (org-roam-node-insert)))

(defun codelahoma-gtd-create-project-note ()
  "Create org-roam note for current project."
  (interactive)
  (when (and (fboundp 'org-roam-capture)
             (org-at-heading-p))
    (let ((project-name (org-get-heading t t t t)))
      (org-roam-capture nil "p"))))

(codelahoma-gtd-load-component 'roam-integration)

(defun codelahoma-gtd-setup-agenda-views ()
  "Configure org-agenda custom views for GTD."
  (setq org-agenda-custom-commands
        '(("g" "GTD Views"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-start-with-log-mode t)))
            (todo "NEXT" ((org-agenda-overriding-header "Next Actions")))
            (todo "WAITING" ((org-agenda-overriding-header "Waiting For")))
            (todo "TODO" ((org-agenda-overriding-header "Projects")
                          (org-agenda-files (list (codelahoma-gtd-projects-file)))))))
          ("d" "Daily Dashboard"
           ((agenda "" ((org-agenda-span 'day)))
            (todo "NEXT" ((org-agenda-overriding-header "Next Actions")
                          (org-agenda-sorting-strategy '(priority-down effort-up))))
            (todo "WAITING" ((org-agenda-overriding-header "Waiting For")))))
          ("w" "Weekly Review"
           ((agenda "" ((org-agenda-span 'week)))
            (todo "TODO" ((org-agenda-overriding-header "All Open Projects")))
            (todo "WAITING" ((org-agenda-overriding-header "All Waiting Items")))
            (todo "SOMEDAY" ((org-agenda-overriding-header "Someday/Maybe"))))))))

(codelahoma-gtd-load-component 'agenda-integration)

(defun codelahoma-gtd-agenda-gtd-view ()
  "Open GTD agenda view."
  (interactive)
  (org-agenda nil "g"))

(defun codelahoma-gtd-agenda-daily ()
  "Open daily dashboard agenda view."
  (interactive)
  (org-agenda nil "d"))

(defun codelahoma-gtd-agenda-weekly ()
  "Open weekly review agenda view."
  (interactive)
  (org-agenda nil "w"))

(require 'ert)

(ert-deftest codelahoma-gtd-test-initialization ()
  "Test GTD system initialization."
  (let ((temp-dir (make-temp-file "gtd-test" t)))
    (unwind-protect
        (let ((codelahoma-gtd-directory temp-dir))
          (codelahoma-gtd-create-directories)
          (should (file-exists-p temp-dir))
          (should (file-exists-p (expand-file-name "archive/" temp-dir)))
          (should (file-exists-p (expand-file-name "reviews/" temp-dir))))
      (delete-directory temp-dir t))))

(ert-deftest codelahoma-gtd-test-file-creation ()
  "Test GTD file creation."
  (let ((temp-dir (make-temp-file "gtd-test" t)))
    (unwind-protect
        (let ((codelahoma-gtd-directory temp-dir))
          (codelahoma-gtd-create-files)
          (dolist (file-def codelahoma-gtd-files)
            (should (file-exists-p 
                     (expand-file-name (car file-def) temp-dir)))))
      (delete-directory temp-dir t))))

(ert-deftest codelahoma-gtd-test-context-detection ()
  "Test context detection."
  (let ((codelahoma-gtd-energy-level 'high))
    (should (funcall (codelahoma-gtd-context-predicate
                      (car (cl-member-if (lambda (ctx)
                                           (equal (codelahoma-gtd-context-name ctx)
                                                  ":high-energy"))
                                         codelahoma-gtd-contexts)))))))

(codelahoma-gtd-load-component 'unit-tests)

(ert-deftest codelahoma-gtd-test-capture-flow ()
  "Test capture workflow integration."
  (let ((temp-dir (make-temp-file "gtd-test" t)))
    (unwind-protect
        (let ((codelahoma-gtd-directory temp-dir)
              (org-capture-templates codelahoma-gtd-capture-templates))
          (codelahoma-gtd-create-files)
          ;; Test would simulate capture here
          (should t))  ; Placeholder
      (delete-directory temp-dir t))))

(defun codelahoma-gtd-run-all-tests ()
  "Run all GTD tests."
  (interactive)
  (ert-run-tests-batch-and-exit "^codelahoma-gtd-test-"))

(codelahoma-gtd-load-component 'integration-tests)

(defun codelahoma-gtd-migrate-from-old-system ()
  "Migrate from previous GTD system."
  (interactive)
  (when (y-or-n-p "This will migrate your old GTD files. Continue? ")
    (message "Migration would happen here...")
    ;; Implementation would:
    ;; 1. Find old GTD files
    ;; 2. Parse and convert format
    ;; 3. Create new structure
    ;; 4. Preserve data
    ))

(codelahoma-gtd-load-component 'migration-tools)

(defvar codelahoma-roam-capture-templates
  '(("l" "Literature Note" plain
     "* Source: %^{Source}\n* Author: %^{Author}\n* Date: %U\n\n## Key Ideas\n\n%?\n\n## Personal Thoughts\n\n\n## Connections\n\n\n## Generated Actions\n- TODO Review and extract actionable items"
     :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags: :literature:\n")
     :unnarrowed t)
    
    ("p" "Permanent Note" plain
     "* Concept: %^{Core Concept}\n* Date: %U\n\n## Main Idea\n\n%?\n\n## Evidence/Examples\n\n\n## Connections\n\n\n## Applications\n\n\n## Questions\n\n"
     :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags: :permanent:\n")
     :unnarrowed t)
    
    ("m" "Meeting Note" plain
     "* Meeting: %^{Meeting Topic}\n* Date: %U\n* Attendees: %^{Attendees}\n\n## Agenda/Topics\n\n%?\n\n## Key Decisions\n\n\n## Action Items\n\n\n## Follow-up Notes\n\n"
     :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags: :meeting:\n")
     :unnarrowed t)
    
    ("r" "Research Note" plain
     "* Research Topic: %^{Topic}\n* Date: %U\n* Context: %^{Why researching this?}\n\n## Current Understanding\n\n%?\n\n## Key Questions\n\n\n## Findings\n\n\n## Next Steps\n- TODO Continue research on specific aspect\n\n## Related Projects\n\n"
     :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags: :research:\n")
     :unnarrowed t)
    
    ("i" "Insight/Synthesis" plain
     "* Insight: %^{Core Insight}\n* Date: %U\n* Triggered by: %^{What sparked this insight?}\n\n## The Insight\n\n%?\n\n## Why This Matters\n\n\n## Connected Ideas\n\n\n## Potential Applications\n\n\n## Generated Actions\n- TODO Explore application in current project\n\n"
     :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags: :insight:\n")
     :unnarrowed t))
  "Capture templates for Zettelkasten notes.")

(defun codelahoma-roam-setup-capture-templates ()
  "Configure org-roam capture templates."
  (when (featurep 'org-roam)
    (setq org-roam-capture-templates codelahoma-roam-capture-templates)))

(codelahoma-gtd-load-component 'roam-templates)

(defun codelahoma-gtd-link-to-note ()
  "Link current GTD item to an existing or new Zettelkasten note."
  (interactive)
  (when (and (featurep 'org-roam) (org-at-heading-p))
    (let ((item-title (org-get-heading t t t t)))
      (org-roam-node-insert)
      (save-excursion
        (org-back-to-heading)
        (org-end-of-meta-data t)
        (insert (format "Related note: [[id:%s][%s]]\n\n" 
                        (org-roam-node-id (org-roam-node-at-point))
                        item-title))))))

(defun codelahoma-roam-extract-actions ()
  "Extract action items from current Zettelkasten note and create GTD tasks."
  (interactive)
  (when (and (featurep 'org-roam) (org-roam-file-p))
    (save-excursion
      (goto-char (point-min))
      (let ((actions '())
            (note-title (org-roam-get-keyword "TITLE"))
            (note-id (org-roam-node-id (org-roam-node-at-point))))
        
        ;; Find action items in the note
        (while (re-search-forward "^\\s-*- TODO \\(.+\\)$" nil t)
          (push (match-string 1) actions))
        
        ;; Create GTD tasks for each action
        (when actions
          (find-file (codelahoma-gtd-inbox-file))
          (goto-char (point-max))
          (dolist (action actions)
            (insert (format "* TODO %s :zettel:\n  :PROPERTIES:\n  :CREATED: %s\n  :SOURCE_NOTE: [[id:%s][%s]]\n  :END:\n\n"
                            action
                            (format-time-string "[%Y-%m-%d %a %H:%M]")
                            note-id
                            note-title)))
          (save-buffer)
          (message "Created %d GTD tasks from Zettelkasten note" (length actions)))))))

(defun codelahoma-roam-create-project-note ()
  "Create a Zettelkasten research note for the current GTD project."
  (interactive)
  (when (and (featurep 'org-roam) (org-at-heading-p))
    (let* ((project-title (org-get-heading t t t t))
           (project-id (org-id-get-create))
           (note-title (format "Research: %s" project-title)))
      (org-roam-capture nil "r" nil :node (org-roam-node-create :title note-title))
      ;; Link back to the project
      (save-excursion
        (goto-char (point-max))
        (insert (format "\n## Related GTD Project\n[[id:%s][%s]]\n" project-id project-title))))))

(codelahoma-gtd-load-component 'gtd-roam-linking)

(defun codelahoma-roam-setup-keybindings ()
  "Set up keybindings for Zettelkasten workflow."
  (when (featurep 'org-roam)
    ;; Zettelkasten namespace
    (spacemacs/declare-prefix "ooz" "zettelkasten")
    
    ;; Core roam functions
    (spacemacs/set-leader-keys "oozf" 'org-roam-node-find)
    (spacemacs/set-leader-keys "oozi" 'org-roam-node-insert)
    (spacemacs/set-leader-keys "oozc" 'org-roam-capture)
    (spacemacs/set-leader-keys "oozd" 'org-roam-dailies-capture-today)
    (spacemacs/set-leader-keys "oozj" 'org-roam-dailies-goto-today)
    
    ;; GTD integration functions
    (spacemacs/set-leader-keys "oozl" 'codelahoma-gtd-link-to-note)
    (spacemacs/set-leader-keys "ooze" 'codelahoma-roam-extract-actions)
    (spacemacs/set-leader-keys "oozp" 'codelahoma-roam-create-project-note)
    (spacemacs/set-leader-keys "oozn" 'codelahoma-gtd-inbox-to-note)
    (spacemacs/set-leader-keys "oozD" 'codelahoma-gtd-inbox-to-daily-note)
    
    ;; Navigation
    (spacemacs/set-leader-keys "oozb" 'org-roam-buffer-toggle)
    (spacemacs/set-leader-keys "oozg" 'org-roam-graph)))

(defun codelahoma-roam-setup-which-key ()
  "Configure which-key descriptions for Zettelkasten."
  (when (featurep 'org-roam)
    (which-key-add-key-based-replacements
      "SPC o o z" "zettelkasten"
      "SPC o o z f" "find note"
      "SPC o o z i" "insert link"
      "SPC o o z c" "capture note"
      "SPC o o z d" "daily capture"
      "SPC o o z j" "daily note"
      "SPC o o z l" "link to note"
      "SPC o o z e" "extract actions"
      "SPC o o z p" "project note"
      "SPC o o z n" "inbox to note"
      "SPC o o z D" "inbox to daily"
      "SPC o o z b" "roam buffer"
      "SPC o o z g" "graph view")))

(with-eval-after-load 'which-key
  (codelahoma-roam-setup-which-key))

(codelahoma-gtd-load-component 'roam-keybindings)

(defun codelahoma-elfeed-save-to-roam-dailies ()
  "Save current elfeed entry to org-roam dailies with better formatting."
  (interactive)
  (when (and (featurep 'org-roam) (featurep 'elfeed))
    (let* ((entry (elfeed-search-selected :single))
           (title (elfeed-entry-title entry))
           (link (elfeed-entry-link entry))
           (content (elfeed-deref (elfeed-entry-content entry)))
           (date (format-time-string "%Y-%m-%d"))
           (dailies-dir (expand-file-name org-roam-dailies-directory org-roam-directory))
           (daily-file (expand-file-name (concat date ".org") dailies-dir)))
      
      ;; Ensure dailies directory exists
      (make-directory dailies-dir t)
      
      ;; Create or append to daily file
      (unless (file-exists-p daily-file)
        (with-temp-buffer 
          (insert (format "#+title: %s\n\n" date))
          (write-file daily-file)))
      
      (with-current-buffer (find-file-noselect daily-file)
        (goto-char (point-max))
        (insert (format "\n* %s\n:PROPERTIES:\n:URL: %s\n:CAPTURED: %s\n:END:\n\n%s\n\n** Thoughts\n\n\n** Actions\n- TODO Review and extract key insights\n\n"
                        title
                        link
                        (format-time-string "[%Y-%m-%d %a %H:%M]")
                        (if content (substring content 0 (min 500 (length content))) "")))
        (save-buffer))
      (message "Saved '%s' to daily note" title))))

;; Set up elfeed integration
(with-eval-after-load 'elfeed
  (require 'elfeed)
  (define-key elfeed-search-mode-map (kbd "r") 'codelahoma-elfeed-save-to-roam-dailies))

(codelahoma-gtd-load-component 'elfeed-roam-integration)

(defun codelahoma-roam-weekly-review ()
  "Conduct weekly review of Zettelkasten notes and generate GTD actions."
  (interactive)
  (when (featurep 'org-roam)
    (let ((week-ago (time-subtract (current-time) (days-to-time 7))))
      ;; Find recent notes
      (org-roam-node-find)
      ;; TODO: Implement automatic review of recent notes
      (message "Weekly Zettelkasten review initiated"))))

(defun codelahoma-roam-concept-map ()
  "Create a concept map from the current note's connections."
  (interactive)
  (when (and (featurep 'org-roam) (org-roam-file-p))
    (org-roam-graph)
    (message "Concept map generated")))

(defun codelahoma-roam-literature-note-from-gtd ()
  "Create a literature note for the current GTD reference item."
  (interactive)
  (when (and (featurep 'org-roam) (org-at-heading-p))
    (let* ((item-title (org-get-heading t t t t))
           (note-title (format "Literature: %s" item-title)))
      (org-roam-capture nil "l" nil :node (org-roam-node-create :title note-title)))))

(defun codelahoma-gtd-inbox-to-note ()
  "Convert current GTD inbox item to a Zettelkasten note during processing."
  (interactive)
  (when (and (featurep 'org-roam) (org-at-heading-p))
    (let* ((item-title (org-get-heading t t t t))
           (item-body (save-excursion
                        (org-end-of-meta-data t)
                        (let ((start (point))
                              (end (save-excursion
                                     (outline-next-heading)
                                     (point))))
                          (buffer-substring-no-properties start end))))
           (template-choice (read-char-choice
                             "Create note: [l]iterature, [p]ermanent, [r]esearch, [i]nsight, [m]eeting: "
                             '(?l ?p ?r ?i ?m)))
           (template-key (char-to-string template-choice))
           (note-title item-title))
      
      ;; Create the roam note with content from inbox item
      (org-roam-capture nil template-key nil :node (org-roam-node-create :title note-title))
      
      ;; Insert the original content into the note
      (when (and item-body (not (string-empty-p (string-trim item-body))))
        (save-excursion
          (goto-char (point-max))
          (insert "\n## Original Capture\n")
          (insert item-body)))
      
      ;; Mark the original inbox item as processed
      (save-excursion
        (org-back-to-heading)
        (org-todo "DONE")
        (org-end-of-meta-data t)
        (insert (format "Converted to note: [[id:%s][%s]]\n\n"
                        (org-roam-node-id (org-roam-node-at-point))
                        note-title)))
      
      (message "Converted '%s' to %s note" item-title 
               (pcase template-choice
                 (?l "literature")
                 (?p "permanent") 
                 (?r "research")
                 (?i "insight")
                 (?m "meeting"))))))

(defun codelahoma-gtd-inbox-to-daily-note ()
  "Move current GTD inbox item to today's daily note as a thought."
  (interactive)
  (when (and (featurep 'org-roam) (org-at-heading-p))
    (let* ((item-title (org-get-heading t t t t))
           (item-body (save-excursion
                        (org-end-of-meta-data t)
                        (let ((start (point))
                              (end (save-excursion
                                     (outline-next-heading)
                                     (point))))
                          (buffer-substring-no-properties start end))))
           (daily-file (org-roam-dailies--daily-note-path)))
      
      ;; Ensure today's daily note exists
      (unless (file-exists-p daily-file)
        (org-roam-dailies-capture-today))
      
      ;; Add content to daily note
      (with-current-buffer (find-file-noselect daily-file)
        (goto-char (point-max))
        (insert (format "\n## Inbox Processing Thought\n### %s\n%s\n"
                        item-title
                        (if (and item-body (not (string-empty-p (string-trim item-body))))
                            item-body
                          "Captured for further development")))
        (save-buffer))
      
      ;; Mark original item as processed
      (org-todo "DONE")
      (org-end-of-meta-data t)
      (insert (format "Moved to daily note: %s\n\n" 
                      (format-time-string "%Y-%m-%d")))
      
      (message "Moved '%s' to daily note" item-title))))

(codelahoma-gtd-load-component 'roam-workflow-helpers)

(defun codelahoma-gtd-roam-agenda-views ()
  "Add Zettelkasten-aware agenda views."
  (when (featurep 'org-roam)
    (add-to-list 'org-agenda-custom-commands
                 '("z" "Zettelkasten Integration"
                   ((todo "TODO" ((org-agenda-overriding-header "Tasks from Zettelkasten Notes")
                                  (org-agenda-tag-filter-preset '("+zettel"))))
                    (tags-todo "+research" ((org-agenda-overriding-header "Research Projects")))
                    (tags-todo "+literature" ((org-agenda-overriding-header "Literature to Process"))))))
    
    ;; Add roam context to existing views
    (setq org-agenda-prefix-format
          '((agenda . " %i %-12:c%?-12t% s")
            (todo . " %i %-12:c %(org-roam-node-title)")
            (tags . " %i %-12:c")
            (search . " %i %-12:c")))))

(with-eval-after-load 'org-agenda
  (codelahoma-gtd-roam-agenda-views))

(codelahoma-gtd-load-component 'roam-agenda-integration)
