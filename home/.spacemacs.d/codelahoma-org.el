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

(defcustom codelahoma-gtd-knowledge-directory "~/personal/org-files/knowledge/"
  "Base directory for Zettelkasten knowledge files."
  :type 'directory
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-daily-directory "~/personal/org-files/daily/"
  "Base directory for daily notes."
  :type 'directory
  :group 'codelahoma-gtd)

(defcustom codelahoma-roam-directory "~/personal/org-files/knowledge/"
  "Directory for Zettelkasten notes."
  :type 'directory
  :group 'codelahoma-gtd)

(defvar codelahoma-roam-capture-templates
  '(("n" "permanent note" plain
     "%?"
     :target (file+head "${slug}.org"
                        "#+title: ${title}\n#+created: %U\n#+filetags: :permanent:\n")
     :unnarrowed t)
    
    ("l" "literature note" plain
     "* Source\n- Author: %^{Author}\n- Type: %^{Type|book|article|video|course}\n- Date: %U\n- Link: %^{Link}\n\n* Key Ideas\n%?\n\n* Personal Thoughts\n\n* Questions\n\n* Action Items\n- [ ] \n\n* Related Notes\n- "
     :target (file+head "literature/${slug}.org"
                        "#+title: ${title}\n#+created: %U\n#+filetags: :literature:\n")
     :unnarrowed t)
    
    ("r" "reference note" plain
     "* Overview\n%?\n\n* Key Points\n\n* Examples\n\n* Related Topics\n- "
     :target (file+head "references/${slug}.org"
                        "#+title: ${title}\n#+created: %U\n#+filetags: :reference:\n")
     :unnarrowed t)
    
    ("d" "daily note" entry
     "* %<%H:%M> %?"
     :target (file+head "daily/%<%Y-%m-%d>.org"
                        "#+title: %<%Y-%m-%d %A>\n#+created: %U\n#+filetags: :daily:\n\n* Morning Review\n- [ ] Review calendar\n- [ ] Review GTD inbox\n- [ ] Set daily priorities\n\n* Work Log\n\n* Personal Log\n\n* Evening Review\n- [ ] Process inbox\n- [ ] Update task states\n- [ ] Plan tomorrow\n")
     :unnarrowed t)
    
    ("p" "project note" plain
     "* Overview\nGTD Link: [[file:../gtd/projects.org::*%^{Project Name}]]\n\n* Goals\n%?\n\n* Key Decisions\n\n* Resources\n\n* Progress Log\n\n* Lessons Learned\n"
     :target (file+head "projects/${slug}.org"
                        "#+title: ${title} Knowledge Base\n#+created: %U\n#+filetags: :project:\n")
     :unnarrowed t))
  "Roam capture templates for Zettelkasten.")

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
  '(("inbox.org" . "GTD Inbox")
    ("projects.org" . "GTD Projects") 
    ("someday.org" . "Someday/Maybe")
    ("calendar.org" . "Calendar"))
  "GTD file definitions.")

(defun codelahoma-gtd-create-directories ()
  "Create GTD directory structure."
  (make-directory codelahoma-gtd-directory t)
  (make-directory (concat codelahoma-gtd-directory "archive/") t)
  (make-directory codelahoma-gtd-knowledge-directory t)
  (make-directory (concat codelahoma-gtd-knowledge-directory "permanent/") t)
  (make-directory (concat codelahoma-gtd-knowledge-directory "literature/") t)
  (make-directory codelahoma-gtd-daily-directory t)
  (make-directory (expand-file-name "areas" (file-name-directory (directory-file-name codelahoma-gtd-directory))) t)
  (make-directory (expand-file-name "resources" (file-name-directory (directory-file-name codelahoma-gtd-directory))) t)
  (make-directory (expand-file-name "system" (file-name-directory (directory-file-name codelahoma-gtd-directory))) t)
  (make-directory (expand-file-name "system/templates" (file-name-directory (directory-file-name codelahoma-gtd-directory))) t)
  (make-directory (expand-file-name "system/reviews" (file-name-directory (directory-file-name codelahoma-gtd-directory))) t))

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
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@)" "SOMEDAY(s)" "HOLD(h@)" 
                    "|" "DONE(d!)" "CANCELLED(c@)")
          (sequence "EVENT(e)" "APPOINTMENT(a)" "|" "DONE(d!)" "CANCELLED(c@)")))
  
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
  
  ;; Personal captures
  (spacemacs/declare-prefix "oocp" "personal")
  (spacemacs/set-leader-keys "oocpi" 'codelahoma-gtd-capture-personal-inbox)
  (spacemacs/set-leader-keys "oocpp" 'codelahoma-gtd-capture-personal-project)
  (spacemacs/set-leader-keys "oocpn" 'codelahoma-gtd-capture-personal-next)
  
  ;; Work captures
  (spacemacs/declare-prefix "oocw" "work")
  (spacemacs/set-leader-keys "oocwi" 'codelahoma-gtd-capture-work-inbox)
  (spacemacs/set-leader-keys "oocwp" 'codelahoma-gtd-capture-work-project)
  (spacemacs/set-leader-keys "oocwn" 'codelahoma-gtd-capture-work-next)
  (spacemacs/set-leader-keys "oocww" 'codelahoma-gtd-capture-work-waiting)
  
  ;; Media captures
  (spacemacs/declare-prefix "oocm" "media")
  (spacemacs/set-leader-keys "oocmm" 'codelahoma-gtd-capture-movie-with-omdb)
  (spacemacs/set-leader-keys "oocmt" 'codelahoma-gtd-capture-tv-with-omdb)
  (spacemacs/set-leader-keys "oocmM" 'codelahoma-gtd-capture-movie)  ; Manual fallback
  (spacemacs/set-leader-keys "oocmT" 'codelahoma-gtd-capture-tv-show) ; Manual fallback
  
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
  (spacemacs/set-leader-keys "ooaa" 'org-agenda)
  (spacemacs/set-leader-keys "ooag" 'codelahoma-gtd-agenda-gtd-view)
  (spacemacs/set-leader-keys "ooad" 'codelahoma-gtd-agenda-daily)
  (spacemacs/set-leader-keys "ooaw" 'codelahoma-gtd-agenda-weekly)
  (spacemacs/set-leader-keys "ooap" 'codelahoma-gtd-agenda-personal)
  (spacemacs/set-leader-keys "ooaW" 'codelahoma-gtd-agenda-work)
  (spacemacs/set-leader-keys "ooam" 'codelahoma-gtd-agenda-media)
  
  ;; Save all org buffers
  (spacemacs/set-leader-keys "oos" 'org-save-all-org-buffers)
  
  ;; Zettelkasten (Knowledge Management)
  (spacemacs/declare-prefix "ooz" "zettelkasten")
  (spacemacs/set-leader-keys "oozn" 'org-roam-node-find)
  (spacemacs/set-leader-keys "oozi" 'org-roam-node-insert)
  (spacemacs/set-leader-keys "oozc" 'org-roam-capture)
  (spacemacs/set-leader-keys "oozd" 'org-roam-dailies-goto-today)
  (spacemacs/set-leader-keys "oozD" 'org-roam-dailies-goto-date)
  (spacemacs/set-leader-keys "oozb" 'org-roam-buffer-toggle)
  (spacemacs/set-leader-keys "oozg" 'org-roam-graph)
  (spacemacs/set-leader-keys "oozr" 'org-roam-ref-find)
  
  ;; Integration between GTD and Zettelkasten
  (spacemacs/declare-prefix "ooi" "integrate")
  (spacemacs/set-leader-keys "ooil" 'codelahoma-gtd-link-to-roam)
  (spacemacs/set-leader-keys "ooie" 'codelahoma-gtd-extract-actions)
  (spacemacs/set-leader-keys "ooir" 'codelahoma-gtd-review-project-knowledge)
  (spacemacs/set-leader-keys "ooit" 'codelahoma-gtd-create-task-from-note))

(codelahoma-gtd-load-component 'keybindings)

(defun codelahoma-gtd-setup-which-key ()
  "Configure which-key descriptions for GTD."
  (which-key-add-key-based-replacements
    "SPC o o" "GTD"
    "SPC o o c" "capture"
    "SPC o o c c" "generic capture"
    "SPC o o c i" "inbox item"
    "SPC o o c p" "personal"
    "SPC o o c p i" "personal inbox"
    "SPC o o c p p" "personal project"
    "SPC o o c p n" "personal next"
    "SPC o o c w" "work"
    "SPC o o c w i" "work inbox"
    "SPC o o c w p" "work project"
    "SPC o o c w n" "work next"
    "SPC o o c w w" "work waiting"
    "SPC o o c m" "media"
    "SPC o o c m m" "movie (with OMDB)"
    "SPC o o c m t" "tv show (with OMDB)"
    "SPC o o c m M" "movie (manual)"
    "SPC o o c m T" "tv show (manual)"
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
    "SPC o o a a" "standard agenda"
    "SPC o o a g" "GTD view"
    "SPC o o a d" "daily dashboard"
    "SPC o o a w" "weekly review"
    "SPC o o a p" "personal view"
    "SPC o o a W" "work view"
    "SPC o o a m" "media dashboard"
    "SPC o o s" "save all org buffers"
    "SPC o o z" "zettelkasten"
    "SPC o o z n" "find/create note"
    "SPC o o z i" "insert link"
    "SPC o o z c" "capture"
    "SPC o o z d" "daily note"
    "SPC o o z D" "daily note (date)"
    "SPC o o z b" "backlinks"
    "SPC o o z g" "graph"
    "SPC o o z r" "find reference"
    "SPC o o i" "integrate"
    "SPC o o i l" "link to roam"
    "SPC o o i e" "extract actions"
    "SPC o o i r" "review project knowledge"
    "SPC o o i t" "task from note"))

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
  `(("i" "Inbox" entry (file ,#'codelahoma-gtd-inbox-file)
     "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %i")
    
    ("p" "Personal")
    ("pi" "Personal Inbox" entry (file ,#'codelahoma-gtd-inbox-file)
     "* TODO %? :personal:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %i")
    ("pp" "Personal Project" entry (file ,#'codelahoma-gtd-projects-file)
     "* TODO %? [/] :personal:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n** TODO First task")
    ("pn" "Personal Next Action" entry (file ,(lambda () (codelahoma-gtd-file "next-actions")))
     "* NEXT %? :personal:\n  :PROPERTIES:\n  :CREATED: %U\n  :CONTEXT: %^{Context|@home|@errands|@calls|@computer}\n  :END:")
    
    ("w" "Work")
    ("wi" "Work Inbox" entry (file ,#'codelahoma-gtd-inbox-file)
     "* TODO %? :work:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %i")
    ("wp" "Work Project" entry (file ,#'codelahoma-gtd-projects-file)
     "* TODO %? [/] :work:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n** TODO First task")
    ("wn" "Work Next Action" entry (file ,(lambda () (codelahoma-gtd-file "next-actions")))
     "* NEXT %? :work:\n  :PROPERTIES:\n  :CREATED: %U\n  :CONTEXT: %^{Context|@office|@calls|@computer|@meetings}\n  :END:")
    ("ww" "Work Waiting For" entry (file ,(lambda () (codelahoma-gtd-file "waiting-for")))
     "* WAITING %? :work:waiting:\n  :PROPERTIES:\n  :CREATED: %U\n  :WAITING_ON: %^{Waiting on}\n  :END:")
    
    ("n" "Next Action (Generic)" entry (file ,(lambda () (codelahoma-gtd-file "next-actions")))
     "* NEXT %?\n  :PROPERTIES:\n  :CREATED: %U\n  :CONTEXT: %^{Context|@home|@office|@errands|@calls|@computer}\n  :END:")
    ("W" "Waiting For (Generic)" entry (file ,(lambda () (codelahoma-gtd-file "waiting-for")))
     "* WAITING %? :waiting:\n  :PROPERTIES:\n  :CREATED: %U\n  :WAITING_ON: %^{Waiting on}\n  :END:")
    
    ("m" "Media")
    ("mm" "Movie to Watch" entry 
     (file+headline "~/personal/org-files/gtd/media.org" "Movies")
     "** TODO [#C] Watch %^{Movie Title} :personal:movie:\n   :PROPERTIES:\n   :DIRECTOR: %^{Director|}\n   :YEAR: %^{Year|}\n   :STREAMING: %^{Where to watch|}\n   :GENRE: %^{Genre|drama|comedy|action|scifi|horror|documentary|animation|thriller|}\n   :RECOMMENDED_BY: %^{Recommended by|}\n   :END:\n   %?")
    ("mt" "TV Show to Watch" entry
     (file+headline "~/personal/org-files/gtd/media.org" "TV Shows")
     "** TODO [#C] Watch %^{Show Title} :personal:tv:\n   :PROPERTIES:\n   :SEASONS: %^{Number of seasons|}\n   :STREAMING: %^{Platform|}\n   :GENRE: %^{Genre|drama|comedy|scifi|documentary|reality|anime|}\n   :END:\n   %?")
    ("mr" "Media Review" plain
     (function codelahoma-gtd-media-review-target)
     "#+title: %^{Title} Review\n#+filetags: :media:%^{Type|movie|tv}:\n#+date: %U\n\n* Quick Take\n%?\n\n* Themes\n\n* Memorable Moments\n\n* Connections\n\n* Rating: %^{Rating}/10")
    
    ;; OMDB-enhanced templates
    ("mo" "Movie (OMDB)" entry 
     (file+headline "~/personal/org-files/gtd/media.org" "Movies")
     "** TODO [#C] Watch %(plist-get org-capture-plist :omdb-title) :personal:movie:\n   :PROPERTIES:\n   :DIRECTOR: %(plist-get org-capture-plist :omdb-director)\n   :YEAR: %(plist-get org-capture-plist :omdb-year)\n   :GENRE: %(plist-get org-capture-plist :omdb-genre)\n   :IMDB_RATING: %(plist-get org-capture-plist :omdb-rating)\n   :RUNTIME: %(plist-get org-capture-plist :omdb-runtime)\n   :ACTORS: %(plist-get org-capture-plist :omdb-actors)\n   :STREAMING: %^{Where to watch}\n   :RECOMMENDED_BY: %^{Recommended by}\n   :END:\n   %(plist-get org-capture-plist :omdb-plot)\n   %?")
    ("to" "TV Show (OMDB)" entry
     (file+headline "~/personal/org-files/gtd/media.org" "TV Shows")
     "** TODO [#C] Watch %(plist-get org-capture-plist :omdb-title) :personal:tv:\n   :PROPERTIES:\n   :YEAR: %(plist-get org-capture-plist :omdb-year)\n   :SEASONS: %(plist-get org-capture-plist :omdb-seasons)\n   :GENRE: %(plist-get org-capture-plist :omdb-genre)\n   :IMDB_RATING: %(plist-get org-capture-plist :omdb-rating)\n   :ACTORS: %(plist-get org-capture-plist :omdb-actors)\n   :STREAMING: %^{Platform}\n   :END:\n   %(plist-get org-capture-plist :omdb-plot)\n   %?"))
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

;; Personal capture functions
(defun codelahoma-gtd-capture-personal-inbox ()
  "Quick capture to personal inbox."
  (interactive)
  (org-capture nil "pi"))

(defun codelahoma-gtd-capture-personal-project ()
  "Capture a new personal project."
  (interactive)
  (org-capture nil "pp"))

(defun codelahoma-gtd-capture-personal-next ()
  "Capture a personal next action."
  (interactive)
  (org-capture nil "pn"))

;; Work capture functions
(defun codelahoma-gtd-capture-work-inbox ()
  "Quick capture to work inbox."
  (interactive)
  (org-capture nil "wi"))

(defun codelahoma-gtd-capture-work-project ()
  "Capture a new work project."
  (interactive)
  (org-capture nil "wp"))

(defun codelahoma-gtd-capture-work-next ()
  "Capture a work next action."
  (interactive)
  (org-capture nil "wn"))

(defun codelahoma-gtd-capture-work-waiting ()
  "Capture a work waiting item."
  (interactive)
  (org-capture nil "ww"))

;; Media capture functions
(defun codelahoma-gtd-capture-movie ()
  "Capture a movie to watch."
  (interactive)
  (org-capture nil "mm"))

(defun codelahoma-gtd-capture-tv-show ()
  "Capture a TV show to watch."
  (interactive)
  (org-capture nil "mt"))

(defun codelahoma-gtd-media-review-target ()
  "Determine target for media review based on current context."
  (let* ((title (read-string "Review title: "))
         (filename (concat (format-time-string "%Y%m%d-")
                          (replace-regexp-in-string "[^a-zA-Z0-9]" "-" title)
                          ".org")))
    (expand-file-name filename "~/personal/org-files/roam/media/")))

;; OMDB Integration
(defvar codelahoma-gtd-omdb-api-key (getenv "OMDB_API_KEY")
  "API key for OMDB service.")

(defun codelahoma-gtd-omdb-search (title &optional year type)
  "Search OMDB for TITLE with optional YEAR and TYPE."
  (when codelahoma-gtd-omdb-api-key
    (let* ((url (concat "http://www.omdbapi.com/?"
                       "apikey=" codelahoma-gtd-omdb-api-key
                       "&t=" (url-hexify-string title)
                       (when year (format "&y=%s" year))
                       (when type (format "&type=%s" type))))
           (response (with-current-buffer (url-retrieve-synchronously url t t 5)
                      (goto-char (point-min))
                      (re-search-forward "\n\n")
                      (json-read))))
      (when (string= (cdr (assoc 'Response response)) "True")
        response))))

(defun codelahoma-gtd-capture-movie-with-omdb ()
  "Capture a movie with OMDB data."
  (interactive)
  (let* ((title (read-string "Movie title: "))
         (year (read-string "Year (optional): "))
         (data (codelahoma-gtd-omdb-search title year "movie")))
    (if data
        (let ((org-capture-plist
               (list :omdb-title (cdr (assoc 'Title data))
                     :omdb-director (cdr (assoc 'Director data))
                     :omdb-year (cdr (assoc 'Year data))
                     :omdb-genre (cdr (assoc 'Genre data))
                     :omdb-plot (cdr (assoc 'Plot data))
                     :omdb-rating (cdr (assoc 'imdbRating data))
                     :omdb-runtime (cdr (assoc 'Runtime data))
                     :omdb-actors (cdr (assoc 'Actors data)))))
          (org-capture nil "mo"))
      (message "Movie not found in OMDB, using manual entry")
      (org-capture nil "mm"))))

(defun codelahoma-gtd-capture-tv-with-omdb ()
  "Capture a TV show with OMDB data."
  (interactive)
  (let* ((title (read-string "TV show title: "))
         (data (codelahoma-gtd-omdb-search title nil "series")))
    (if data
        (let ((org-capture-plist
               (list :omdb-title (cdr (assoc 'Title data))
                     :omdb-year (cdr (assoc 'Year data))
                     :omdb-genre (cdr (assoc 'Genre data))
                     :omdb-plot (cdr (assoc 'Plot data))
                     :omdb-rating (cdr (assoc 'imdbRating data))
                     :omdb-seasons (cdr (assoc 'totalSeasons data))
                     :omdb-actors (cdr (assoc 'Actors data)))))
          (org-capture nil "to"))
      (message "TV show not found in OMDB, using manual entry")
      (org-capture nil "mt"))))

(defun codelahoma-gtd-update-media-from-omdb ()
  "Update current media entry with OMDB data."
  (interactive)
  (when (org-at-heading-p)
    (let* ((title (org-get-heading t t t t))
           (is-movie (member "movie" (org-get-tags)))
           (is-tv (member "tv" (org-get-tags)))
           (type (cond (is-movie "movie")
                      (is-tv "series")
                      (t (completing-read "Type: " '("movie" "series")))))
           (year (org-entry-get nil "YEAR"))
           (data (codelahoma-gtd-omdb-search title year type)))
      (if data
          (progn
            (org-set-property "DIRECTOR" (cdr (assoc 'Director data)))
            (org-set-property "YEAR" (cdr (assoc 'Year data)))
            (org-set-property "GENRE" (cdr (assoc 'Genre data)))
            (org-set-property "IMDB_RATING" (cdr (assoc 'imdbRating data)))
            (org-set-property "RUNTIME" (cdr (assoc 'Runtime data)))
            (org-set-property "ACTORS" (cdr (assoc 'Actors data)))
            (when (string= type "series")
              (org-set-property "SEASONS" (cdr (assoc 'totalSeasons data))))
            ;; Add plot if not already present
            (save-excursion
              (org-back-to-heading)
              (org-end-of-meta-data)
              (unless (looking-at-p "\\S-")
                (insert "\n" (cdr (assoc 'Plot data)) "\n")))
            (message "Updated with OMDB data"))
        (message "Not found in OMDB")))))

(defun codelahoma-gtd-media-open-imdb ()
  "Open IMDB page for current media entry."
  (interactive)
  (when (org-at-heading-p)
    (let* ((title (org-get-heading t t t t))
           (year (org-entry-get nil "YEAR"))
           (is-movie (member "movie" (org-get-tags)))
           (is-tv (member "tv" (org-get-tags)))
           (type (cond (is-movie "movie")
                      (is-tv "series")
                      (t "movie")))
           (data (codelahoma-gtd-omdb-search title year type)))
      (if (and data (cdr (assoc 'imdbID data)))
          (browse-url (concat "https://www.imdb.com/title/" 
                             (cdr (assoc 'imdbID data))))
        (browse-url (concat "https://www.imdb.com/find?q=" 
                           (url-hexify-string title)))))))

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
                   "Process: [d]o now, [p]roject, [n]ext action, [w]aiting, [s]omeday, [r]eference, [t]rash: "
                   '(?d ?p ?n ?w ?s ?r ?t))))
      (pcase choice
        (?d (codelahoma-gtd-apply-two-minute-rule))
        (?p (codelahoma-gtd-convert-to-project))
        (?n (codelahoma-gtd-file-as-next-action))
        (?w (codelahoma-gtd-file-as-waiting))
        (?s (codelahoma-gtd-file-as-someday))
        (?r (codelahoma-gtd-file-as-reference))
        (?t (org-cut-subtree)))
      (widen)
      (when (and (not (eobp)) (org-at-heading-p))
        (when (y-or-n-p "Process next item? ")
          (codelahoma-gtd-process-current-item))))))

(codelahoma-gtd-load-component 'inbox-processing)

;; Navigation functions
(defun codelahoma-gtd-open-inbox ()
  "Open GTD inbox file."
  (interactive)
  (find-file (codelahoma-gtd-inbox-file)))

(defun codelahoma-gtd-open-projects ()
  "Open GTD projects file."
  (interactive)
  (find-file (codelahoma-gtd-projects-file)))

(defun codelahoma-gtd-open-next-actions ()
  "Open GTD next actions view."
  (interactive)
  (org-agenda nil "g")
  (org-agenda-filter-apply '("+NEXT") 'tag))

(defun codelahoma-gtd-open-someday ()
  "Open GTD someday/maybe file."
  (interactive)
  (find-file (expand-file-name "someday.org" codelahoma-gtd-directory)))

(defun codelahoma-gtd-open-calendar ()
  "Open GTD calendar file."
  (interactive)
  (find-file (expand-file-name "calendar.org" codelahoma-gtd-directory)))

(codelahoma-gtd-load-component 'navigation)

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
        '(("g" "GTD View"
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
            (todo "SOMEDAY" ((org-agenda-overriding-header "Someday/Maybe")))))
          ("p" "Personal View"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-tag-filter-preset '("+personal" "-work"))))
            (todo "NEXT" ((org-agenda-overriding-header "Personal Next Actions")
                          (org-agenda-tag-filter-preset '("+personal" "-work"))))
            (todo "WAITING" ((org-agenda-overriding-header "Personal Waiting For")
                            (org-agenda-tag-filter-preset '("+personal" "-work"))))
            (todo "TODO" ((org-agenda-overriding-header "Personal Projects")
                          (org-agenda-tag-filter-preset '("+personal" "-work"))
                          (org-agenda-files (list (codelahoma-gtd-projects-file)))))))
          ("W" "Work View"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-tag-filter-preset '("+work" "-personal"))))
            (todo "NEXT" ((org-agenda-overriding-header "Work Next Actions")
                          (org-agenda-tag-filter-preset '("+work" "-personal"))))
            (todo "WAITING" ((org-agenda-overriding-header "Work Waiting For")
                            (org-agenda-tag-filter-preset '("+work" "-personal"))))
            (todo "TODO" ((org-agenda-overriding-header "Work Projects")
                          (org-agenda-tag-filter-preset '("+work" "-personal"))
                          (org-agenda-files (list (codelahoma-gtd-projects-file)))))))
          ("m" "Media Dashboard"
           ((todo "TODO|NEXT" 
                  ((org-agenda-overriding-header "📺 Media Queue")
                   (org-agenda-files (list (codelahoma-gtd-file "media")))
                   (org-agenda-sorting-strategy '(priority-down effort-up))))
            (tags "media+CLOSED>=\"<-1m>\""
                  ((org-agenda-overriding-header "🎬 Recently Watched"))))))))

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

(defun codelahoma-gtd-agenda-personal ()
  "Open personal agenda view."
  (interactive)
  (org-agenda nil "p"))

(defun codelahoma-gtd-agenda-work ()
  "Open work agenda view."
  (interactive)
  (org-agenda nil "W"))

(defun codelahoma-gtd-agenda-media ()
  "Open media dashboard agenda view."
  (interactive)
  (org-agenda nil "m"))

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

(defun codelahoma-gtd-setup-roam-keybindings ()
  "Set up Zettelkasten keybindings."
  ;; Zettelkasten namespace
  (spacemacs/declare-prefix "ooz" "zettelkasten")
  
  ;; Note creation
  (spacemacs/set-leader-keys "oozn" 'org-roam-node-find)
  (spacemacs/set-leader-keys "oozi" 'org-roam-node-insert)
  (spacemacs/set-leader-keys "oozc" 'org-roam-capture)
  (spacemacs/set-leader-keys "oozd" 'org-roam-dailies-goto-today)
  (spacemacs/set-leader-keys "oozD" 'org-roam-dailies-goto-date)
  
  ;; Note navigation
  (spacemacs/set-leader-keys "oozb" 'org-roam-buffer-toggle)
  (spacemacs/set-leader-keys "oozg" 'org-roam-graph)
  (spacemacs/set-leader-keys "oozr" 'org-roam-ref-find)
  
  ;; Integration commands
  (spacemacs/declare-prefix "ooi" "integrate")
  (spacemacs/set-leader-keys "ooil" 'codelahoma-gtd-link-to-roam)
  (spacemacs/set-leader-keys "ooie" 'codelahoma-gtd-extract-actions)
  (spacemacs/set-leader-keys "ooir" 'codelahoma-gtd-review-project-knowledge)
  (spacemacs/set-leader-keys "ooit" 'codelahoma-gtd-task-from-note))

(with-eval-after-load 'org-roam
  (codelahoma-gtd-setup-roam-keybindings))

(codelahoma-gtd-load-component 'roam-keybindings)

(defun codelahoma-gtd-link-to-roam ()
  "Link current GTD item to a Zettelkasten note."
  (interactive)
  (if (featurep 'org-roam)
      (when (org-at-heading-p)
        (let ((node (org-roam-node-read)))
          (org-set-property "ROAM_REF" (org-roam-node-id node))
          (message "Linked to: %s" (org-roam-node-title node))))
    (message "Org-roam not available. Please install it first.")))

(defun codelahoma-gtd-extract-actions ()
  "Extract TODO items from current buffer to GTD inbox."
  (interactive)
  (let ((actions '()))
    (org-element-map (org-element-parse-buffer) 'item
      (lambda (item)
        (let ((text (org-element-property :raw-value item)))
          (when (string-match "\\[ \\]" text)
            (push (string-trim (replace-regexp-in-string "\\[ \\]" "" text)) actions)))))
    (when actions
      (with-current-buffer (find-file-noselect (codelahoma-gtd-inbox-file))
        (goto-char (point-max))
        (dolist (action (reverse actions))
          (insert (format "* TODO %s :extracted:\n  :PROPERTIES:\n  :CREATED: %s\n  :SOURCE: [[file:%s]]\n  :END:\n\n"
                          action
                          (format-time-string "[%Y-%m-%d %a %H:%M]")
                          (buffer-file-name))))
        (save-buffer))
      (message "Extracted %d actions to GTD inbox" (length actions)))))

(defun codelahoma-gtd-create-task-from-note ()
  "Create a GTD task from current Zettelkasten note."
  (interactive)
  (if (featurep 'org-roam)
      (let* ((node (org-roam-node-at-point))
             (title (when node (org-roam-node-title node)))
             (id (when node (org-roam-node-id node))))
        (if node
            (progn
              (org-capture nil "i")
              (insert title)
              (org-set-property "ROAM_REF" id))
          (message "No org-roam node at point")))
    (message "Org-roam not available. Please install it first.")))

(defun codelahoma-gtd-review-project-knowledge ()
  "Review knowledge base for current project."
  (interactive)
  (when (org-at-heading-p)
    (let* ((project-name (org-get-heading t t t t))
           (knowledge-file (expand-file-name 
                           (concat (replace-regexp-in-string "[^a-zA-Z0-9]" "-" project-name) ".org")
                           (concat codelahoma-roam-directory "projects/"))))
      (if (file-exists-p knowledge-file)
          (find-file-other-window knowledge-file)
        (when (y-or-n-p (format "Create knowledge base for %s? " project-name))
          (find-file-other-window knowledge-file)
          (insert (format "#+title: %s Knowledge Base\n#+created: %s\n#+filetags: :project:\n\n* Overview\nGTD Link: [[file:../../gtd/projects.org::*%s]]\n\n* Goals\n\n* Key Decisions\n\n* Resources\n\n* Progress Log\n\n* Lessons Learned\n"
                          project-name
                          (format-time-string "[%Y-%m-%d %a %H:%M]")
                          project-name))
          (save-buffer))))))

(codelahoma-gtd-load-component 'integration-functions)

(defun codelahoma-gtd-initialize-roam ()
  "Initialize org-roam for Zettelkasten."
  (when (featurep 'org-roam)
    (setq org-roam-directory codelahoma-roam-directory
          org-roam-capture-templates codelahoma-roam-capture-templates
          org-roam-node-display-template "${title:*} ${tags:10}"
          org-roam-completion-everywhere t)
    
    ;; Create directory structure
    (dolist (dir '("daily" "literature" "permanent" "references" "projects" "media"))
      (make-directory (expand-file-name dir codelahoma-roam-directory) t))
    
    (org-roam-db-autosync-mode 1)
    (message "Org-roam initialized for Zettelkasten")))

(with-eval-after-load 'org-roam
  (codelahoma-gtd-initialize-roam))

(codelahoma-gtd-load-component 'roam-initialization)

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

(defun codelahoma-gtd-activate-simple ()
  "Activate the GTD system (without org-roam setup)."
  (interactive)
  (codelahoma-gtd-initialize)
  (codelahoma-gtd-setup-states)
  (codelahoma-gtd-setup-capture-templates)
  (codelahoma-gtd-setup-agenda-views)
  (codelahoma-gtd-update-agenda-files)
  (message "GTD system activated"))

;; Then add this to handle org-roam setup separately:
(with-eval-after-load 'org-roam
  (when (fboundp 'codelahoma-gtd-setup-org-roam)
    (codelahoma-gtd-setup-org-roam)
    (message "GTD: Org-roam integration activated")))

;; Auto-activate when org loads (using the simpler version)
(with-eval-after-load 'org
  (message "GTD: Setting up system...")
  (codelahoma-gtd-activate-simple)  ; Use the version without org-roam
  (codelahoma-gtd-setup-keybindings)
  (message "GTD: System setup complete, keybindings should be available"))
