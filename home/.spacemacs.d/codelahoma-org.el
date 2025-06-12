;; Directory and File Management

;; [[file:../codelahoma-org.org::*Directory and File Management][Directory and File Management:1]]
;; ======================================
;; GTD Directory Structure Configuration
;; ======================================

(defvar rk/org-gtd-base-dir (expand-file-name "~/personal/org-files/")
  "Base directory for GTD org files.")

(defvar rk/org-gtd-work-dir (expand-file-name "work/" rk/org-gtd-base-dir)
  "Directory for work-related GTD files.")

(defvar rk/org-gtd-personal-dir (expand-file-name "personal/" rk/org-gtd-base-dir)
  "Directory for personal GTD files.")

(defvar rk/org-gtd-notes-dir (expand-file-name "notes/" rk/org-gtd-personal-dir)
  "Directory for personal note files.")

(defvar rk/org-gtd-meetings-dir (expand-file-name "meetings/" rk/org-gtd-work-dir)
  "Directory for work meeting files.")

(defun rk/org-file (filename &optional subdir)
  "Generate full path for org file FILENAME in optional SUBDIR.
SUBDIR can be 'work', 'personal', 'notes', 'meetings', or nil for base directory."
  (let ((base-dir (cond
                   ((equal subdir "work") rk/org-gtd-work-dir)
                   ((equal subdir "personal") rk/org-gtd-personal-dir)
                   ((equal subdir "notes") rk/org-gtd-notes-dir)
                   ((equal subdir "meetings") rk/org-gtd-meetings-dir)
                   (t rk/org-gtd-base-dir))))
    (expand-file-name filename base-dir)))

(defun rk/create-gtd-structure ()
  "Create GTD dirs/files and insert default headlines for capture targets."
  (interactive)
  (let* ((dirs (list rk/org-gtd-base-dir rk/org-gtd-work-dir rk/org-gtd-personal-dir
                     rk/org-gtd-notes-dir rk/org-gtd-meetings-dir))
         (files `(,(rk/org-file "inbox.org")
                  ,(rk/org-file "archive.org")
                  ,(rk/org-file "gtd.org" "work")
                  ,(rk/org-file "projects.org" "work")
                  ,(rk/org-file "someday.org" "work")
                  ,(rk/org-file "gtd.org" "personal")
                  ,(rk/org-file "projects.org" "personal")
                  ,(rk/org-file "someday.org" "personal"))))
    (dolist (dir dirs)
      (unless (file-exists-p dir)
        (make-directory dir t)))
    (dolist (file files)
      (unless (file-exists-p file)
        (with-temp-file file
          (insert (format "#+TITLE: %s\n" (file-name-base file)))
          (insert "#+STARTUP: overview\n\n")
          ;; Insert headings for capture targets
          (pcase (file-name-nondirectory file)
            ("gtd.org"      (insert "* Tasks\n* High Energy Tasks\n* Low Energy Tasks\n* Meetings\n* Habits\n"))
            ("projects.org" (insert "* Active Projects\n"))
            ("someday.org"  (insert "* Someday/Maybe\n* Reading List\n"))
            ("inbox.org"    (insert "* Inbox\n* Meetings\n")) ;; FIXED: Added a default "Inbox" headline
            (_ nil)))))
  (message "GTD structure initialized with headings.")))

(defun rk/validate-gtd-structure ()
  "Validate that GTD directory structure exists and is complete."
  (interactive)
  (let ((missing-dirs '())
        (missing-files '())
        (dirs `(,rk/org-gtd-base-dir ,rk/org-gtd-work-dir ,rk/org-gtd-personal-dir))
        (files `(,(rk/org-file "inbox.org")
                 ,(rk/org-file "archive.org")
                 ,(rk/org-file "gtd.org" "work")
                 ,(rk/org-file "projects.org" "work")
                 ,(rk/org-file "someday.org" "work")
                 ,(rk/org-file "gtd.org" "personal")
                 ,(rk/org-file "projects.org" "personal")
                 ,(rk/org-file "someday.org" "personal"))))

    (dolist (dir dirs)
      (unless (file-directory-p dir) ;; FIXED: check for directory specifically
        (push dir missing-dirs)))

    (dolist (file files)
      (unless (file-exists-p file)
        (push file missing-files)))

    (if (and (null missing-dirs) (null missing-files))
        (message "✅ GTD structure is complete and valid!")
      (progn
        (when missing-dirs
          ;; FIXED: `string-join` is not a standard elisp function. Use `mapconcat`.
          (message "❌ Missing directories: %s" (mapconcat #'identity missing-dirs ", ")))
        (when missing-files
          (message "❌ Missing files: %s" (mapconcat #'identity missing-files ", ")))))))
;; Directory and File Management:1 ends here

;; GTD TODO Keywords and State Management

;; [[file:../codelahoma-org.org::*GTD TODO Keywords and State Management][GTD TODO Keywords and State Management:1]]
(setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
        (sequence "PROJECT(p)" "|" "COMPLETED(C)")
        (sequence "SOMEDAY(s)" "|" "DECIDED(D)")))

(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)

(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "orange" :weight bold))
        ("TODO" . (:foreground "red" :weight bold))
        ("WAITING" . (:foreground "yellow" :weight bold))
        ("PROJECT" . (:foreground "blue" :weight bold))
        ("SOMEDAY" . (:foreground "purple" :weight bold))))

(setq org-tag-alist
      '(("@work" . ?w) ("@home" . ?h) ("@office" . ?o)
        ("@computer" . ?c) ("@phone" . ?p) ("@errands" . ?e)
        (:startgrouptag)
        ("ENERGY" . ?E)
        (:grouptags)
        ("@high_energy" . ?H) ("@low_energy" . ?L)
        (:endgrouptag)
        (:startgrouptag)
        ("PRIORITY" . ?P)
        (:grouptags)
        ("A" . ?A) ("B" . ?B) ("C" . ?C)
        (:endgrouptag)))

(setq org-fast-tag-selection-single-key t
      org-use-fast-todo-selection t)
;; GTD TODO Keywords and State Management:1 ends here

;; GTD Archive Configuration

;; [[file:../codelahoma-org.org::*GTD Archive Configuration][GTD Archive Configuration:1]]
;; Configure archive location
(setq org-archive-location (concat (rk/org-file "archive.org") "::* Archived Tasks %Y"))

(defun rk/org-archive-add-timestamp ()
  "Add timestamp property when archiving items."
  (org-set-property "ARCHIVED" (format-time-string "[%Y-%m-%d %a %H:%M]")))

(add-hook 'org-archive-hook 'rk/org-archive-add-timestamp)

(defun rk/archive-done-tasks-in-buffer ()
  "Archive all DONE, CANCELLED, COMPLETED, and DECIDED items in current buffer."
  (interactive)
  ;; FIXED: The original code used `org-map-entries` with `org-archive-subtree`,
  ;; which is a classic bug. It modifies the buffer structure while iterating,
  ;; causing it to skip entries. This `while` loop is a robust alternative.
  (let ((archived-count 0)
        (done-matcher (mapconcat #'identity '("DONE" "CANCELLED" "COMPLETED" "DECIDED") "\\|")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (format org-heading-regexp done-matcher) nil t)
        (org-archive-subtree)
        (setq archived-count (1+ archived-count))
        ;; Go back one line to not miss consecutive entries after archiving
        (goto-char (line-beginning-position 0))))
    (message "Archived %d completed items" archived-count)))
;; GTD Archive Configuration:1 ends here

;; GTD Capture Templates Configuration

;; [[file:../codelahoma-org.org::*GTD Capture Templates Configuration][GTD Capture Templates Configuration:1]]
;; Helper functions for capture templates. This is good practice.
(defun rk/capture-file-inbox () (rk/org-file "inbox.org"))
(defun rk/capture-file-work-gtd () (rk/org-file "gtd.org" "work"))
(defun rk/capture-file-work-projects () (rk/org-file "projects.org" "work"))
(defun rk/capture-file-personal-gtd () (rk/org-file "gtd.org" "personal"))
(defun rk/capture-file-personal-projects () (rk/org-file "projects.org" "personal"))
(defun rk/capture-file-work-someday () (rk/org-file "someday.org" "work"))
(defun rk/capture-file-personal-someday () (rk/org-file "someday.org" "personal"))

(setq org-capture-templates
      `( ;; FIXED: The key issue in the original was evaluation time.
         ;; `(file (rk/org-file "inbox.org"))` runs the function ONCE at startup.
         ;; `(file (lambda () (rk/org-file "inbox.org")))` runs the function
         ;; every time you capture, which is what's needed. I've applied this
         ;; fix to all templates that used the incorrect form.
         ;;
         ;; Also fixed `%^{Scheduled}t` to `%^t` which is the correct template
         ;; expansion for a timestamp prompt with a calendar.

         ("i" "Inbox Item" entry
          (file (lambda () (rk/capture-file-inbox)))
          "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"
          :empty-lines 1)

         ("n" "Quick Note" entry
          (file (lambda () (rk/capture-file-inbox)))
          "* %? :NOTE:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"
          :empty-lines 1)

         ;; --- Work Templates ---
         ("w" "Work")
         ("wt" "Work Task" entry
          (file+headline (lambda () (rk/capture-file-work-gtd)) "Tasks")
          "* TODO %? :@work:\n  SCHEDULED: %^t\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"
          :empty-lines 1)

         ("wp" "Work Project" entry
          (file+headline (lambda () (rk/capture-file-work-projects)) "Active Projects")
          "* PROJECT %? :@work:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n\n** Purpose/Outcome\n   %^{Purpose}\n\n** Next Actions\n*** TODO %^{First Action}\n"
          :empty-lines 1)

         ("wm" "Work Meeting" entry
          (file+headline (lambda () (rk/capture-file-work-gtd)) "Meetings")
          "* NEXT Meeting: %? :@work:@agenda:\n  SCHEDULED: %^T\n  :PROPERTIES:\n  :CREATED: %U\n  :ATTENDEES: %^{Attendees}\n  :LOCATION: %^{Location|Office|Remote}\n  :END:\n\n** Agenda\n   %i\n\n** Notes\n\n** Action Items\n"
          :empty-lines 1)

         ;; --- Personal Templates ---
         ("p" "Personal")
         ("pt" "Personal Task" entry
          (file+headline (lambda () (rk/capture-file-personal-gtd)) "Tasks")
          "* TODO %? :@home:\n  SCHEDULED: %^t\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"
          :empty-lines 1)

         ("pp" "Personal Project" entry
          (file+headline (lambda () (rk/capture-file-personal-projects)) "Active Projects")
          "* PROJECT %? :@home:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n\n** Purpose/Outcome\n   %^{Purpose}\n\n** Next Actions\n*** TODO %^{First Action}\n"
          :empty-lines 1)

         ;; --- Someday Templates ---
         ("s" "Someday/Maybe")
         ("sw" "Work Someday" entry
          (file+headline (lambda () (rk/capture-file-work-someday)) "Someday/Maybe")
          "* SOMEDAY %? :@work:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"
          :empty-lines 1)

         ("sp" "Personal Someday" entry
          (file+headline (lambda () (rk/capture-file-personal-someday)) "Someday/Maybe")
          "* SOMEDAY %? :@home:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"
          :empty-lines 1)
         ))
;; GTD Capture Templates Configuration:1 ends here

;; GTD Refile Configuration

;; [[file:../codelahoma-org.org::*GTD Refile Configuration][GTD Refile Configuration:1]]
;; Configure refile targets for GTD system
(setq org-refile-targets
      `( ;; FIXED: Wrapped file paths in a backquoted list and used ,(function-call)
         ;; to ensure the paths are correctly evaluated at load time.
         (nil :maxlevel . 3)
         (,(rk/org-file "gtd.org" "work") :maxlevel . 2)
         (,(rk/org-file "projects.org" "work") :maxlevel . 2)
         (,(rk/org-file "someday.org" "work") :maxlevel . 2)
         (,(rk/org-file "gtd.org" "personal") :maxlevel . 2)
         (,(rk/org-file "projects.org" "personal") :maxlevel . 2)
         (,(rk/org-file "someday.org" "personal") :maxlevel . 2)
         (,(rk/org-file "archive.org") :maxlevel . 1)))

(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-use-cache t)

;; Smart refile function based on context
(defun rk/refile-to-someday ()
  "Quick refile to someday/maybe based on context."
  (interactive)
  (let* ((tags (org-get-tags))
         (is-work (member "work" tags)) ;; Check for "work" tag, not "@work"
         (target-file (if is-work
                          (rk/org-file "someday.org" "work")
                        (rk/org-file "someday.org" "personal"))))
    ;; The final nil in the rfloc list means find headline by name
    (org-refile nil nil (list "Someday/Maybe" target-file nil))))
;; GTD Refile Configuration:1 ends here

;; GTD Context Switching System

;; [[file:../codelahoma-org.org::*GTD Context Switching System][GTD Context Switching System:1]]
;; This is a powerful concept. The implementation below is corrected and robust.

(defvar rk/org-context-mode 'unified
  "Current GTD context mode: 'work, 'personal, or 'unified.")

(defun rk/org-update-agenda-files ()
  "Update `org-agenda-files` based on current context mode."
  (setq org-agenda-files
        (cond
         ((eq rk/org-context-mode 'work)
          `(,(rk/org-file "inbox.org")
            ,(rk/org-file "gtd.org" "work")
            ,(rk/org-file "projects.org" "work")
            ,(rk/org-file "someday.org" "work")))
         ((eq rk/org-context-mode 'personal)
          `(,(rk/org-file "inbox.org")
            ,(rk/org-file "gtd.org" "personal")
            ,(rk/org-file "projects.org" "personal")
            ,(rk/org-file "someday.org" "personal")))
         (t ; unified mode
          `(,(rk/org-file "inbox.org")
            ,(rk/org-file "gtd.org" "work")
            ,(rk/org-file "projects.org" "work")
            ,(rk/org-file "someday.org" "work")
            ,(rk/org-file "gtd.org" "personal")
            ,(rk/org-file "projects.org" "personal")
            ,(rk/org-file "someday.org" "personal")))))
  (org-agenda-prepare-buffers org-agenda-files) ;; Refresh agenda file list
  (message "GTD Context: %s mode activated." (capitalize (symbol-name rk/org-context-mode))))

(defun rk/org-work-mode ()
  "Switch to work-only GTD context."
  (interactive)
  (setq rk/org-context-mode 'work)
  (rk/org-update-agenda-files))

(defun rk/org-personal-mode ()
  "Switch to personal-only GTD context."
  (interactive)
  (setq rk/org-context-mode 'personal)
  (rk/org-update-agenda-files))

(defun rk/org-unified-mode ()
  "Switch to unified GTD context (work + personal)."
  (interactive)
  (setq rk/org-context-mode 'unified)
  (rk/org-update-agenda-files))

(defun rk/org-context-status ()
  "Display current context mode and active files."
  (interactive)
  (message "Current GTD Context: %s" (capitalize (symbol-name rk/org-context-mode))))

;; Initialize context on startup
(defun rk/org-initialize-context ()
  "Initialize GTD context system on startup."
  ;; Default to unified and update file list
  (rk/org-unified-mode))

;; Hook into org-mode loading
(with-eval-after-load 'org
  (rk/org-initialize-context))

;; Ensure hydra is available before defining one
(eval-after-load 'hydra
  '(defhydra rk/org-context-hydra (:color blue :hint nil)
     "
^GTD Context^
^───────────^
_w_: 📊 Work
_p_: 🏠 Personal
_u_: 🌟 Unified
_s_: Show Status
_q_: Quit
"
     ("w" rk/org-work-mode)
     ("p" rk/org-personal-mode)
     ("u" rk/org-unified-mode)
     ("s" rk/org-context-status :color red)
     ("q" nil "Quit")))
;; GTD Context Switching System:1 ends here

;; GTD Custom Agenda Commands

;; [[file:../codelahoma-org.org::*GTD Custom Agenda Commands][GTD Custom Agenda Commands:1]]
(setq org-agenda-custom-commands
      '(("g" "GTD Dashboards")

        ;; FIXED: The original code used a hallucinated variable `org-agenda-tag-filter-preset`.
        ;; The correct way is to use `tags-todo` or `tags` as the agenda type.
        ;; I have rewritten all agenda views to use correct Org Mode syntax.
        ;; Also fixed scoping of `org-agenda-files` to apply to the whole command set.

        ("gw" "Work Dashboard"
         ((agenda "" ((org-agenda-span 'day)))
          (tags-todo "work" ((org-agenda-overriding-header "\n🚀 Next Actions - Work")))
          (todo "WAITING" ((org-agenda-overriding-header "\n⏳ Waiting For - Work")))
          (todo "PROJECT" ((org-agenda-overriding-header "\n📋 Active Projects - Work"))))
         ((org-agenda-files (lambda () (rk/org-update-agenda-files) ; Ensure context is right
                                      (cond ((eq rk/org-context-mode 'personal) '()) ; Show nothing if in personal mode
                                            (t (list (rk/org-file "gtd.org" "work")
                                                     (rk/org-file "projects.org" "work"))))))))

        ("gp" "Personal Dashboard"
         ((agenda "" ((org-agenda-span 'day)))
          (tags-todo "home" ((org-agenda-overriding-header "\n🚀 Next Actions - Personal")))
          (todo "WAITING" ((org-agenda-overriding-header "\n⏳ Waiting For - Personal")))
          (todo "PROJECT" ((org-agenda-overriding-header "\n📋 Active Projects - Personal"))))
         ((org-agenda-files (lambda () (rk/org-update-agenda-files)
                                      (cond ((eq rk/org-context-mode 'work) '())
                                            (t (list (rk/org-file "gtd.org" "personal")
                                                     (rk/org-file "projects.org" "personal"))))))))

        ("gu" "Unified Dashboard"
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-overriding-header "🌟 Unified Dashboard - Complete Overview\n")))
          (alltodo "" ((org-agenda-overriding-header "\n🚀 All Next Actions & Waiting Items\n")))
          (todo "PROJECT" ((org-agenda-overriding-header "\n📋 All Active Projects\n"))))
         ((org-agenda-files (lambda () (rk/org-update-agenda-files) org-agenda-files)))) ; Use current context's files

        ("gi" "Inbox Processing"
         ((alltodo "" ((org-agenda-overriding-header "📥 Inbox - Items to Process\n"))))
         ((org-agenda-files `(,(rk/org-file "inbox.org")))))

        ("ge" "Energy-Based View"
         ((tags-todo "+high_energy"
                     ((org-agenda-overriding-header "⚡ High Energy Tasks")))
          (tags-todo "+low_energy"
                     ((org-agenda-overriding-header "\n🔋 Low Energy Tasks"))))
         ((org-agenda-files (lambda () (rk/org-update-agenda-files) org-agenda-files))))

        ))
;; GTD Custom Agenda Commands:1 ends here

;; GTD Unified Keybinding System

;; [[file:../codelahoma-org.org::*GTD Unified Keybinding System][GTD Unified Keybinding System:1]]
;; This section defines functions that will be bound to keys later.
;; Defining them first avoids "void function" errors on startup.
(defun rk/goto-inbox () (interactive) (find-file (rk/org-file "inbox.org")))
(defun rk/goto-work-gtd () (interactive) (find-file (rk/org-file "gtd.org" "work")))
(defun rk/goto-personal-gtd () (interactive) (find-file (rk/org-file "gtd.org" "personal")))
(defun rk/goto-work-projects () (interactive) (find-file (rk/org-file "projects.org" "work")))
(defun rk/goto-personal-projects () (interactive) (find-file (rk/org-file "projects.org" "personal")))

(defun rk/agenda-work-dashboard () (interactive) (org-agenda nil "gw"))
(defun rk/agenda-personal-dashboard () (interactive) (org-agenda nil "gp"))
(defun rk/agenda-unified-dashboard () (interactive) (org-agenda nil "gu"))
(defun rk/agenda-inbox-processing () (interactive) (org-agenda nil "gi"))

(defun rk/context-aware-capture ()
  "Capture a task, intelligently selecting work/personal based on context."
  (interactive)
  (cond
   ((eq rk/org-context-mode 'work) (org-capture nil "wt"))
   ((eq rk/org-context-mode 'personal) (org-capture nil "pt"))
   (t (let ((choice (completing-read "Capture type: " '("Work Task" "Personal Task" "Inbox"))))
        (cond
         ((string= choice "Work Task") (org-capture nil "wt"))
         ((string= choice "Personal Task") (org-capture nil "pt"))
         (t (org-capture nil "i")))))))

(defun rk/setup-gtd-keybindings ()
  "Set up comprehensive GTD keybindings under SPC o g prefix for GTD."
  ;; Using "g" for GTD to avoid conflict with `org-mode`'s own "o" prefix.
  (spacemacs/declare-prefix "og" "gtd")

  ;; Capture
  (spacemacs/declare-prefix "ogc" "capture")
  (spacemacs/set-leader-keys
    "ogci" '(lambda () (interactive) (org-capture nil "i"))
    "ogcn" '(lambda () (interactive) (org-capture nil "n"))
    "ogcw" '(lambda () (interactive) (org-capture nil "wt"))
    "ogcp" '(lambda () (interactive) (org-capture nil "pt"))
    "ogcW" '(lambda () (interactive) (org-capture nil "wp"))
    "ogcP" '(lambda () (interactive) (org-capture nil "pp"))
    "ogct" 'rk/context-aware-capture
    "ogcC" 'org-capture)

  ;; Agenda
  (spacemacs/declare-prefix "oga" "agenda")
  (spacemacs/set-leader-keys
    "ogaw" 'rk/agenda-work-dashboard
    "ogap" 'rk/agenda-personal-dashboard
    "ogau" 'rk/agenda-unified-dashboard
    "ogai" 'rk/agenda-inbox-processing
    "ogaa" 'org-agenda)

  ;; Go To File
  (spacemacs/declare-prefix "ogg" "goto")
  (spacemacs/set-leader-keys
    "oggi" 'rk/goto-inbox
    "oggw" 'rk/goto-work-gtd
    "oggp" 'rk/goto-personal-gtd
    "oggW" 'rk/goto-work-projects
    "oggP" 'rk/goto-personal-projects)

  ;; Mode Switching
  (spacemacs/declare-prefix "ogm" "mode")
  (spacemacs/set-leader-keys
    "ogmw" 'rk/org-work-mode
    "ogmp" 'rk/org-personal-mode
    "ogmu" 'rk/org-unified-mode
    "ogms" 'rk/org-context-status
    "ogmh" 'rk/org-context-hydra/body)

  ;; Other Operations
  (spacemacs/set-leader-keys
    "ogr" 'org-refile
    "ogA" 'rk/archive-done-tasks-in-buffer))

(with-eval-after-load 'org
  (rk/setup-gtd-keybindings))
;; GTD Unified Keybinding System:1 ends here

;; Org Heading Color Schemes

;; [[file:../codelahoma-org.org::*Org Heading Color Schemes][Org Heading Color Schemes:1]]
(defvar org-heading-colors-schemes
  '(("Arctic"    . ("#88C0D0" "#81A1C1" "#5E81AC" "#B48EAD" "gray"))
    ("Autumn"    . ("#BF616A" "#D08770" "#EBCB8B" "#A3BE8C" "gray"))
    ("Cyber"     . ("#B48EAD" "#88C0D0" "#A3BE8C" "#5E81AC" "gray"))
    ("Nordic"    . ("#8FBCBB" "#88C0D0" "#81A1C1" "#5E81AC" "gray")))
  "Alist of org heading color schemes (using Nord palette for better theme compatibility).")

(defun switch-org-colors (scheme-name)
  "Switch org heading colors to a predefined scheme."
  (interactive
   (list (completing-read "Choose color scheme: "
                         (mapcar #'car org-heading-colors-schemes))))
  (let* ((colors (cdr (assoc scheme-name org-heading-colors-schemes)))
         (variable-tuple '(:family "sans-serif"))
         (headline '(:inherit default :weight normal)))
    (custom-theme-set-faces
     'user
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5 :foreground ,(nth 0 colors)))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.4 :foreground ,(nth 1 colors)))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.3 :foreground ,(nth 2 colors)))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.2 :foreground ,(nth 3 colors)))))
     `(org-done ((t (,@headline :foreground ,(nth 4 colors) :strike-through t)))))
    (message "Switched to %s color scheme" scheme-name)))
;; Org Heading Color Schemes:1 ends here

;; Org Bullet Schemes (Requires org-superstar)

;; [[file:../codelahoma-org.org::*Org Bullet Schemes (Requires org-superstar)][Org Bullet Schemes (Requires org-superstar):1]]
(defvar rk/org-bullet-schemes
  '(("Geometric" . ("◉" "○" "◈" "◇"))
    ("Arrows"    . ("➤" "➢" "➣" "➼"))
    ("Modern"    . ("◆" "▶" "▸" "▹"))
    ("Squares"   . ("⬣" "⬡" "⬢" "⬩")))
  "Alist of org heading bullet schemes for `org-superstar-mode`.")

(defun rk/switch-org-bullets (scheme-name)
  "Switch org heading bullets to a predefined scheme."
  (interactive
   (list (completing-read "Choose bullet scheme: "
                          (mapcar #'car rk/org-bullet-schemes))))
  (if (require 'org-superstar nil 'noerror)
      (let ((bullets (cdr (assoc scheme-name rk/org-bullet-schemes))))
        (setq org-superstar-headline-bullets-list bullets)
        (org-superstar-restart)
        (message "Switched to %s bullet scheme" scheme-name))
    (message "Error: `org-superstar` package is not installed.")))
;; Org Bullet Schemes (Requires org-superstar):1 ends here

;; System Health Check


;; [[file:../codelahoma-org.org::*System Health Check][System Health Check:1]]
(defun rk/gtd-health-check ()
  "Verify GTD system is properly configured and all files exist."
  (interactive)
  (message "Running GTD Health Check...")
  (rk/validate-gtd-structure))

(provide 'codelahoma-org)
;; System Health Check:1 ends here
