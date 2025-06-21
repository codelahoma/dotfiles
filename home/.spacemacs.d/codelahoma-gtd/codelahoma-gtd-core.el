;;; codelahoma-gtd-core.el --- Core GTD functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Core functionality for the personal GTD system including directory management,
;; file operations, and foundational utilities.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-roam)

;;; Task State Management

(defcustom codelahoma-gtd-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "ACTIVE(a)" "|" "DONE(d)")
    (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
    (sequence "PROJECT(p)" "|" "COMPLETED(C)"))
  "GTD task state keywords."
  :type 'sexp
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-todo-keyword-faces
  '(("TODO" . (:foreground "#dc752f" :weight bold))
    ("NEXT" . (:foreground "#4f97d7" :weight bold))
    ("ACTIVE" . (:foreground "#f2241f" :weight bold))
    ("DONE" . (:foreground "#86dc2f" :weight bold))
    ("WAITING" . (:foreground "#b1951d" :weight bold))
    ("HOLD" . (:foreground "#a45bad" :weight bold))
    ("CANCELLED" . (:foreground "#9f8766" :weight bold :strike-through t))
    ("PROJECT" . (:foreground "#2d9574" :weight bold :box t))
    ("COMPLETED" . (:foreground "#86dc2f" :weight bold :box t)))
  "Face properties for GTD todo keywords."
  :type 'alist
  :group 'codelahoma-gtd)

(defun codelahoma-gtd-setup-todo-keywords ()
  "Configure org-mode with GTD todo keywords."
  (setq org-todo-keywords codelahoma-gtd-todo-keywords)
  (setq org-todo-keyword-faces codelahoma-gtd-todo-keyword-faces)
  
  ;; State change logging
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  
  ;; Fast todo selection
  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil))

(defun codelahoma-gtd-set-next-action ()
  "Mark current task as NEXT action."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-todo "NEXT")
    (org-priority ?A)
    (when (org-entry-get nil "DELEGATED_TO")
      (org-set-property "DELEGATED_TO" nil))
    (message "Task marked as NEXT action")))

(defun codelahoma-gtd-delegate-task ()
  "Delegate current task and set to WAITING."
  (interactive)
  (when (eq major-mode 'org-mode)
    (let ((delegate-to (read-string "Delegate to: ")))
      (org-todo "WAITING")
      (org-set-property "DELEGATED_TO" delegate-to)
      (org-set-property "DELEGATED_ON" (format-time-string "[%Y-%m-%d %a]"))
      (org-entry-put nil "WAITING_REASON" (format "Delegated to %s" delegate-to))
      (message "Task delegated to %s" delegate-to))))

(defun codelahoma-gtd-convert-to-project ()
  "Convert current task to a project."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-todo "PROJECT")
    (org-set-property "PROJECT_CREATED" (format-time-string "[%Y-%m-%d %a]"))
    ;; Add project planning template
    (org-end-of-subtree)
    (insert "\n** Outcomes\n- [ ] \n\n** Next Actions\n*** NEXT \n\n** Notes\n")
    (message "Converted to project - add outcomes and next actions")))

;;; Project and Area Structure

(defcustom codelahoma-gtd-project-properties
  '("PROJECT_TYPE" "OUTCOME" "DEADLINE" "STAKEHOLDER" "STATUS")
  "Properties to track for projects."
  :type '(repeat string)
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-areas-of-focus
  '("Personal Development"
    "Health & Fitness"
    "Relationships"
    "Career"
    "Finance"
    "Home & Environment"
    "Hobbies & Recreation"
    "Community & Service")
  "Areas of responsibility for life management."
  :type '(repeat string)
  :group 'codelahoma-gtd)

;;; Project Management Functions

(defun codelahoma-gtd-new-project ()
  "Create a new GTD project with template."
  (interactive)
  (let* ((title (read-string "Project title: "))
         (area (completing-read "Area of focus: " codelahoma-gtd-areas-of-focus))
         (outcome (read-string "Desired outcome: "))
         (deadline (org-read-date nil nil nil "Deadline (optional): ")))
    (find-file (expand-file-name "projects.org" codelahoma-gtd-directory))
    (goto-char (point-max))
    (insert (format "\n* PROJECT %s :%s:\n" title (codelahoma-gtd-area-to-tag area)))
    (org-set-property "AREA_OF_FOCUS" area)
    (org-set-property "OUTCOME" outcome)
    (when deadline
      (org-deadline nil deadline))
    (org-set-property "PROJECT_CREATED" (format-time-string "[%Y-%m-%d %a]"))
    (org-set-property "STATUS" "Active")
    (insert "\n** Purpose/Outcome\n" outcome "\n\n")
    (insert "** Success Criteria\n- [ ] \n\n")
    (insert "** Next Actions\n*** NEXT \n\n")
    (insert "** Project Notes\n")
    (insert (format "Created: %s\n\n" (format-time-string "%Y-%m-%d")))
    (goto-char (point-min))
    (re-search-forward "\\*\\*\\* NEXT " nil t)
    (message "Project created - add next actions")))

(defun codelahoma-gtd-area-to-tag (area)
  "Convert area name to org tag."
  (upcase (replace-regexp-in-string 
           "[& ]+" "_" 
           (replace-regexp-in-string "[^[:alnum:]& ]" "" area))))

(defun codelahoma-gtd-list-projects (&optional area)
  "List all projects, optionally filtered by area."
  (interactive)
  (let ((org-agenda-files (list (expand-file-name "projects.org" codelahoma-gtd-directory))))
    (org-tags-view nil (concat "PROJECT" (when area (format "+%s" (codelahoma-gtd-area-to-tag area)))))))

(defun codelahoma-gtd-project-status ()
  "Show project status overview."
  (interactive)
  (let ((projects (codelahoma-gtd-get-all-projects))
        (active 0) (stalled 0) (completed 0))
    (dolist (project projects)
      (pcase (plist-get project :status)
        ("Active" (cl-incf active))
        ("Stalled" (cl-incf stalled))
        ("Completed" (cl-incf completed))))
    (message "Projects - Active: %d, Stalled: %d, Completed: %d, Total: %d" 
             active stalled completed (length projects))))

(defun codelahoma-gtd-get-all-projects ()
  "Get all projects as a list of plists."
  (let ((projects '()))
    (with-current-buffer (find-file-noselect 
                         (expand-file-name "projects.org" codelahoma-gtd-directory))
      (org-map-entries
       (lambda ()
         (push (list :title (org-get-heading t t t t)
                    :status (or (org-entry-get nil "STATUS") "Active")
                    :area (org-entry-get nil "AREA_OF_FOCUS")
                    :deadline (org-get-deadline-time nil))
               projects))
       "PROJECT"))
    (nreverse projects)))

(defun codelahoma-gtd-find-stalled-projects ()
  "Find projects with no NEXT actions."
  (interactive)
  (let ((org-agenda-files (list (expand-file-name "projects.org" codelahoma-gtd-directory))))
    (org-tags-view nil "PROJECT-TODO=\"NEXT\"")))

(defun codelahoma-gtd-archive-completed-projects ()
  "Archive completed projects."
  (interactive)
  (when (y-or-n-p "Archive all completed projects? ")
    (find-file (expand-file-name "projects.org" codelahoma-gtd-directory))
    (org-map-entries
     (lambda ()
       (when (string= (org-get-todo-state) "COMPLETED")
         (org-archive-subtree)
         (setq org-map-continue-from (point))))
     "PROJECT")
    (org-save-all-org-buffers)
    (message "Completed projects archived")))

;;; Project Templates

(defcustom codelahoma-gtd-project-templates
  '(("software" . ((properties . (("PROJECT_TYPE" . "Software Development")))
                   (sections . ("Purpose/Outcome" "Requirements" "Technical Design" 
                               "Implementation Plan" "Testing Strategy" "Next Actions"))))
    ("learning" . ((properties . (("PROJECT_TYPE" . "Learning")))
                   (sections . ("Learning Objectives" "Resources" "Study Plan" 
                               "Practice Exercises" "Progress Tracking" "Next Actions"))))
    ("home" . ((properties . (("PROJECT_TYPE" . "Home Improvement")))
               (sections . ("Desired Result" "Materials Needed" "Steps" 
                           "Budget" "Timeline" "Next Actions")))))
  "Project templates by type."
  :type 'alist
  :group 'codelahoma-gtd)

(defun codelahoma-gtd-new-project-from-template ()
  "Create a new project from a template."
  (interactive)
  (let* ((template-name (completing-read "Project template: " 
                                        (mapcar #'car codelahoma-gtd-project-templates)))
         (template (alist-get template-name codelahoma-gtd-project-templates nil nil #'string=))
         (title (read-string "Project title: ")))
    (codelahoma-gtd-new-project)
    ;; Apply template properties
    (dolist (prop (alist-get 'properties template))
      (org-set-property (car prop) (cdr prop)))
    ;; Apply template sections
    (goto-char (point-max))
    (dolist (section (alist-get 'sections template))
      (unless (string= section "Next Actions") ; Already added by new-project
        (insert (format "\n** %s\n\n" section))))))

(defun codelahoma-gtd-ensure-directories ()
  "Ensure all GTD directories exist."
  (interactive)
  (let ((base-dir (expand-file-name "~/personal/org-files/"))
        (dirs '("gtd" "gtd/archive" "gtd/reviews"
                "knowledge" "knowledge/permanent" 
                "knowledge/literature" "knowledge/references"
                "knowledge/projects" "knowledge/daily"
                "areas" "resources" "resources/templates"
                "resources/checklists")))
    (dolist (dir dirs)
      (let ((full-path (expand-file-name dir base-dir)))
        (unless (file-exists-p full-path)
          (make-directory full-path t)
          (message "Created directory: %s" full-path))))))

(defun codelahoma-gtd-verify-files ()
  "Verify all required GTD files exist."
  (interactive)
  (let ((files '(("inbox.org" . "Inbox")
                 ("projects.org" . "Projects") 
                 ("next-actions.org" . "Next Actions")
                 ("waiting-for.org" . "Waiting For")
                 ("someday.org" . "Someday Maybe")
                 ("calendar.org" . "Calendar") 
                 ("media.org" . "Media"))))
    (dolist (file-info files)
      (let* ((filename (car file-info))
             (title (cdr file-info))
             (full-path (expand-file-name filename codelahoma-gtd-directory)))
        (unless (file-exists-p full-path)
          (with-temp-buffer
            (insert (format "#+TITLE: %s\n" title))
            (insert "#+FILETAGS: :gtd:\n")
            (insert "#+STARTUP: overview\n")
            (insert "\n")
            (write-file full-path))
          (message "Created file: %s" full-path))))))

(defun codelahoma-gtd-initialize ()
  "Initialize the GTD system, ensuring all directories and files exist."
  (interactive)
  (codelahoma-gtd-ensure-directories)
  (codelahoma-gtd-verify-files)
  ;; Initialize org-roam
  (codelahoma-gtd-roam-setup)
  (codelahoma-gtd-roam-initialize)
  ;; Phase 2: Setup todo keywords, capture, and processing
  (codelahoma-gtd-setup-todo-keywords)
  (require 'codelahoma-gtd-capture)
  (codelahoma-gtd-setup-capture)
  (require 'codelahoma-gtd-process)
  (codelahoma-gtd-setup-refile-targets)
  (codelahoma-gtd-setup-processing-hooks)
  (message "GTD system initialized successfully"))

(defun codelahoma-gtd-open-file (file)
  "Open a GTD FILE from the configured directory."
  (find-file (expand-file-name file codelahoma-gtd-directory)))

;; File access shortcuts
(defun codelahoma-gtd-open-inbox ()
  "Open the GTD inbox file."
  (interactive)
  (codelahoma-gtd-open-file "inbox.org"))

(defun codelahoma-gtd-open-projects ()
  "Open the GTD projects file."
  (interactive)
  (codelahoma-gtd-open-file "projects.org"))

(defun codelahoma-gtd-open-next-actions ()
  "Open the GTD next actions file."
  (interactive)
  (codelahoma-gtd-open-file "next-actions.org"))

(defun codelahoma-gtd-open-waiting-for ()
  "Open the GTD waiting for file."
  (interactive)
  (codelahoma-gtd-open-file "waiting-for.org"))

(defun codelahoma-gtd-open-someday ()
  "Open the GTD someday/maybe file."
  (interactive)
  (codelahoma-gtd-open-file "someday.org"))

(defun codelahoma-gtd-open-calendar ()
  "Open the GTD calendar file."
  (interactive)
  (codelahoma-gtd-open-file "calendar.org"))

(defun codelahoma-gtd-open-media ()
  "Open the GTD media file."
  (interactive)
  (codelahoma-gtd-open-file "media.org"))

;; Auto-save functionality
(defvar codelahoma-gtd-auto-save-timer nil
  "Timer for auto-saving org buffers.")

(defun codelahoma-gtd-auto-save-org-buffers ()
  "Save all org-mode buffers in the GTD directory."
  (save-excursion
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (and (buffer-file-name)
                 (string-prefix-p (expand-file-name codelahoma-gtd-directory)
                                  (buffer-file-name))
                 (buffer-modified-p))
        (save-buffer)))))

(defun codelahoma-gtd-enable-auto-save ()
  "Enable automatic saving of GTD org buffers."
  (interactive)
  (when (and codelahoma-gtd-auto-save-interval
             (> codelahoma-gtd-auto-save-interval 0))
    (when codelahoma-gtd-auto-save-timer
      (cancel-timer codelahoma-gtd-auto-save-timer))
    (setq codelahoma-gtd-auto-save-timer
          (run-with-timer codelahoma-gtd-auto-save-interval
                          codelahoma-gtd-auto-save-interval
                          'codelahoma-gtd-auto-save-org-buffers))
    (message "GTD auto-save enabled (every %d seconds)" 
             codelahoma-gtd-auto-save-interval)))

(defun codelahoma-gtd-disable-auto-save ()
  "Disable automatic saving of GTD org buffers."
  (interactive)
  (when codelahoma-gtd-auto-save-timer
    (cancel-timer codelahoma-gtd-auto-save-timer)
    (setq codelahoma-gtd-auto-save-timer nil)
    (message "GTD auto-save disabled")))

;; Directory validation
(defun codelahoma-gtd-validate-structure ()
  "Validate GTD directory structure and report status."
  (interactive)
  (let ((base-dir (expand-file-name "~/personal/org-files/"))
        (all-good t)
        (messages '()))
    
    ;; Check base directory
    (unless (file-exists-p base-dir)
      (setq all-good nil)
      (push (format "✗ Base directory missing: %s" base-dir) messages))
    
    ;; Check required directories
    (let ((required-dirs '("gtd" "gtd/archive" "gtd/reviews"
                          "knowledge" "knowledge/permanent" 
                          "knowledge/literature" "knowledge/references"
                          "knowledge/projects" "knowledge/daily"
                          "areas" "resources" "resources/templates"
                          "resources/checklists")))
      (dolist (dir required-dirs)
        (let ((full-path (expand-file-name dir base-dir)))
          (unless (file-exists-p full-path)
            (setq all-good nil)
            (push (format "✗ Directory missing: %s" dir) messages)))))
    
    ;; Check required GTD files
    (let ((required-files '("inbox.org" "projects.org" "next-actions.org"
                           "waiting-for.org" "someday.org" "calendar.org" 
                           "media.org")))
      (dolist (file required-files)
        (let ((full-path (expand-file-name (concat "gtd/" file) base-dir)))
          (unless (file-exists-p full-path)
            (setq all-good nil)
            (push (format "✗ File missing: gtd/%s" file) messages)))))
    
    ;; Report status
    (if all-good
        (message "✓ GTD directory structure validated successfully")
      (message "GTD structure validation failed:\n%s" 
               (mapconcat 'identity (reverse messages) "\n")))
    
    all-good))

;; Run validation on load
(defvar codelahoma-gtd-structure-valid nil
  "Whether the GTD directory structure has been validated.")

(defun codelahoma-gtd-check-structure-on-startup ()
  "Check GTD structure on startup and create if needed."
  (unless codelahoma-gtd-structure-valid
    (if (codelahoma-gtd-validate-structure)
        (setq codelahoma-gtd-structure-valid t)
      (when (y-or-n-p "GTD structure incomplete. Create missing directories/files? ")
        (codelahoma-gtd-ensure-directories)
        (codelahoma-gtd-verify-files)
        (setq codelahoma-gtd-structure-valid (codelahoma-gtd-validate-structure))))))

;; Add to initialization
(add-hook 'after-init-hook 'codelahoma-gtd-check-structure-on-startup)

;;; Phase 2 Integration Functions

(defun codelahoma-gtd-initialize-phase2 ()
  "Initialize Phase 2 GTD components."
  (codelahoma-gtd-setup-todo-keywords)
  (require 'codelahoma-gtd-capture)
  (codelahoma-gtd-setup-capture)
  (require 'codelahoma-gtd-process)
  (codelahoma-gtd-setup-refile-targets)
  (codelahoma-gtd-setup-processing-hooks)
  ;; Phase 3: Configure agenda if available
  (when (featurep 'codelahoma-gtd-agenda)
    (codelahoma-gtd-configure-agenda))
  (message "GTD Phase 2 initialized"))

(defun codelahoma-gtd-workflow-status ()
  "Show current GTD system status."
  (interactive)
  (let* ((inbox-count (codelahoma-gtd-count-entries "inbox.org"))
         (next-count (codelahoma-gtd-count-entries "next-actions.org" "NEXT"))
         (waiting-count (codelahoma-gtd-count-entries "waiting-for.org" "WAITING"))
         (project-count (codelahoma-gtd-count-entries "projects.org" "PROJECT"))
         (today-count (length (org-agenda-get-day-entries 
                              (expand-file-name "calendar.org" codelahoma-gtd-directory)
                              (calendar-current-date)))))
    (message "GTD Status - Inbox: %d | Next: %d | Waiting: %d | Projects: %d | Today: %d"
             inbox-count next-count waiting-count project-count today-count)))

(defun codelahoma-gtd-count-entries (file &optional todo-keyword)
  "Count entries in FILE, optionally filtered by TODO-KEYWORD."
  (let ((count 0)
        (path (expand-file-name file codelahoma-gtd-directory)))
    (when (file-exists-p path)
      (with-current-buffer (find-file-noselect path)
        (save-excursion
          (goto-char (point-min))
          (if todo-keyword
              (while (re-search-forward (format "^\\*+ %s " todo-keyword) nil t)
                (cl-incf count))
            (while (org-next-visible-heading 1)
              (cl-incf count))))))
    count))

;; Performance benchmarking
(defun codelahoma-gtd-benchmark-operations ()
  "Benchmark key GTD operations."
  (interactive)
  (let ((results '()))
    ;; Benchmark capture
    (push (cons "Capture" 
                (benchmark-elapse (codelahoma-gtd-capture-inbox)))
          results)
    ;; Benchmark refile
    (push (cons "Refile" 
                (benchmark-elapse 
                 (with-current-buffer (find-file-noselect 
                                      (expand-file-name "inbox.org" codelahoma-gtd-directory))
                   (goto-char (point-min))
                   (org-refile-get-targets))))
          results)
    ;; Benchmark project list
    (push (cons "List Projects" 
                (benchmark-elapse (codelahoma-gtd-get-all-projects)))
          results)
    ;; Display results
    (with-output-to-temp-buffer "*GTD Benchmarks*"
      (princ "GTD Operation Benchmarks\n")
      (princ "========================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-15s: %.3f seconds\n" (car result) (cdr result)))))))

;; Error recovery
(defun codelahoma-gtd-check-health ()
  "Check GTD system health and fix common issues."
  (interactive)
  (let ((issues '()))
    ;; Check required files exist
    (dolist (file '("inbox.org" "projects.org" "next-actions.org" 
                   "waiting-for.org" "someday.org"))
      (unless (file-exists-p (expand-file-name file codelahoma-gtd-directory))
        (push (format "Missing file: %s" file) issues)))
    ;; Check for corrupted properties
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^:PROPERTIES:" nil t)
            (unless (re-search-forward "^:END:" nil t)
              (push (format "Unclosed properties drawer in %s" 
                           (file-name-nondirectory file)) 
                    issues))))))
    ;; Report or fix issues
    (if issues
        (progn
          (with-output-to-temp-buffer "*GTD Health Check*"
            (princ "GTD System Issues Found:\n")
            (princ "========================\n\n")
            (dolist (issue issues)
              (princ (format "- %s\n" issue))))
          (when (y-or-n-p "Attempt automatic fixes? ")
            (codelahoma-gtd-auto-fix-issues issues)))
      (message "GTD system health check passed ✓"))))

(defun codelahoma-gtd-auto-fix-issues (issues)
  "Attempt to automatically fix common issues."
  (dolist (issue issues)
    (cond
     ((string-match "Missing file: \\(.+\\)" issue)
      (codelahoma-gtd-verify-files)
      (message "Created missing files"))
     ((string-match "Unclosed properties drawer" issue)
      (message "Please manually fix unclosed property drawers")))))

;;; Decision Support Functions (Phase 3)

(defun codelahoma-gtd-smart-next-action ()
  "Intelligently suggest the next action based on multiple factors."
  (interactive)
  (let* ((current-contexts (if (fboundp 'codelahoma-gtd-current-contexts)
                              (codelahoma-gtd-current-contexts)
                            '("@anywhere")))
         (current-energy (if (boundp 'codelahoma-gtd-current-energy)
                           codelahoma-gtd-current-energy
                          "medium"))
         (available-time (codelahoma-gtd-estimate-available-time))
         (tasks (codelahoma-gtd-score-all-tasks current-contexts 
                                               current-energy 
                                               available-time)))
    (if tasks
        (codelahoma-gtd-present-smart-suggestions tasks)
      (message "No suitable tasks found for current context"))))

(defun codelahoma-gtd-score-all-tasks (contexts energy time)
  "Score all available tasks based on CONTEXTS, ENERGY, and TIME."
  (let ((scored-tasks '()))
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (when (member (org-get-todo-state) '("NEXT" "TODO"))
             (let ((score (codelahoma-gtd-calculate-task-score 
                          contexts energy time)))
               (when (> score 0)
                 (push (list :title (org-get-heading t t t t)
                           :score score
                           :context (org-entry-get nil "CONTEXT")
                           :effort (org-entry-get nil "Effort")
                           :priority (org-get-priority (org-get-heading))
                           :deadline (org-get-deadline-time nil)
                           :file file
                           :point (point))
                       scored-tasks)))))
         "TODO|NEXT")))
    ;; Sort by score
    (sort scored-tasks (lambda (a b) (> (plist-get a :score) 
                                       (plist-get b :score))))))

(defun codelahoma-gtd-calculate-task-score (contexts energy time)
  "Calculate task score based on CONTEXTS, ENERGY, and TIME."
  (let ((score 0)
        (task-context (org-entry-get nil "CONTEXT"))
        (task-effort (org-entry-get nil "Effort"))
        (task-energy (org-entry-get nil "ENERGY_REQUIRED"))
        (deadline (org-get-deadline-time nil))
        (priority (org-get-priority (org-get-heading))))
    
    ;; Context match (40 points max)
    (when (or (not task-context)
              (member task-context contexts))
      (cl-incf score 40))
    
    ;; Priority score (30 points max)
    (cl-incf score (/ (* (- priority ?A) 30) 3))
    
    ;; Deadline urgency (20 points max)
    (when deadline
      (let ((days-until (/ (- deadline (float-time)) 86400)))
        (cond
         ((< days-until 1) (cl-incf score 20))
         ((< days-until 3) (cl-incf score 15))
         ((< days-until 7) (cl-incf score 10))
         ((< days-until 14) (cl-incf score 5)))))
    
    ;; Energy match (10 points max)
    (when (or (not task-energy)
              (and (fboundp 'codelahoma-gtd-energy-match-p)
                   (codelahoma-gtd-energy-match-p energy task-energy)))
      (cl-incf score 10))
    
    ;; Time fit (penalty for tasks too long)
    (when (and task-effort time)
      (let ((effort-minutes (codelahoma-gtd-effort-to-minutes task-effort)))
        (when (> effort-minutes time)
          (cl-decf score 20))))
    
    score))

(defun codelahoma-gtd-estimate-available-time ()
  "Estimate available time until next appointment."
  (let* ((now (current-time))
         (next-appt (codelahoma-gtd-next-appointment-time)))
    (if next-appt
        (/ (- next-appt (float-time now)) 60) ; minutes
      120))) ; Default 2 hours if no appointments

(defun codelahoma-gtd-next-appointment-time ()
  "Get time of next appointment from calendar."
  ;; Simple implementation - could be enhanced with calendar integration
  nil)

(defun codelahoma-gtd-effort-to-minutes (effort)
  "Convert EFFORT string to minutes."
  (when effort
    (cond
     ((string-match "\\([0-9]+\\)h" effort)
      (* 60 (string-to-number (match-string 1 effort))))
     ((string-match "\\([0-9]+\\)m" effort)
      (string-to-number (match-string 1 effort)))
     ((string-match "\\([0-9]+\\)d" effort)
      (* 480 (string-to-number (match-string 1 effort)))) ; 8 hours per day
     (t 30)))) ; Default 30 minutes

(defun codelahoma-gtd-present-smart-suggestions (tasks)
  "Present TASKS as smart suggestions."
  (let ((buffer (get-buffer-create "*GTD Smart Suggestions*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert "Smart Task Suggestions\n")
      (insert "======================\n\n")
      (insert (format "Context: %s | Energy: %s | Available time: %d min\n\n"
                     (if (fboundp 'codelahoma-gtd-current-contexts)
                         (mapconcat #'identity (codelahoma-gtd-current-contexts) ", ")
                       "@anywhere")
                     (if (boundp 'codelahoma-gtd-current-energy)
                         codelahoma-gtd-current-energy
                       "medium")
                     (codelahoma-gtd-estimate-available-time)))
      
      (let ((count 0))
        (dolist (task (seq-take tasks 5))
          (cl-incf count)
          (let ((start (point)))
            (insert (format "%d. %s (Score: %d)\n"
                           count
                           (plist-get task :title)
                           (plist-get task :score)))
            (insert (format "   %s | Priority %s | Effort: %s\n"
                           (or (plist-get task :context) "Any context")
                           (char-to-string (plist-get task :priority))
                           (or (plist-get task :effort) "?")))
            (when (plist-get task :deadline)
              (insert (format "   Deadline: %s\n"
                             (format-time-string "%Y-%m-%d" 
                                               (plist-get task :deadline)))))
            (insert "\n")
            ;; Make clickable
            (put-text-property start (1- (point)) 'task-data task)
            (put-text-property start (1- (point)) 'keymap 
                              (let ((map (make-sparse-keymap)))
                                (define-key map [mouse-1] 'codelahoma-gtd-open-task)
                                (define-key map (kbd "RET") 'codelahoma-gtd-open-task)
                                map)))))
      (goto-char (point-min))
      (read-only-mode 1))
    (display-buffer buffer)))

(defun codelahoma-gtd-open-task ()
  "Open task at point."
  (interactive)
  (let ((task-data (get-text-property (point) 'task-data)))
    (when task-data
      (find-file (plist-get task-data :file))
      (goto-char (plist-get task-data :point))
      (org-show-entry))))

;;; Decision Tree Processing

(defun codelahoma-gtd-decision-tree ()
  "Interactive decision tree for processing."
  (interactive)
  (let ((item (org-get-heading t t t t)))
    (message "Processing: %s" item)
    
    ;; Is it actionable?
    (if (y-or-n-p "Is this actionable? ")
        (progn
          ;; Yes - determine action type
          (if (y-or-n-p "Will it take less than 2 minutes? ")
              (progn
                (message "Do it now!")
                (org-todo "ACTIVE"))
            ;; More than 2 minutes
            (if (y-or-n-p "Is this a project (multiple steps)? ")
                (codelahoma-gtd-convert-to-project)
              ;; Single action
              (progn
                (org-todo "TODO")
                (when (fboundp 'codelahoma-gtd-assign-context)
                  (codelahoma-gtd-assign-context))
                (when (y-or-n-p "Can you delegate this? ")
                  (codelahoma-gtd-delegate-task))
                (when (y-or-n-p "Schedule for specific time? ")
                  (org-schedule nil))))))
      ;; Not actionable
      (if (y-or-n-p "Is this reference material? ")
          (progn
            (org-set-tags ":REFERENCE:")
            (org-refile nil nil (list nil 
                                    (expand-file-name "reference.org" 
                                                     codelahoma-gtd-directory)
                                    nil nil)))
        ;; Not reference
        (if (y-or-n-p "Might this be useful someday? ")
            (progn
              (org-set-tags ":SOMEDAY:")
              (org-refile nil nil (list nil
                                      (expand-file-name "someday.org"
                                                       codelahoma-gtd-directory)
                                      nil nil)))
          ;; Trash it
          (when (y-or-n-p "Delete this item? ")
            (org-cut-subtree)))))))

;;; Count overdue tasks

(defun codelahoma-gtd-count-overdue ()
  "Count overdue tasks."
  (let ((count 0)
        (today (float-time)))
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (let ((deadline (org-get-deadline-time nil)))
             (when (and deadline (< deadline today))
               (cl-incf count))))
         "TODO|NEXT")))
    count))

(provide 'codelahoma-gtd-core)
;;; codelahoma-gtd-core.el ends here