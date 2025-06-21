;;; codelahoma-bridge.el --- Bridge between GTD and Zettelkasten -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org-roam "2.0"))

;;; Commentary:
;; Bidirectional linking system between GTD tasks and Zettelkasten notes.
;; Enables seamless integration of task management with knowledge management.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)
(require 'org-roam)
(require 'org-id)

;;; Configuration

(defcustom codelahoma-bridge-auto-link t
  "Automatically create links between related tasks and notes."
  :type 'boolean
  :group 'codelahoma-gtd)

(defcustom codelahoma-bridge-backlink-section "* Tasks"
  "Section heading for task backlinks in knowledge notes."
  :type 'string
  :group 'codelahoma-gtd)

(defcustom codelahoma-bridge-link-property "ROAM_REFS"
  "Property used to store knowledge note references in tasks."
  :type 'string
  :group 'codelahoma-gtd)

;;; Core Linking Functions

(defun codelahoma-bridge-link-task-to-note ()
  "Link current task to a Zettelkasten note."
  (interactive)
  (unless (org-entry-get nil "TODO")
    (user-error "Not in a task"))
  (let* ((task-id (org-id-get-create))
         (task-title (org-get-heading t t t t))
         (task-file (buffer-file-name))
         (note (org-roam-node-read nil nil nil t "Link to note: ")))
    (when note
      ;; Add link in task
      (org-entry-put nil codelahoma-bridge-link-property 
                     (org-roam-node-id note))
      ;; Add note title as link
      (org-end-of-meta-data)
      (unless (save-excursion
                (re-search-forward (regexp-quote (org-roam-node-title note))
                                  (save-excursion (org-end-of-subtree t) (point))
                                  t))
        (insert "\nKnowledge: [[id:" (org-roam-node-id note) "]["
                (org-roam-node-title note) "]]\n"))
      ;; Add backlink in note
      (codelahoma-bridge-add-task-backlink 
       note task-id task-title task-file)
      (message "Linked task to: %s" 
               (org-roam-node-title note)))))

(defun codelahoma-bridge-add-task-backlink (note task-id task-title task-file)
  "Add backlink to NOTE for task with TASK-ID and TASK-TITLE from TASK-FILE."
  (let ((note-file (org-roam-node-file note)))
    (with-current-buffer (find-file-noselect note-file)
      (save-excursion
        (goto-char (point-max))
        ;; Find or create tasks section
        (unless (re-search-backward 
                 (concat "^" (regexp-quote codelahoma-bridge-backlink-section))
                 nil t)
          (goto-char (point-max))
          (insert "\n" codelahoma-bridge-backlink-section "\n"))
        (org-end-of-subtree t t)
        ;; Add task link
        (insert "\n- [ ] [[id:" task-id "][" task-title "]] "
                "([[file:" task-file "][" 
                (file-name-nondirectory task-file) "]])\n"))
      (save-buffer))))

(defun codelahoma-bridge-extract-tasks-from-note ()
  "Extract actionable items from current Zettelkasten note."
  (interactive)
  (unless (org-roam-buffer-p)
    (user-error "Not in an org-roam buffer"))
  (let ((note-id (org-roam-id-at-point))
        (note-title (or (org-roam-get-keyword "TITLE")
                       (file-name-base (buffer-file-name))))
        (tasks (codelahoma-bridge-find-todos)))
    (if tasks
        (progn
          (codelahoma-bridge-create-linked-tasks 
           tasks note-id note-title)
          (message "Extracted %d tasks from note" (length tasks)))
      (message "No tasks found in note"))))

(defun codelahoma-bridge-find-todos ()
  "Find TODO items in current buffer."
  (let (todos)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-todo-line-regexp nil t)
        (let ((todo-state (match-string 2))
              (heading (match-string 3)))
          (when (and todo-state heading)
            (push (list :state todo-state
                       :heading heading
                       :point (point)
                       :level (org-current-level))
                  todos)))))
    (nreverse todos)))

(defun codelahoma-bridge-create-linked-tasks (tasks note-id note-title)
  "Create GTD tasks from TASKS linked to NOTE-ID with NOTE-TITLE."
  (let ((inbox-file codelahoma-gtd-inbox-file)
        (created 0))
    (with-current-buffer (find-file-noselect inbox-file)
      (goto-char (point-max))
      (dolist (task tasks)
        (let ((heading (plist-get task :heading))
              (state (plist-get task :state)))
          ;; Create task in inbox
          (insert "\n* " state " " heading "\n")
          (org-set-property codelahoma-bridge-link-property note-id)
          (insert "Source: [[id:" note-id "][" note-title "]]\n")
          (insert "Captured: " (format-time-string "[%Y-%m-%d %a %H:%M]") "\n")
          (cl-incf created)))
      (save-buffer))
    (message "Created %d tasks in inbox" created)))

;;; Navigation Functions

(defun codelahoma-bridge-navigate-link ()
  "Navigate between linked tasks and notes."
  (interactive)
  (cond
   ;; In a task with note reference
   ((and (org-entry-get nil "TODO")
         (org-entry-get nil codelahoma-bridge-link-property))
    (let ((note-id (org-entry-get nil codelahoma-bridge-link-property)))
      (org-roam-node-visit 
       (org-roam-node-from-id note-id))))
   ;; In a note - show linked tasks
   ((org-roam-buffer-p)
    (codelahoma-bridge-show-linked-tasks))
   ;; Look for ID links in text
   ((save-excursion
      (beginning-of-line)
      (re-search-forward "\\[\\[id:\\([^]]+\\)\\]" (line-end-position) t))
    (let ((id (match-string 1)))
      (org-roam-id-open id nil)))
   (t
    (message "No linked items found"))))

(defun codelahoma-bridge-show-linked-tasks ()
  "Show tasks linked to current knowledge note."
  (interactive)
  (unless (org-roam-buffer-p)
    (user-error "Not in an org-roam buffer"))
  (let* ((note-id (org-roam-id-at-point))
         (tasks (codelahoma-bridge-find-linked-tasks note-id)))
    (if tasks
        (codelahoma-bridge-display-linked-tasks tasks)
      (message "No linked tasks found"))))

(defun codelahoma-bridge-find-linked-tasks (note-id)
  "Find all tasks linked to NOTE-ID."
  (let (tasks)
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward 
                  (concat "^\\*+ \\(" org-todo-regexp "\\)") nil t)
            (when (string= (org-entry-get nil codelahoma-bridge-link-property)
                          note-id)
              (push (list :file file
                         :heading (org-get-heading t t t t)
                         :state (org-get-todo-state)
                         :id (org-id-get)
                         :point (point-marker))
                    tasks))))))
    (nreverse tasks)))

(defun codelahoma-bridge-display-linked-tasks (tasks)
  "Display TASKS in a temporary buffer."
  (let ((buffer (get-buffer-create "*Linked Tasks*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Linked Tasks\n\n")
      (dolist (task tasks)
        (insert (format "* %s %s\n" 
                       (plist-get task :state)
                       (plist-get task :heading)))
        (insert (format "  [[file:%s::*%s][Open]]\n"
                       (plist-get task :file)
                       (plist-get task :heading))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buffer)))

;;; Link Management

(defun codelahoma-bridge-unlink-task ()
  "Remove knowledge note link from current task."
  (interactive)
  (when (org-entry-get nil codelahoma-bridge-link-property)
    (org-entry-delete nil codelahoma-bridge-link-property)
    ;; Remove link text if present
    (save-excursion
      (org-end-of-meta-data)
      (when (re-search-forward "^Knowledge: \\[\\[id:[^]]+\\]\\[.*\\]\\]$"
                              (save-excursion (org-end-of-subtree t) (point))
                              t)
        (delete-region (line-beginning-position) 
                      (1+ (line-end-position)))))
    (message "Task unlinked from knowledge note")))

(defun codelahoma-bridge-update-links ()
  "Update all task-note links to ensure consistency."
  (interactive)
  (let ((updated 0))
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward 
                  (concat "^\\*+ \\(" org-todo-regexp "\\)") nil t)
            (when-let ((note-id (org-entry-get nil codelahoma-bridge-link-property)))
              (when (org-roam-node-from-id note-id)
                (cl-incf updated)))))))
    (message "Updated %d task-note links" updated)))

;;; Quick Access Functions

(defun codelahoma-bridge-create-linked-note ()
  "Create a new knowledge note linked to current task."
  (interactive)
  (unless (org-entry-get nil "TODO")
    (user-error "Not in a task"))
  (let* ((task-title (org-get-heading t t t t))
         (task-id (org-id-get-create))
         (default-title (concat "Notes: " task-title)))
    (org-roam-capture- 
     :node (org-roam-node-create :title default-title)
     :templates '(("d" "default" plain "%?"
                   :target (file+head "${slug}.org"
                                     "#+title: ${title}\n#+created: %U\n\n* Overview\n\n* Details\n\n* Tasks\n- [ ] [[id:%^{Task ID}][%^{Task Title}]]\n")
                   :unnarrowed t))
     :props `(:task-id ,task-id :task-title ,task-title))
    ;; Link will be created in the capture template
    (org-entry-put nil codelahoma-bridge-link-property 
                   (caar (last org-roam-capture--info)))))

(defun codelahoma-bridge-jump-to-linked ()
  "Jump to linked knowledge note or task."
  (interactive)
  (codelahoma-bridge-navigate-link))

;;; Integration Helpers

(defun codelahoma-bridge-task-has-note-p ()
  "Check if current task has a linked knowledge note."
  (and (org-entry-get nil "TODO")
       (org-entry-get nil codelahoma-bridge-link-property)))

(defun codelahoma-bridge-note-has-tasks-p ()
  "Check if current knowledge note has linked tasks."
  (and (org-roam-buffer-p)
       (save-excursion
         (goto-char (point-min))
         (re-search-forward 
          (concat "^" (regexp-quote codelahoma-bridge-backlink-section))
          nil t))))

(defun codelahoma-bridge-count-links ()
  "Count task-note links in the system."
  (let ((task-links 0)
        (note-links 0))
    ;; Count task links
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward 
                  (concat ":" codelahoma-bridge-link-property ":") nil t)
            (cl-incf task-links)))))
    ;; Count note backlinks  
    (dolist (node (org-roam-node-list))
      (with-current-buffer (find-file-noselect (org-roam-node-file node))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward 
                 (concat "^" (regexp-quote codelahoma-bridge-backlink-section))
                 nil t)
            (cl-incf note-links)))))
    (message "Found %d task->note links and %d notes with task backlinks" 
             task-links note-links)))

(provide 'codelahoma-bridge)
;;; codelahoma-bridge.el ends here