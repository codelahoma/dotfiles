;;; codelahoma-unified-search.el --- Unified search for GTD-Zettelkasten -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Unified search interface that searches across tasks, notes, projects,
;; and all GTD-Zettelkasten content with advanced filtering.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)
(require 'codelahoma-bridge)
(require 'org-roam)
(require 'cl-lib)

;;; Configuration

(defcustom codelahoma-search-default-scope "All"
  "Default search scope."
  :type 'string
  :group 'codelahoma-gtd)

(defcustom codelahoma-search-max-results 100
  "Maximum number of search results to display."
  :type 'integer
  :group 'codelahoma-gtd)

(defcustom codelahoma-search-context-lines 2
  "Number of context lines to show around matches."
  :type 'integer
  :group 'codelahoma-gtd)

(defcustom codelahoma-search-save-history t
  "Save search history between sessions."
  :type 'boolean
  :group 'codelahoma-gtd)

;;; Search Core

(defvar codelahoma-search-history nil
  "History of search queries.")

(defvar codelahoma-search-saved-searches nil
  "List of saved searches.")

(defvar codelahoma-search-results-buffer "*GTD-Search Results*"
  "Name of search results buffer.")

(defun codelahoma-search ()
  "Unified search across GTD and Zettelkasten."
  (interactive)
  (let* ((query (read-string "Search: " nil 'codelahoma-search-history))
         (scope (completing-read "Scope: " 
                                '("All" "Tasks" "Notes" "Projects" 
                                  "Inbox" "Archives" "Linked")
                                nil t codelahoma-search-default-scope))
         (results (codelahoma-search-execute query scope)))
    (codelahoma-search-display-results query scope results)))

(defun codelahoma-search-execute (query scope)
  "Execute QUERY in SCOPE."
  (message "Searching for '%s' in %s..." query scope)
  (let ((results '()))
    (pcase scope
      ("All" 
       (setq results (append (codelahoma-search-tasks query)
                            (codelahoma-search-notes query)
                            (codelahoma-search-projects query))))
      ("Tasks" (setq results (codelahoma-search-tasks query)))
      ("Notes" (setq results (codelahoma-search-notes query)))
      ("Projects" (setq results (codelahoma-search-projects query)))
      ("Inbox" (setq results (codelahoma-search-inbox query)))
      ("Archives" (setq results (codelahoma-search-archives query)))
      ("Linked" (setq results (codelahoma-search-linked query))))
    ;; Limit results
    (when (> (length results) codelahoma-search-max-results)
      (setq results (seq-take results codelahoma-search-max-results)))
    results))

;;; Search Functions by Type

(defun codelahoma-search-tasks (query)
  "Search for QUERY in all tasks."
  (let ((results '()))
    (dolist (file codelahoma-gtd-files)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward query nil t)
              (when (org-at-heading-p)
                (push (codelahoma-search-create-result 
                       :type "Task"
                       :title (org-get-heading t t t t)
                       :file file
                       :pos (point)
                       :context (codelahoma-search-get-context)
                       :todo-state (org-get-todo-state)
                       :tags (org-get-tags)
                       :priority (org-get-priority (org-get-heading t t)))
                      results)))))))
    (nreverse results)))

(defun codelahoma-search-notes (query)
  "Search for QUERY in knowledge notes."
  (let ((results '()))
    (dolist (node (org-roam-node-list))
      (let ((file (org-roam-node-file node))
            (title (org-roam-node-title node)))
        (when (file-exists-p file)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (when (re-search-forward query nil t)
              (push (codelahoma-search-create-result
                     :type "Note"
                     :title title
                     :file file
                     :pos (point)
                     :context (buffer-substring-no-properties
                              (max (point-min) (- (point) 100))
                              (min (point-max) (+ (point) 100)))
                     :tags (org-roam-node-tags node)
                     :id (org-roam-node-id node))
                    results))))))
    results))

(defun codelahoma-search-projects (query)
  "Search for QUERY in projects."
  (let ((results '()))
    (with-current-buffer (find-file-noselect codelahoma-gtd-projects-file)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward query nil t)
          (when (and (org-at-heading-p)
                     (codelahoma-gtd-project-p))
            (push (codelahoma-search-create-result
                   :type "Project"
                   :title (org-get-heading t t t t)
                   :file codelahoma-gtd-projects-file
                   :pos (point)
                   :context (codelahoma-search-get-context)
                   :todo-state "PROJECT"
                   :tags (org-get-tags))
                  results)))))
    results))

(defun codelahoma-search-inbox (query)
  "Search for QUERY in inbox."
  (let ((results '()))
    (with-current-buffer (find-file-noselect codelahoma-gtd-inbox-file)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward query nil t)
          (when (org-at-heading-p)
            (push (codelahoma-search-create-result
                   :type "Inbox"
                   :title (org-get-heading t t t t)
                   :file codelahoma-gtd-inbox-file
                   :pos (point)
                   :context (codelahoma-search-get-context))
                  results)))))
    results))

(defun codelahoma-search-linked (query)
  "Search for QUERY in items with knowledge links."
  (let ((results '()))
    (dolist (file codelahoma-gtd-files)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (when (and (org-entry-get nil codelahoma-bridge-link-property)
                        (save-excursion
                          (re-search-forward query (org-end-of-subtree t) t)))
               (push (codelahoma-search-create-result
                      :type "Linked"
                      :title (org-get-heading t t t t)
                      :file file
                      :pos (point)
                      :context (codelahoma-search-get-context)
                      :link-id (org-entry-get nil codelahoma-bridge-link-property))
                     results)))
           nil 'file))))
    results))

(defun codelahoma-search-archives (query)
  "Search for QUERY in archived items."
  (let ((results '())
        (archive-files (directory-files codelahoma-gtd-directory t "\\.org_archive$")))
    (dolist (file archive-files)
      (when (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward query nil t)
            (push (codelahoma-search-create-result
                   :type "Archive"
                   :title (or (save-excursion
                               (org-back-to-heading t)
                               (org-get-heading t t t t))
                             "Archived Item")
                   :file file
                   :pos (point)
                   :context (buffer-substring-no-properties
                            (max (point-min) (- (point) 100))
                            (min (point-max) (+ (point) 100))))
                  results)))))
    results))

;;; Result Creation and Display

(defun codelahoma-search-create-result (&rest props)
  "Create a search result with PROPS."
  props)

(defun codelahoma-search-get-context ()
  "Get context around current point."
  (let ((start (save-excursion
                (forward-line (- codelahoma-search-context-lines))
                (point)))
        (end (save-excursion
              (forward-line codelahoma-search-context-lines)
              (point))))
    (buffer-substring-no-properties start end)))

(defun codelahoma-search-display-results (query scope results)
  "Display search RESULTS for QUERY in SCOPE."
  (let ((buffer (get-buffer-create codelahoma-search-results-buffer)))
    (with-current-buffer buffer
      (codelahoma-search-results-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (codelahoma-search-insert-header query scope results)
        (codelahoma-search-insert-results results query))
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(defun codelahoma-search-insert-header (query scope results)
  "Insert search header for QUERY, SCOPE, and RESULTS."
  (insert (propertize "Search Results\n" 'face '(:height 1.3 :weight bold)))
  (insert (format "Query: %s | Scope: %s | Found: %d results\n"
                 (propertize query 'face 'font-lock-string-face)
                 (propertize scope 'face 'font-lock-type-face)
                 (length results)))
  (insert (make-string 60 ?─))
  (insert "\n\n"))

(defun codelahoma-search-insert-results (results query)
  "Insert RESULTS with QUERY highlighting."
  (if results
      (dolist (result results)
        (codelahoma-search-insert-result result query))
    (insert "No results found.\n")))

(defun codelahoma-search-insert-result (result query)
  "Insert a single RESULT with QUERY highlighting."
  (let* ((type (plist-get result :type))
         (title (plist-get result :title))
         (file (plist-get result :file))
         (pos (plist-get result :pos))
         (context (plist-get result :context))
         (todo-state (plist-get result :todo-state))
         (tags (plist-get result :tags))
         (start (point)))
    
    ;; Type indicator
    (insert (propertize (format "[%s] " type)
                       'face (codelahoma-search-type-face type)))
    
    ;; Title (clickable)
    (insert-button title
                  'action (lambda (_)
                           (find-file file)
                           (goto-char pos))
                  'follow-link t
                  'face 'link)
    
    ;; Metadata
    (when todo-state
      (insert " " (propertize todo-state 'face 'org-todo)))
    (when tags
      (insert " " (propertize (format ":%s:" (string-join tags ":"))
                             'face 'org-tag)))
    
    (insert "\n")
    
    ;; File path
    (insert (propertize (format "  %s\n" (abbreviate-file-name file))
                       'face 'font-lock-comment-face))
    
    ;; Context with highlighting
    (when context
      (let ((highlighted (codelahoma-search-highlight-match context query)))
        (insert "  " highlighted "\n")))
    
    (insert "\n")))

(defun codelahoma-search-type-face (type)
  "Return face for search result TYPE."
  (pcase type
    ("Task" 'font-lock-keyword-face)
    ("Note" 'font-lock-doc-face)
    ("Project" 'font-lock-function-name-face)
    ("Inbox" 'font-lock-warning-face)
    ("Archive" 'shadow)
    ("Linked" 'font-lock-constant-face)
    (_ 'default)))

(defun codelahoma-search-highlight-match (text query)
  "Highlight QUERY matches in TEXT."
  (let ((case-fold-search t))
    (replace-regexp-in-string
     (regexp-quote query)
     (lambda (match)
       (propertize match 'face 'highlight))
     text)))

;;; Advanced Search

(defun codelahoma-search-advanced ()
  "Advanced search with multiple criteria."
  (interactive)
  (let* ((criteria (codelahoma-search-build-criteria))
         (results (codelahoma-search-execute-advanced criteria)))
    (codelahoma-search-display-results "Advanced Search" "Custom" results)))

(defun codelahoma-search-build-criteria ()
  "Build advanced search criteria interactively."
  (let ((criteria '()))
    ;; Text search
    (let ((text (read-string "Text contains (empty to skip): ")))
      (unless (string-empty-p text)
        (push (cons 'text text) criteria)))
    
    ;; Tags
    (let ((tags (read-string "Tags (comma-separated, empty to skip): ")))
      (unless (string-empty-p tags)
        (push (cons 'tags (split-string tags "," t " ")) criteria)))
    
    ;; TODO state
    (let ((state (completing-read "State: " 
                                 '("Any" "TODO" "NEXT" "WAITING" "DONE" "PROJECT")
                                 nil t "Any")))
      (unless (string= state "Any")
        (push (cons 'state state) criteria)))
    
    ;; Date range
    (when (y-or-n-p "Add date filter? ")
      (let ((date-type (completing-read "Date type: " 
                                       '("Created" "Modified" "Deadline" "Scheduled")))
            (from (org-read-date nil nil nil "From date: "))
            (to (org-read-date nil nil nil "To date: ")))
        (push (cons 'date-range (list :type date-type :from from :to to)) criteria)))
    
    ;; Has links
    (when (y-or-n-p "Only items with knowledge links? ")
      (push (cons 'has-links t) criteria))
    
    criteria))

(defun codelahoma-search-execute-advanced (criteria)
  "Execute search with advanced CRITERIA."
  (let ((results '()))
    ;; Start with all items
    (dolist (file codelahoma-gtd-files)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (when (codelahoma-search-match-criteria-p criteria)
               (push (codelahoma-search-create-result
                      :type "Advanced"
                      :title (org-get-heading t t t t)
                      :file file
                      :pos (point)
                      :context (codelahoma-search-get-context)
                      :todo-state (org-get-todo-state)
                      :tags (org-get-tags))
                     results)))
           nil 'file))))
    (nreverse results)))

(defun codelahoma-search-match-criteria-p (criteria)
  "Check if current entry matches CRITERIA."
  (cl-every
   (lambda (criterion)
     (pcase (car criterion)
       ('text
        (let ((text (cdr criterion)))
          (save-excursion
            (re-search-forward text (org-end-of-subtree t) t))))
       ('tags
        (let ((required-tags (cdr criterion))
              (item-tags (org-get-tags)))
          (cl-some (lambda (tag) (member tag item-tags)) required-tags)))
       ('state
        (string= (org-get-todo-state) (cdr criterion)))
       ('has-links
        (org-entry-get nil codelahoma-bridge-link-property))
       ('date-range
        (codelahoma-search-match-date-range-p (cdr criterion)))
       (_ t)))
   criteria))

(defun codelahoma-search-match-date-range-p (date-range)
  "Check if current entry matches DATE-RANGE."
  (let* ((type (plist-get date-range :type))
         (from (plist-get date-range :from))
         (to (plist-get date-range :to))
         (date (pcase type
                 ("Created" (org-entry-get nil "CREATED"))
                 ("Modified" (org-entry-get nil "MODIFIED"))
                 ("Deadline" (org-entry-get nil "DEADLINE"))
                 ("Scheduled" (org-entry-get nil "SCHEDULED")))))
    (when date
      (let ((item-time (org-time-string-to-time date))
            (from-time (org-time-string-to-time from))
            (to-time (org-time-string-to-time to)))
        (and (time-less-p from-time item-time)
             (time-less-p item-time to-time))))))

;;; Saved Searches

(defun codelahoma-search-save ()
  "Save current search."
  (interactive)
  (when (eq major-mode 'codelahoma-search-results-mode)
    (let ((name (read-string "Save search as: "))
          (search (list :query codelahoma-search-last-query
                       :scope codelahoma-search-last-scope)))
      (push (cons name search) codelahoma-search-saved-searches)
      (codelahoma-search-persist-saved)
      (message "Search saved as '%s'" name))))

(defun codelahoma-search-load ()
  "Load a saved search."
  (interactive)
  (if codelahoma-search-saved-searches
      (let* ((name (completing-read "Load search: " 
                                   (mapcar #'car codelahoma-search-saved-searches)))
             (search (alist-get name codelahoma-search-saved-searches 
                               nil nil #'string=)))
        (when search
          (let ((query (plist-get search :query))
                (scope (plist-get search :scope)))
            (codelahoma-search-execute-and-display query scope))))
    (message "No saved searches")))

(defun codelahoma-search-execute-and-display (query scope)
  "Execute and display search for QUERY in SCOPE."
  (let ((results (codelahoma-search-execute query scope)))
    (codelahoma-search-display-results query scope results)))

(defun codelahoma-search-persist-saved ()
  "Save searches to file."
  (let ((file (expand-file-name "saved-searches.el" codelahoma-gtd-directory)))
    (with-temp-file file
      (print codelahoma-search-saved-searches (current-buffer)))))

(defun codelahoma-search-load-saved ()
  "Load saved searches from file."
  (let ((file (expand-file-name "saved-searches.el" codelahoma-gtd-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (setq codelahoma-search-saved-searches (read (current-buffer)))))))

;;; Search Results Mode

(defvar codelahoma-search-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'codelahoma-search-refresh)
    (define-key map "s" 'codelahoma-search)
    (define-key map "S" 'codelahoma-search-advanced)
    (define-key map "f" 'codelahoma-search-filter)
    (define-key map "n" 'codelahoma-search-next-result)
    (define-key map "p" 'codelahoma-search-previous-result)
    (define-key map "q" 'quit-window)
    (define-key map "?" 'codelahoma-search-help)
    map)
  "Keymap for search results mode.")

(defvar-local codelahoma-search-last-query nil
  "Last search query.")

(defvar-local codelahoma-search-last-scope nil
  "Last search scope.")

(define-derived-mode codelahoma-search-results-mode special-mode "Search-Results"
  "Major mode for GTD-Zettelkasten search results.

\\{codelahoma-search-results-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defun codelahoma-search-refresh ()
  "Refresh search results."
  (interactive)
  (when (and codelahoma-search-last-query codelahoma-search-last-scope)
    (codelahoma-search-execute-and-display 
     codelahoma-search-last-query 
     codelahoma-search-last-scope)))

(defun codelahoma-search-filter ()
  "Filter current results."
  (interactive)
  (message "Filter functionality not yet implemented"))

(defun codelahoma-search-next-result ()
  "Jump to next result."
  (interactive)
  (when (re-search-forward "^\\[" nil t)
    (beginning-of-line)))

(defun codelahoma-search-previous-result ()
  "Jump to previous result."
  (interactive)
  (when (re-search-backward "^\\[" nil t)
    (beginning-of-line)))

(defun codelahoma-search-help ()
  "Show search help."
  (interactive)
  (message "Search keys: [g]refresh [s]new-search [S]advanced [n]ext [p]revious [q]uit"))

;;; Initialize

(add-hook 'after-init-hook #'codelahoma-search-load-saved)

(provide 'codelahoma-unified-search)
;;; codelahoma-unified-search.el ends here