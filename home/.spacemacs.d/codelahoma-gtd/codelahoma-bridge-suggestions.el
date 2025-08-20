;;; codelahoma-bridge-suggestions.el --- Smart knowledge suggestions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org-roam "2.0"))

;;; Commentary:
;; Intelligent suggestions for connecting tasks with relevant knowledge.
;; Provides context-aware recommendations and learning from user behavior.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)
(require 'codelahoma-bridge)
(require 'org-roam)
(require 'cl-lib)

;;; Configuration

(defcustom codelahoma-bridge-suggestion-threshold 0.3
  "Minimum relevance score for suggestions (0.0-1.0)."
  :type 'float
  :group 'codelahoma-gtd)

(defcustom codelahoma-bridge-max-suggestions 10
  "Maximum number of suggestions to show."
  :type 'integer
  :group 'codelahoma-gtd)

(defcustom codelahoma-bridge-auto-suggest nil
  "Enable automatic suggestions when navigating tasks."
  :type 'boolean
  :group 'codelahoma-gtd)

(defcustom codelahoma-bridge-suggestion-weights
  '((title . 0.4)
    (tags . 0.3)
    (recent . 0.2)
    (frequency . 0.1))
  "Weights for different suggestion factors."
  :type '(alist :key-type symbol :value-type float)
  :group 'codelahoma-gtd)

;;; Suggestion Engine

(defvar codelahoma-bridge-suggestion-cache nil
  "Cache for knowledge suggestions.")

(defvar codelahoma-bridge-auto-suggest-timer nil
  "Timer for automatic suggestions.")

(defvar codelahoma-bridge-interaction-history nil
  "History of user interactions with suggestions.")

(defun codelahoma-bridge-suggest-related-knowledge ()
  "Suggest knowledge notes related to current context."
  (interactive)
  (let* ((context (codelahoma-bridge-analyze-context))
         (suggestions (codelahoma-bridge-find-suggestions context))
         (scored (codelahoma-bridge-score-suggestions suggestions context))
         (filtered (codelahoma-bridge-filter-suggestions scored)))
    (if filtered
        (codelahoma-bridge-show-suggestions filtered)
      (message "No relevant knowledge suggestions found"))))

(defun codelahoma-bridge-analyze-context ()
  "Analyze current context for suggestions."
  (let ((context (make-hash-table :test 'equal)))
    ;; Current heading
    (when (org-get-heading)
      (puthash 'heading (org-get-heading t t t t) context)
      (puthash 'keywords (codelahoma-bridge-extract-keywords 
                          (org-get-heading t t t t)) context))
    
    ;; Tags
    (when (org-get-tags)
      (puthash 'tags (org-get-tags) context))
    
    ;; Current project
    (when (codelahoma-gtd-current-project)
      (puthash 'project (codelahoma-gtd-current-project) context))
    
    ;; Recent notes accessed
    (puthash 'recent (codelahoma-bridge-get-recent-notes 5) context)
    
    ;; Task state and properties
    (when (org-entry-get nil "TODO")
      (puthash 'todo-state (org-entry-get nil "TODO") context))
    
    ;; Context from properties
    (when (org-entry-get nil "CONTEXT")
      (puthash 'task-context (org-entry-get nil "CONTEXT") context))
    
    ;; Body text (first 200 chars)
    (save-excursion
      (org-end-of-meta-data)
      (let ((start (point))
            (end (min (+ (point) 200) (org-end-of-subtree t))))
        (puthash 'body (buffer-substring-no-properties start end) context)))
    
    context))

(defun codelahoma-bridge-extract-keywords (text)
  "Extract keywords from TEXT."
  (let* ((words (split-string (downcase text) "[^a-z0-9]+" t))
         (stopwords '("the" "a" "an" "and" "or" "but" "in" "on" "at" 
                     "to" "for" "of" "with" "by" "from" "as" "is"
                     "was" "are" "were" "been" "be" "have" "has"
                     "had" "do" "does" "did" "will" "would" "could"
                     "should" "may" "might" "must" "shall" "can"))
         (keywords (cl-remove-if 
                   (lambda (word) 
                     (or (member word stopwords)
                         (< (length word) 3)))
                   words)))
    keywords))

(defun codelahoma-bridge-find-suggestions (context)
  "Find knowledge suggestions based on CONTEXT."
  (let ((suggestions '())
        (all-nodes (org-roam-node-list)))
    
    ;; Keyword matching
    (when-let ((keywords (gethash 'keywords context)))
      (dolist (node all-nodes)
        (let ((node-title (downcase (org-roam-node-title node)))
              (match-count 0))
          (dolist (keyword keywords)
            (when (string-match-p (regexp-quote keyword) node-title)
              (cl-incf match-count)))
          (when (> match-count 0)
            (push (cons node match-count) suggestions)))))
    
    ;; Tag matching
    (when-let ((tags (gethash 'tags context)))
      (dolist (node all-nodes)
        (let ((node-tags (org-roam-node-tags node))
              (match-count 0))
          (dolist (tag tags)
            (when (member tag node-tags)
              (cl-incf match-count)))
          (when (> match-count 0)
            (let ((existing (assoc node suggestions)))
              (if existing
                  (setcdr existing (+ (cdr existing) match-count))
                (push (cons node match-count) suggestions)))))))
    
    ;; Project-related notes
    (when-let ((project (gethash 'project context)))
      (dolist (node all-nodes)
        (with-temp-buffer
          (when (file-exists-p (org-roam-node-file node))
            (insert-file-contents (org-roam-node-file node))
            (when (search-forward project nil t)
              (let ((existing (assoc node suggestions)))
                (if existing
                    (setcdr existing (+ (cdr existing) 2))
                  (push (cons node 2) suggestions))))))))
    
    ;; Remove duplicates and return
    (cl-remove-duplicates suggestions :key #'car)))

(defun codelahoma-bridge-score-suggestions (suggestions context)
  "Score SUGGESTIONS based on relevance to CONTEXT."
  (mapcar 
   (lambda (suggestion)
     (let* ((node (car suggestion))
            (base-score (cdr suggestion))
            (title-score (codelahoma-bridge-score-title node context))
            (recency-score (codelahoma-bridge-score-recency node))
            (frequency-score (codelahoma-bridge-score-frequency node))
            (tag-score (codelahoma-bridge-score-tags node context))
            (weights codelahoma-bridge-suggestion-weights)
            (total-score (+ (* base-score 0.3)
                           (* title-score (alist-get 'title weights))
                           (* tag-score (alist-get 'tags weights))
                           (* recency-score (alist-get 'recent weights))
                           (* frequency-score (alist-get 'frequency weights)))))
       (list node total-score 
             :title title-score
             :tags tag-score
             :recent recency-score
             :frequency frequency-score)))
   suggestions))

(defun codelahoma-bridge-score-title (node context)
  "Score NODE title relevance to CONTEXT."
  (let* ((title (downcase (org-roam-node-title node)))
         (heading (downcase (or (gethash 'heading context) "")))
         (keywords (gethash 'keywords context))
         (score 0))
    ;; Direct substring match
    (when (and (> (length heading) 3)
               (string-match-p (regexp-quote heading) title))
      (setq score 1.0))
    ;; Keyword matches
    (when keywords
      (let ((match-count 0))
        (dolist (keyword keywords)
          (when (string-match-p (regexp-quote keyword) title)
            (cl-incf match-count)))
        (setq score (max score (/ (float match-count) (length keywords))))))
    score))

(defun codelahoma-bridge-score-tags (node context)
  "Score NODE tags relevance to CONTEXT."
  (let* ((node-tags (org-roam-node-tags node))
         (context-tags (gethash 'tags context))
         (score 0))
    (when (and node-tags context-tags)
      (let ((matches (cl-intersection node-tags context-tags :test #'string=)))
        (setq score (/ (float (length matches)) 
                      (max (length node-tags) (length context-tags))))))
    score))

(defun codelahoma-bridge-score-recency (node)
  "Score NODE based on recency of access."
  (let* ((file (org-roam-node-file node))
         (access-time (nth 4 (file-attributes file)))
         (days-ago (/ (float-time (time-subtract (current-time) access-time))
                     (* 24 60 60))))
    (cond
     ((< days-ago 1) 1.0)
     ((< days-ago 7) 0.7)
     ((< days-ago 30) 0.4)
     (t 0.1))))

(defun codelahoma-bridge-score-frequency (node)
  "Score NODE based on access frequency."
  (let ((interactions (cl-count-if 
                      (lambda (h) (equal (car h) (org-roam-node-id node)))
                      codelahoma-bridge-interaction-history)))
    (min 1.0 (/ interactions 10.0))))

(defun codelahoma-bridge-filter-suggestions (scored-suggestions)
  "Filter SCORED-SUGGESTIONS by threshold and limit."
  (let* ((threshold codelahoma-bridge-suggestion-threshold)
         (filtered (cl-remove-if 
                   (lambda (s) (< (cadr s) threshold))
                   scored-suggestions))
         (sorted (sort filtered (lambda (a b) (> (cadr a) (cadr b)))))
         (limited (seq-take sorted codelahoma-bridge-max-suggestions)))
    limited))

;;; Suggestion Display

(defun codelahoma-bridge-show-suggestions (suggestions)
  "Display SUGGESTIONS in a buffer."
  (let ((buffer (get-buffer-create "*Knowledge Suggestions*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Knowledge Suggestions\n\n")
      (insert (format "Context: %s\n\n" 
                     (or (org-get-heading t t t t) "Current location")))
      
      (if suggestions
          (progn
            (insert "* Suggested Knowledge Notes\n\n")
            (dolist (suggestion suggestions)
              (let* ((node (car suggestion))
                     (score (cadr suggestion))
                     (details (cddr suggestion)))
                (insert (format "** [[id:%s][%s]] (%.2f)\n" 
                               (org-roam-node-id node)
                               (org-roam-node-title node)
                               score))
                (when (org-roam-node-tags node)
                  (insert "   Tags: " 
                         (string-join (org-roam-node-tags node) ", ") "\n"))
                (insert "   Relevance: ")
                (insert (format "Title: %.2f | " (plist-get details :title)))
                (insert (format "Tags: %.2f | " (plist-get details :tags)))
                (insert (format "Recent: %.2f | " (plist-get details :recent)))
                (insert (format "Frequency: %.2f\n" (plist-get details :frequency)))
                (insert "\n"))))
        (insert "No relevant suggestions found.\n"))
      
      (insert "\n* Actions\n")
      (insert "- Press RET on a link to open the note\n")
      (insert "- Press 'l' on a link to create a task-note link\n")
      (insert "- Press 'r' to refresh suggestions\n")
      (insert "- Press 'q' to close this buffer\n")
      
      (goto-char (point-min))
      (forward-line 3)
      (read-only-mode 1)
      
      ;; Set up keybindings
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key "l" 'codelahoma-bridge-link-from-suggestion)
      (local-set-key "r" 'codelahoma-bridge-refresh-suggestions)
      (local-set-key "q" 'quit-window))
    
    (pop-to-buffer buffer)))

(defun codelahoma-bridge-link-from-suggestion ()
  "Create a link from the suggestion at point."
  (interactive)
  (when-let ((link (org-element-context))
             (id (when (eq (org-element-type link) 'link)
                   (org-element-property :path link))))
    (when (string-prefix-p "id:" (org-element-property :raw-link link))
      (let ((node (org-roam-node-from-id id)))
        (when node
          ;; Record interaction
          (push (cons id (current-time)) 
                codelahoma-bridge-interaction-history)
          ;; Switch back to original buffer and create link
          (other-window -1)
          (org-entry-put nil codelahoma-bridge-link-property id)
          (message "Linked to: %s" (org-roam-node-title node)))))))

(defun codelahoma-bridge-refresh-suggestions ()
  "Refresh the suggestions buffer."
  (interactive)
  (codelahoma-bridge-suggest-related-knowledge))

;;; Learning and Adaptation

(defun codelahoma-bridge-learn-from-link (task-context note-id)
  "Learn from user creating a link between TASK-CONTEXT and NOTE-ID."
  (push (list :action 'link
             :context task-context
             :note note-id
             :time (current-time))
        codelahoma-bridge-interaction-history)
  (codelahoma-bridge-save-learning-data))

(defun codelahoma-bridge-learn-from-rejection (task-context suggested-notes)
  "Learn from user rejecting SUGGESTED-NOTES for TASK-CONTEXT."
  (push (list :action 'reject
             :context task-context
             :notes suggested-notes
             :time (current-time))
        codelahoma-bridge-interaction-history)
  (codelahoma-bridge-save-learning-data))

(defun codelahoma-bridge-save-learning-data ()
  "Save learning data to file."
  (let ((file (expand-file-name "suggestion-history.el" 
                               codelahoma-gtd-directory)))
    (with-temp-file file
      (print codelahoma-bridge-interaction-history (current-buffer)))))

(defun codelahoma-bridge-load-learning-data ()
  "Load learning data from file."
  (let ((file (expand-file-name "suggestion-history.el" 
                               codelahoma-gtd-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (setq codelahoma-bridge-interaction-history
              (read (current-buffer)))))))

;;; Auto-suggest Mode

(defun codelahoma-bridge-auto-suggest-mode (&optional arg)
  "Toggle automatic knowledge suggestions.
With prefix ARG, enable if positive, disable otherwise."
  (interactive "P")
  (setq codelahoma-bridge-auto-suggest
        (if arg
            (> (prefix-numeric-value arg) 0)
          (not codelahoma-bridge-auto-suggest)))
  
  (if codelahoma-bridge-auto-suggest
      (progn
        (when codelahoma-bridge-auto-suggest-timer
          (cancel-timer codelahoma-bridge-auto-suggest-timer))
        (setq codelahoma-bridge-auto-suggest-timer
              (run-with-idle-timer 2 t 'codelahoma-bridge-check-suggestions))
        (message "Auto-suggest enabled"))
    (when codelahoma-bridge-auto-suggest-timer
      (cancel-timer codelahoma-bridge-auto-suggest-timer)
      (setq codelahoma-bridge-auto-suggest-timer nil))
    (message "Auto-suggest disabled")))

(defun codelahoma-bridge-check-suggestions ()
  "Check if suggestions should be shown for current context."
  (when (and codelahoma-bridge-auto-suggest
             (derived-mode-p 'org-mode)
             (org-entry-get nil "TODO")
             (not (org-entry-get nil codelahoma-bridge-link-property)))
    (let* ((context (codelahoma-bridge-analyze-context))
           (suggestions (codelahoma-bridge-find-suggestions context)))
      (when suggestions
        (message "Knowledge suggestions available. Press %s to view."
                (substitute-command-keys "\\[codelahoma-bridge-suggest-related-knowledge]"))))))

;;; Recent Notes Tracking

(defun codelahoma-bridge-get-recent-notes (limit)
  "Get LIMIT most recently accessed org-roam notes."
  (let* ((all-nodes (org-roam-node-list))
         (with-times (mapcar 
                     (lambda (node)
                       (cons node (nth 4 (file-attributes 
                                        (org-roam-node-file node)))))
                     all-nodes))
         (sorted (sort with-times 
                      (lambda (a b) 
                        (time-less-p (cdr b) (cdr a)))))
         (limited (seq-take sorted limit)))
    (mapcar #'car limited)))

;;; Suggestion Presets

(defun codelahoma-bridge-suggest-for-project ()
  "Suggest knowledge notes for current project."
  (interactive)
  (unless (codelahoma-gtd-project-p)
    (user-error "Not in a project"))
  (let ((context (make-hash-table :test 'equal)))
    (puthash 'project (org-get-heading t t t t) context)
    (puthash 'tags (cons "project" (org-get-tags)) context)
    (let* ((suggestions (codelahoma-bridge-find-suggestions context))
           (scored (codelahoma-bridge-score-suggestions suggestions context))
           (filtered (codelahoma-bridge-filter-suggestions scored)))
      (if filtered
          (codelahoma-bridge-show-suggestions filtered)
        (message "No knowledge notes found for this project")))))

(defun codelahoma-bridge-suggest-by-context ()
  "Suggest knowledge notes based on task context."
  (interactive)
  (let ((task-context (org-entry-get nil "CONTEXT")))
    (unless task-context
      (user-error "No context set for this task"))
    (let ((context (make-hash-table :test 'equal)))
      (puthash 'task-context task-context context)
      (puthash 'keywords (list task-context) context)
      (let* ((suggestions (codelahoma-bridge-find-suggestions context))
             (scored (codelahoma-bridge-score-suggestions suggestions context))
             (filtered (codelahoma-bridge-filter-suggestions scored)))
        (if filtered
            (codelahoma-bridge-show-suggestions filtered)
          (message "No knowledge notes found for context: %s" task-context))))))

;;; Initialize learning data
(add-hook 'after-init-hook 'codelahoma-bridge-load-learning-data)

(provide 'codelahoma-bridge-suggestions)
;;; codelahoma-bridge-suggestions.el ends here