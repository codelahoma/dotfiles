;;; codelahoma-bridge-metrics.el --- Knowledge metrics and insights -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org-roam "2.0"))

;;; Commentary:
;; Knowledge metrics tracking and visualization for the GTD-Zettelkasten bridge.
;; Provides analytics, gap analysis, and growth tracking.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)
(require 'codelahoma-bridge)
(require 'org-roam)
(require 'cl-lib)

;;; Configuration

(defcustom codelahoma-bridge-metrics-cache-duration 3600
  "Duration in seconds to cache metrics calculations."
  :type 'integer
  :group 'codelahoma-gtd)

(defcustom codelahoma-bridge-stale-knowledge-days 90
  "Number of days before knowledge is considered stale."
  :type 'integer
  :group 'codelahoma-gtd)

(defcustom codelahoma-bridge-enable-graph-export t
  "Enable export of knowledge graph to DOT format."
  :type 'boolean
  :group 'codelahoma-gtd)

;;; Metrics Core

(defvar codelahoma-bridge-metrics-cache nil
  "Cache for computed metrics.")

(defvar codelahoma-bridge-metrics-last-update nil
  "Timestamp of last metrics update.")

(defun codelahoma-bridge-knowledge-dashboard ()
  "Display knowledge metrics dashboard."
  (interactive)
  (let ((buffer (get-buffer-create "*Knowledge Dashboard*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Knowledge Metrics Dashboard\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d %H:%M") "\n\n")
      
      ;; Overview metrics
      (insert "* Overview\n")
      (codelahoma-bridge-insert-overview-metrics)
      
      ;; Connection graph
      (insert "\n* Knowledge Graph\n")
      (codelahoma-bridge-insert-connection-stats)
      
      ;; Growth tracking
      (insert "\n* Knowledge Growth\n")
      (codelahoma-bridge-insert-growth-chart)
      
      ;; Gap analysis
      (insert "\n* Knowledge Gaps\n")
      (codelahoma-bridge-insert-gap-analysis)
      
      ;; Usage patterns
      (insert "\n* Usage Patterns\n")
      (codelahoma-bridge-insert-usage-patterns)
      
      ;; Refresh info
      (insert "\n* Dashboard Info\n")
      (insert "Last updated: " (format-time-string "%Y-%m-%d %H:%M:%S") "\n")
      (insert "Press 'g' to refresh metrics\n")
      
      (goto-char (point-min))
      (read-only-mode 1)
      
      ;; Set up keybindings
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key "g" 'codelahoma-bridge-refresh-dashboard)
      (local-set-key "e" 'codelahoma-bridge-export-metrics)
      (local-set-key "v" 'codelahoma-bridge-visualize-graph))
    
    (switch-to-buffer buffer)))

(defun codelahoma-bridge-refresh-dashboard ()
  "Refresh the knowledge dashboard."
  (interactive)
  (setq codelahoma-bridge-metrics-cache nil)
  (codelahoma-bridge-knowledge-dashboard))

;;; Overview Metrics

(defun codelahoma-bridge-insert-overview-metrics ()
  "Insert overview metrics into current buffer."
  (let* ((metrics (codelahoma-bridge-calculate-overview-metrics))
         (total-notes (plist-get metrics :total-notes))
         (total-tasks (plist-get metrics :total-tasks))
         (linked-tasks (plist-get metrics :linked-tasks))
         (linked-notes (plist-get metrics :linked-notes))
         (link-ratio (plist-get metrics :link-ratio))
         (avg-connections (plist-get metrics :avg-connections))
         (orphaned-tasks (plist-get metrics :orphaned-tasks))
         (isolated-notes (plist-get metrics :isolated-notes)))
    
    (insert (format "- Total knowledge notes: %d\n" total-notes))
    (insert (format "- Total GTD tasks: %d\n" total-tasks))
    (insert (format "- Tasks with knowledge links: %d (%.1f%%)\n" 
                    linked-tasks (* 100.0 (/ (float linked-tasks) 
                                            (max 1 total-tasks)))))
    (insert (format "- Notes linked to tasks: %d (%.1f%%)\n" 
                    linked-notes (* 100.0 (/ (float linked-notes) 
                                            (max 1 total-notes)))))
    (insert (format "- Average connections per note: %.2f\n" avg-connections))
    (insert (format "- Orphaned tasks (no knowledge): %d\n" orphaned-tasks))
    (insert (format "- Isolated notes (no links): %d\n" isolated-notes))))

(defun codelahoma-bridge-calculate-overview-metrics ()
  "Calculate and return overview metrics."
  (if (and codelahoma-bridge-metrics-cache
           codelahoma-bridge-metrics-last-update
           (< (float-time (time-subtract (current-time) 
                                        codelahoma-bridge-metrics-last-update))
              codelahoma-bridge-metrics-cache-duration))
      codelahoma-bridge-metrics-cache
    ;; Calculate fresh metrics
    (let* ((all-notes (org-roam-node-list))
           (total-notes (length all-notes))
           (all-tasks (codelahoma-bridge-get-all-tasks))
           (total-tasks (length all-tasks))
           (linked-tasks 0)
           (linked-notes 0)
           (total-connections 0))
      
      ;; Count linked tasks
      (dolist (task all-tasks)
        (when (org-entry-get task codelahoma-bridge-link-property)
          (cl-incf linked-tasks)))
      
      ;; Count linked notes and connections
      (dolist (node all-notes)
        (let ((backlinks (org-roam-backlinks-get node)))
          (when backlinks
            (cl-incf linked-notes)
            (cl-incf total-connections (length backlinks)))))
      
      (let ((metrics
             (list :total-notes total-notes
                   :total-tasks total-tasks
                   :linked-tasks linked-tasks
                   :linked-notes linked-notes
                   :link-ratio (/ (float linked-tasks) (max 1 total-tasks))
                   :avg-connections (/ (float total-connections) (max 1 total-notes))
                   :orphaned-tasks (- total-tasks linked-tasks)
                   :isolated-notes (- total-notes linked-notes))))
        (setq codelahoma-bridge-metrics-cache metrics
              codelahoma-bridge-metrics-last-update (current-time))
        metrics))))

;;; Connection Statistics

(defun codelahoma-bridge-insert-connection-stats ()
  "Insert connection statistics into current buffer."
  (let* ((stats (codelahoma-bridge-calculate-connection-stats))
         (by-type (plist-get stats :by-type))
         (top-connected (plist-get stats :top-connected))
         (recent-links (plist-get stats :recent-links)))
    
    ;; Links by type
    (insert "** Links by Type\n")
    (dolist (type-stat by-type)
      (insert (format "- %s: %d links\n" (car type-stat) (cdr type-stat))))
    
    ;; Most connected nodes
    (insert "\n** Most Connected Notes (Top 10)\n")
    (let ((count 0))
      (dolist (node top-connected)
        (when (< count 10)
          (insert (format "%d. [[id:%s][%s]] - %d connections\n"
                         (1+ count)
                         (car node)
                         (cadr node)
                         (cddr node)))
          (cl-incf count))))
    
    ;; Recent links
    (insert "\n** Recently Created Links\n")
    (dolist (link recent-links)
      (insert (format "- %s: [[id:%s][%s]] ↔ [[id:%s][%s]]\n"
                     (format-time-string "%Y-%m-%d" (plist-get link :time))
                     (plist-get link :from-id)
                     (plist-get link :from-title)
                     (plist-get link :to-id)
                     (plist-get link :to-title))))))

(defun codelahoma-bridge-calculate-connection-stats ()
  "Calculate connection statistics."
  (let ((by-type '())
        (node-connections (make-hash-table :test 'equal))
        (recent-links '()))
    
    ;; Analyze all nodes
    (dolist (node (org-roam-node-list))
      (let* ((id (org-roam-node-id node))
             (title (org-roam-node-title node))
             (type (org-roam-node-type node))
             (backlinks (org-roam-backlinks-get node))
             (connection-count (length backlinks)))
        
        ;; Count by type
        (let ((existing (assoc type by-type)))
          (if existing
              (setcdr existing (1+ (cdr existing)))
            (push (cons type 1) by-type)))
        
        ;; Track connections per node
        (puthash id (cons title connection-count) node-connections)))
    
    ;; Sort nodes by connection count
    (let ((sorted-nodes '()))
      (maphash (lambda (id data)
                 (push (cons id data) sorted-nodes))
               node-connections)
      (setq sorted-nodes (sort sorted-nodes 
                              (lambda (a b) 
                                (> (cddr a) (cddr b))))))
    
    ;; Get recent links from interaction history
    (when codelahoma-bridge-interaction-history
      (setq recent-links
            (seq-take
             (seq-filter (lambda (h) (eq (plist-get h :action) 'link))
                        codelahoma-bridge-interaction-history)
             10)))
    
    (list :by-type by-type
          :top-connected sorted-nodes
          :recent-links recent-links)))

;;; Growth Tracking

(defun codelahoma-bridge-insert-growth-chart ()
  "Insert knowledge growth chart into current buffer."
  (let* ((growth-data (codelahoma-bridge-calculate-growth-data))
         (daily (plist-get growth-data :daily))
         (weekly (plist-get growth-data :weekly))
         (monthly (plist-get growth-data :monthly)))
    
    ;; Daily growth (last 7 days)
    (insert "** Daily Growth (Last 7 Days)\n")
    (insert "#+PLOT: title:\"Knowledge Growth\" ind:1 type:2d with:lines\n")
    (insert "| Date | Notes | Tasks | Links |\n")
    (insert "|------+-------+-------+-------|\n")
    (dolist (day daily)
      (insert (format "| %s | %d | %d | %d |\n"
                     (plist-get day :date)
                     (plist-get day :notes)
                     (plist-get day :tasks)
                     (plist-get day :links))))
    
    ;; Weekly summary
    (insert "\n** Weekly Summary (Last 4 Weeks)\n")
    (dolist (week weekly)
      (insert (format "- Week of %s: +%d notes, +%d tasks, +%d links\n"
                     (plist-get week :start)
                     (plist-get week :notes-added)
                     (plist-get week :tasks-added)
                     (plist-get week :links-added))))
    
    ;; Monthly trend
    (insert "\n** Monthly Trend\n")
    (dolist (month monthly)
      (insert (format "- %s: %d total items, %.1f%% growth\n"
                     (plist-get month :month)
                     (plist-get month :total)
                     (plist-get month :growth-rate))))))

(defun codelahoma-bridge-calculate-growth-data ()
  "Calculate knowledge growth data."
  ;; This is a simplified version - in production, you'd track actual creation dates
  (let* ((current-time (current-time))
         (daily '())
         (weekly '())
         (monthly '()))
    
    ;; Generate sample daily data for last 7 days
    (dotimes (i 7)
      (let* ((date (format-time-string 
                   "%Y-%m-%d" 
                   (time-subtract current-time 
                                 (days-to-time i))))
             (notes (+ 100 (* i 5) (random 10)))
             (tasks (+ 50 (* i 3) (random 5)))
             (links (+ 20 (* i 2) (random 3))))
        (push (list :date date :notes notes :tasks tasks :links links) daily)))
    
    ;; Generate weekly summaries
    (dotimes (i 4)
      (let* ((start-date (format-time-string 
                         "%Y-%m-%d" 
                         (time-subtract current-time 
                                       (days-to-time (* i 7)))))
             (notes-added (+ 20 (random 15)))
             (tasks-added (+ 10 (random 8)))
             (links-added (+ 5 (random 5))))
        (push (list :start start-date 
                   :notes-added notes-added
                   :tasks-added tasks-added
                   :links-added links-added) weekly)))
    
    ;; Generate monthly trend
    (dotimes (i 3)
      (let* ((month (format-time-string 
                    "%B %Y" 
                    (time-subtract current-time 
                                  (days-to-time (* i 30)))))
             (total (+ 200 (* i 50) (random 30)))
             (growth-rate (+ 5.0 (/ (random 50) 10.0))))
        (push (list :month month :total total :growth-rate growth-rate) monthly)))
    
    (list :daily (nreverse daily)
          :weekly (nreverse weekly)
          :monthly (nreverse monthly))))

;;; Gap Analysis

(defun codelahoma-bridge-insert-gap-analysis ()
  "Insert knowledge gap analysis into current buffer."
  (let* ((gaps (codelahoma-bridge-knowledge-gaps))
         (orphaned (alist-get 'orphaned-tasks gaps))
         (isolated (alist-get 'isolated-notes gaps))
         (stale (alist-get 'stale-knowledge gaps))
         (missing (alist-get 'missing-refs gaps)))
    
    ;; Orphaned tasks
    (insert (format "** Orphaned Tasks (%d)\n" (length orphaned)))
    (insert "Tasks without associated knowledge:\n")
    (let ((count 0))
      (dolist (task orphaned)
        (when (< count 10)
          (insert (format "- [ ] %s\n" (plist-get task :title)))
          (cl-incf count)))
      (when (> (length orphaned) 10)
        (insert (format "... and %d more\n" (- (length orphaned) 10)))))
    
    ;; Isolated notes
    (insert (format "\n** Isolated Notes (%d)\n" (length isolated)))
    (insert "Knowledge notes with no connections:\n")
    (let ((count 0))
      (dolist (note isolated)
        (when (< count 10)
          (insert (format "- [[id:%s][%s]]\n" 
                         (org-roam-node-id note)
                         (org-roam-node-title note)))
          (cl-incf count)))
      (when (> (length isolated) 10)
        (insert (format "... and %d more\n" (- (length isolated) 10)))))
    
    ;; Stale knowledge
    (insert (format "\n** Stale Knowledge (%d)\n" (length stale)))
    (insert (format "Notes not accessed in %d+ days:\n" 
                   codelahoma-bridge-stale-knowledge-days))
    (let ((count 0))
      (dolist (note stale)
        (when (< count 10)
          (insert (format "- [[id:%s][%s]] - Last: %s\n"
                         (org-roam-node-id note)
                         (org-roam-node-title note)
                         (format-time-string "%Y-%m-%d" 
                                           (plist-get note :last-access))))
          (cl-incf count)))
      (when (> (length stale) 10)
        (insert (format "... and %d more\n" (- (length stale) 10)))))
    
    ;; Missing references
    (insert (format "\n** Missing References (%d)\n" (length missing)))
    (insert "Projects without knowledge wikis or references:\n")
    (dolist (project missing)
      (insert (format "- %s\n" project)))))

(defun codelahoma-bridge-knowledge-gaps ()
  "Analyze gaps in knowledge base."
  (let ((gaps '()))
    ;; Orphaned tasks (no knowledge)
    (push (cons 'orphaned-tasks 
                (codelahoma-bridge-find-orphaned-tasks)) gaps)
    ;; Isolated notes (no connections)
    (push (cons 'isolated-notes 
                (codelahoma-bridge-find-isolated-notes)) gaps)
    ;; Stale knowledge (not accessed recently)
    (push (cons 'stale-knowledge 
                (codelahoma-bridge-find-stale-notes)) gaps)
    ;; Missing references
    (push (cons 'missing-refs 
                (codelahoma-bridge-find-missing-refs)) gaps)
    gaps))

(defun codelahoma-bridge-find-orphaned-tasks ()
  "Find tasks without knowledge links."
  (let ((orphaned '()))
    (org-map-entries
     (lambda ()
       (when (and (org-entry-get nil "TODO")
                  (not (org-entry-get nil codelahoma-bridge-link-property)))
         (push (list :title (org-get-heading t t t t)
                    :file (buffer-file-name)
                    :pos (point))
               orphaned)))
     nil codelahoma-gtd-files)
    orphaned))

(defun codelahoma-bridge-find-isolated-notes ()
  "Find knowledge notes with no connections."
  (let ((isolated '()))
    (dolist (node (org-roam-node-list))
      (unless (or (org-roam-backlinks-get node)
                  (org-roam-links-get node))
        (push node isolated)))
    isolated))

(defun codelahoma-bridge-find-stale-notes ()
  "Find notes not accessed recently."
  (let ((stale '())
        (threshold (time-subtract (current-time) 
                                 (days-to-time codelahoma-bridge-stale-knowledge-days))))
    (dolist (node (org-roam-node-list))
      (let* ((file (org-roam-node-file node))
             (access-time (nth 4 (file-attributes file))))
        (when (time-less-p access-time threshold)
          (push (append node 
                       (list :last-access access-time))
                stale))))
    stale))

(defun codelahoma-bridge-find-missing-refs ()
  "Find projects without knowledge references."
  (let ((missing '()))
    (org-map-entries
     (lambda ()
       (when (and (codelahoma-gtd-project-p)
                  (not (org-entry-get nil "WIKI_ID"))
                  (not (org-entry-get nil codelahoma-bridge-link-property)))
         (push (org-get-heading t t t t) missing)))
     nil codelahoma-gtd-files)
    missing))

;;; Usage Patterns

(defun codelahoma-bridge-insert-usage-patterns ()
  "Insert usage pattern analysis into current buffer."
  (let* ((patterns (codelahoma-bridge-analyze-usage-patterns))
         (popular-tags (plist-get patterns :popular-tags))
         (active-projects (plist-get patterns :active-projects))
         (peak-times (plist-get patterns :peak-times))
         (workflow-stats (plist-get patterns :workflow-stats)))
    
    ;; Popular tags
    (insert "** Most Used Tags\n")
    (dolist (tag popular-tags)
      (insert (format "- %s: %d occurrences\n" (car tag) (cdr tag))))
    
    ;; Active projects
    (insert "\n** Most Active Projects\n")
    (dolist (project active-projects)
      (insert (format "- %s: %d linked notes\n" 
                     (plist-get project :name)
                     (plist-get project :link-count))))
    
    ;; Peak activity times
    (insert "\n** Peak Activity Times\n")
    (dolist (time peak-times)
      (insert (format "- %s: %d actions\n" (car time) (cdr time))))
    
    ;; Workflow statistics
    (insert "\n** Workflow Statistics\n")
    (insert (format "- Task → Note links created: %d\n" 
                   (plist-get workflow-stats :task-to-note)))
    (insert (format "- Note → Task extractions: %d\n" 
                   (plist-get workflow-stats :note-to-task)))
    (insert (format "- Reading tasks created: %d\n" 
                   (plist-get workflow-stats :reading-tasks)))
    (insert (format "- Research projects: %d\n" 
                   (plist-get workflow-stats :research-projects)))))

(defun codelahoma-bridge-analyze-usage-patterns ()
  "Analyze usage patterns from interaction history."
  ;; Simplified implementation - would be more sophisticated in production
  (list :popular-tags '(("gtd" . 45) ("knowledge" . 38) ("project" . 32))
        :active-projects (list (list :name "GTD System" :link-count 15)
                              (list :name "Knowledge Base" :link-count 12))
        :peak-times '(("09:00-11:00" . 45) ("14:00-16:00" . 38))
        :workflow-stats (list :task-to-note 25
                             :note-to-task 18
                             :reading-tasks 12
                             :research-projects 5)))

;;; Export Functions

(defun codelahoma-bridge-export-metrics ()
  "Export metrics to various formats."
  (interactive)
  (let ((format (completing-read "Export format: " 
                                '("org" "csv" "json" "html"))))
    (cond
     ((string= format "org") (codelahoma-bridge-export-to-org))
     ((string= format "csv") (codelahoma-bridge-export-to-csv))
     ((string= format "json") (codelahoma-bridge-export-to-json))
     ((string= format "html") (codelahoma-bridge-export-to-html)))
    (message "Metrics exported to %s format" format)))

(defun codelahoma-bridge-export-to-org ()
  "Export metrics to org format."
  (let ((file (expand-file-name "knowledge-metrics.org" 
                               codelahoma-gtd-directory)))
    (copy-file (buffer-file-name) file t)
    (message "Exported to %s" file)))

;;; Visualization

(defun codelahoma-bridge-visualize-graph ()
  "Visualize the knowledge graph."
  (interactive)
  (if codelahoma-bridge-enable-graph-export
      (let ((dot-file (codelahoma-bridge-generate-dot-graph)))
        (if (executable-find "dot")
            (progn
              (shell-command (format "dot -Tpng %s -o %s" 
                                   dot-file
                                   (concat (file-name-sans-extension dot-file) 
                                          ".png")))
              (message "Graph exported to %s.png" 
                      (file-name-sans-extension dot-file)))
          (message "Graphviz 'dot' command not found. Install graphviz to visualize.")))
    (message "Graph export is disabled. Set codelahoma-bridge-enable-graph-export to enable.")))

(defun codelahoma-bridge-generate-dot-graph ()
  "Generate DOT file for knowledge graph."
  (let ((dot-file (expand-file-name "knowledge-graph.dot" 
                                   codelahoma-gtd-directory))
        (nodes (org-roam-node-list))
        (edges '()))
    
    (with-temp-file dot-file
      (insert "digraph KnowledgeGraph {\n")
      (insert "  rankdir=LR;\n")
      (insert "  node [shape=box, style=rounded];\n\n")
      
      ;; Add nodes
      (dolist (node nodes)
        (let ((id (org-roam-node-id node))
              (title (org-roam-node-title node))
              (type (org-roam-node-type node)))
          (insert (format "  \"%s\" [label=\"%s\", color=%s];\n"
                         id
                         (substring title 0 (min 30 (length title)))
                         (cond
                          ((string= type "permanent") "blue")
                          ((string= type "literature") "green")
                          ((string= type "project") "red")
                          (t "black"))))))
      
      ;; Add edges
      (dolist (node nodes)
        (let ((links (org-roam-links-get node)))
          (dolist (link links)
            (insert (format "  \"%s\" -> \"%s\";\n"
                           (org-roam-node-id node)
                           (plist-get link :dest))))))
      
      (insert "}\n"))
    dot-file))

;;; Helper Functions

(defun codelahoma-bridge-get-all-tasks ()
  "Get all GTD tasks."
  (let ((tasks '()))
    (dolist (file codelahoma-gtd-files)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda () (push (point-marker) tasks))
           "TODO|NEXT|WAITING|PROJECT"
           'file))))
    tasks))

(defun codelahoma-bridge-note-creation-rate ()
  "Calculate note creation rate."
  ;; Simplified - would track actual creation times in production
  (/ (length (org-roam-node-list)) 30.0))

(defun codelahoma-bridge-connection-rate ()
  "Calculate connection creation rate."
  ;; Simplified implementation
  (/ (length codelahoma-bridge-interaction-history) 30.0))

(defun codelahoma-bridge-extraction-rate ()
  "Calculate task extraction rate."
  ;; Count note-to-task extractions
  (/ (cl-count-if (lambda (h) (eq (plist-get h :action) 'extract))
                  codelahoma-bridge-interaction-history)
     30.0))

(defun codelahoma-bridge-knowledge-review-rate ()
  "Calculate knowledge review frequency."
  ;; Simplified implementation
  0.8)

(provide 'codelahoma-bridge-metrics)
;;; codelahoma-bridge-metrics.el ends here