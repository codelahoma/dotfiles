;;; codelahoma-gtd-analytics.el --- Review analytics and insights -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Analytics and insights system for GTD reviews.
;; Tracks review completion, identifies patterns, and provides productivity insights.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)

;;; Analytics Database

(defcustom codelahoma-gtd-analytics-file
  (expand-file-name "analytics.el" codelahoma-gtd-directory)
  "File to store analytics data."
  :type 'file
  :group 'codelahoma-gtd)

(defvar codelahoma-gtd-analytics-data nil
  "Analytics data structure.")

(defun codelahoma-gtd-load-analytics ()
  "Load analytics data from file."
  (when (file-exists-p codelahoma-gtd-analytics-file)
    (with-temp-buffer
      (insert-file-contents codelahoma-gtd-analytics-file)
      (goto-char (point-min))
      (condition-case nil
          (setq codelahoma-gtd-analytics-data (read (current-buffer)))
        (error (setq codelahoma-gtd-analytics-data nil))))))

(defun codelahoma-gtd-save-analytics ()
  "Save analytics data to file."
  (with-temp-file codelahoma-gtd-analytics-file
    (print codelahoma-gtd-analytics-data (current-buffer))))

(defun codelahoma-gtd-ensure-analytics-loaded ()
  "Ensure analytics data is loaded."
  (unless codelahoma-gtd-analytics-data
    (codelahoma-gtd-load-analytics))
  (unless codelahoma-gtd-analytics-data
    (setq codelahoma-gtd-analytics-data
          '((reviews . ())
            (tasks . ())
            (patterns . ())
            (streaks . ())))))

;;; Review Tracking

(defun codelahoma-gtd-record-review (type)
  "Record completion of review TYPE."
  (codelahoma-gtd-ensure-analytics-loaded)
  (let* ((date (format-time-string "%Y-%m-%d"))
         (time (format-time-string "%H:%M"))
         (day-of-week (format-time-string "%a"))
         (entry (list :type type 
                     :date date 
                     :time time
                     :day day-of-week
                     :duration nil)))
    (push entry (alist-get 'reviews codelahoma-gtd-analytics-data))
    (codelahoma-gtd-update-streak type)
    (codelahoma-gtd-save-analytics)
    (message "Recorded %s review" type)))

(defun codelahoma-gtd-record-review-duration (type duration)
  "Record DURATION for review TYPE completed today."
  (codelahoma-gtd-ensure-analytics-loaded)
  (let* ((date (format-time-string "%Y-%m-%d"))
         (reviews (alist-get 'reviews codelahoma-gtd-analytics-data))
         (today-review (seq-find (lambda (r) 
                                  (and (eq (plist-get r :type) type)
                                       (string= (plist-get r :date) date)))
                                reviews)))
    (when today-review
      (plist-put today-review :duration duration)
      (codelahoma-gtd-save-analytics))))

;;; Streak Tracking

(defun codelahoma-gtd-update-streak (type)
  "Update streak for review TYPE."
  (let* ((streaks (alist-get 'streaks codelahoma-gtd-analytics-data))
         (streak-data (alist-get type streaks))
         (current-streak (or (plist-get streak-data :current) 0))
         (last-date (plist-get streak-data :last-date))
         (today (format-time-string "%Y-%m-%d")))
    
    (if (or (not last-date)
            (codelahoma-gtd-consecutive-dates-p last-date today type))
        ;; Continue or start streak
        (setf (alist-get type (alist-get 'streaks codelahoma-gtd-analytics-data))
              (list :current (1+ current-streak)
                    :last-date today
                    :longest (max (1+ current-streak) 
                                (or (plist-get streak-data :longest) 0))))
      ;; Break streak
      (setf (alist-get type (alist-get 'streaks codelahoma-gtd-analytics-data))
            (list :current 1
                  :last-date today
                  :longest (or (plist-get streak-data :longest) current-streak))))))

(defun codelahoma-gtd-consecutive-dates-p (date1 date2 type)
  "Check if DATE1 and DATE2 are consecutive for review TYPE."
  (let* ((time1 (date-to-time date1))
         (time2 (date-to-time date2))
         (days-between (- (time-to-days time2) (time-to-days time1))))
    (pcase type
      ('daily (<= days-between 1))
      ('weekly (<= days-between 7))
      ('monthly (<= days-between 31))
      (_ t))))

(defun codelahoma-gtd-review-streak (type)
  "Get current streak for review TYPE."
  (codelahoma-gtd-ensure-analytics-loaded)
  (let* ((streaks (alist-get 'streaks codelahoma-gtd-analytics-data))
         (streak-data (alist-get type streaks)))
    (or (plist-get streak-data :current) 0)))

(defun codelahoma-gtd-longest-streak (type)
  "Get longest streak for review TYPE."
  (codelahoma-gtd-ensure-analytics-loaded)
  (let* ((streaks (alist-get 'streaks codelahoma-gtd-analytics-data))
         (streak-data (alist-get type streaks)))
    (or (plist-get streak-data :longest) 0)))

;;; Task Analytics

(defun codelahoma-gtd-record-task-completion (task-data)
  "Record completion of a task with TASK-DATA."
  (codelahoma-gtd-ensure-analytics-loaded)
  (let ((entry (list :date (format-time-string "%Y-%m-%d")
                    :time (format-time-string "%H:%M")
                    :title (plist-get task-data :title)
                    :context (plist-get task-data :context)
                    :effort (plist-get task-data :effort)
                    :project (plist-get task-data :project))))
    (push entry (alist-get 'tasks codelahoma-gtd-analytics-data))
    (codelahoma-gtd-save-analytics)))

(defun codelahoma-gtd-weekly-velocity ()
  "Calculate average weekly task completion velocity."
  (codelahoma-gtd-ensure-analytics-loaded)
  (let* ((tasks (alist-get 'tasks codelahoma-gtd-analytics-data))
         (four-weeks-ago (time-subtract (current-time) (days-to-time 28)))
         (recent-tasks (seq-filter 
                       (lambda (task)
                         (time-less-p four-weeks-ago 
                                     (date-to-time (plist-get task :date))))
                       tasks))
         (weeks (/ 28.0 7)))
    (if recent-tasks
        (/ (length recent-tasks) weeks)
      0)))

(defun codelahoma-gtd-completion-by-context ()
  "Analyze task completion by context."
  (codelahoma-gtd-ensure-analytics-loaded)
  (let ((tasks (alist-get 'tasks codelahoma-gtd-analytics-data))
        (context-counts (make-hash-table :test 'equal)))
    (dolist (task tasks)
      (let ((context (or (plist-get task :context) "none")))
        (puthash context (1+ (gethash context context-counts 0)) context-counts)))
    context-counts))

(defun codelahoma-gtd-completion-by-hour ()
  "Analyze task completion by hour of day."
  (codelahoma-gtd-ensure-analytics-loaded)
  (let ((tasks (alist-get 'tasks codelahoma-gtd-analytics-data))
        (hour-counts (make-vector 24 0)))
    (dolist (task tasks)
      (let* ((time-str (plist-get task :time))
             (hour (if time-str
                      (string-to-number (substring time-str 0 2))
                    12)))
        (aset hour-counts hour (1+ (aref hour-counts hour)))))
    hour-counts))

;;; Pattern Recognition

(defun codelahoma-gtd-identify-patterns ()
  "Identify patterns in task completion and reviews."
  (codelahoma-gtd-ensure-analytics-loaded)
  (let ((patterns '()))
    ;; Best day for reviews
    (let ((review-days (codelahoma-gtd-review-day-distribution)))
      (when review-days
        (let ((best-day (car (seq-reduce 
                             (lambda (best current)
                               (if (> (cdr current) (cdr best))
                                   current best))
                             review-days
                             (car review-days)))))
          (push (list :type 'best-review-day :value best-day) patterns))))
    
    ;; Most productive hour
    (let* ((hour-counts (codelahoma-gtd-completion-by-hour))
           (max-hour (seq-position hour-counts (seq-max hour-counts))))
      (when (and max-hour (> (aref hour-counts max-hour) 0))
        (push (list :type 'peak-hour :value max-hour) patterns)))
    
    ;; Context preference
    (let ((context-counts (codelahoma-gtd-completion-by-context)))
      (when (> (hash-table-count context-counts) 0)
        (let ((contexts-list '()))
          (maphash (lambda (k v) (push (cons k v) contexts-list)) context-counts)
          (let ((top-context (car (seq-sort (lambda (a b) (> (cdr a) (cdr b))) 
                                          contexts-list))))
            (push (list :type 'preferred-context :value (car top-context)) patterns)))))
    
    (setf (alist-get 'patterns codelahoma-gtd-analytics-data) patterns)
    (codelahoma-gtd-save-analytics)
    patterns))

(defun codelahoma-gtd-review-day-distribution ()
  "Get distribution of reviews by day of week."
  (let ((reviews (alist-get 'reviews codelahoma-gtd-analytics-data))
        (day-counts (make-hash-table :test 'equal)))
    (dolist (review reviews)
      (let ((day (plist-get review :day)))
        (puthash day (1+ (gethash day day-counts 0)) day-counts)))
    (let ((result '()))
      (maphash (lambda (k v) (push (cons k v) result)) day-counts)
      result)))

;;; Insights Dashboard

(defun codelahoma-gtd-insights-dashboard ()
  "Display productivity insights dashboard."
  (interactive)
  (codelahoma-gtd-ensure-analytics-loaded)
  (codelahoma-gtd-identify-patterns)
  
  (let ((buffer (get-buffer-create "*GTD Analytics & Insights*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: GTD Analytics & Insights\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d %H:%M") "\n\n")
      
      ;; Review streaks
      (insert "* Review Streaks\n")
      (codelahoma-gtd-insert-streak-info 'daily "Daily")
      (codelahoma-gtd-insert-streak-info 'weekly "Weekly") 
      (codelahoma-gtd-insert-streak-info 'monthly "Monthly")
      
      ;; Productivity metrics
      (insert "\n* Productivity Metrics\n")
      (insert (format "- Weekly velocity: %.1f tasks/week\n" 
                     (codelahoma-gtd-weekly-velocity)))
      (insert (format "- Total tasks tracked: %d\n"
                     (length (alist-get 'tasks codelahoma-gtd-analytics-data))))
      (insert (format "- Reviews completed: %d\n"
                     (length (alist-get 'reviews codelahoma-gtd-analytics-data))))
      
      ;; Patterns & insights
      (insert "\n* Patterns & Insights\n")
      (codelahoma-gtd-insert-patterns)
      
      ;; Completion heatmap
      (insert "\n* Completion Heatmap\n")
      (codelahoma-gtd-insert-hour-heatmap)
      
      ;; Context distribution
      (insert "\n* Context Distribution\n")
      (codelahoma-gtd-insert-context-distribution)
      
      ;; Recommendations
      (insert "\n* Recommendations\n")
      (codelahoma-gtd-insert-recommendations)
      
      (goto-char (point-min))
      (read-only-mode 1))
    
    (switch-to-buffer buffer)))

(defun codelahoma-gtd-insert-streak-info (type name)
  "Insert streak information for TYPE with NAME."
  (let ((current (codelahoma-gtd-review-streak type))
        (longest (codelahoma-gtd-longest-streak type)))
    (insert (format "- %s: %d days (longest: %d)\n" 
                   name current longest))
    (when (> current 0)
      (insert (format "  %s\n" (codelahoma-gtd-streak-visualization current))))))

(defun codelahoma-gtd-streak-visualization (streak)
  "Create visual representation of STREAK."
  (let ((symbols (cond
                  ((>= streak 30) "🔥🔥🔥")
                  ((>= streak 14) "🔥🔥")
                  ((>= streak 7) "🔥")
                  ((>= streak 3) "✨")
                  (t "🌱"))))
    (format "%s %s" symbols
           (cond
            ((>= streak 30) "On fire! Keep it up!")
            ((>= streak 14) "Great momentum!")
            ((>= streak 7) "Building consistency!")
            ((>= streak 3) "Good start!")
            (t "Keep going!")))))

(defun codelahoma-gtd-insert-patterns ()
  "Insert identified patterns."
  (let ((patterns (alist-get 'patterns codelahoma-gtd-analytics-data)))
    (if patterns
        (dolist (pattern patterns)
          (pcase (plist-get pattern :type)
            ('best-review-day 
             (insert (format "- Best day for reviews: %s\n" 
                           (plist-get pattern :value))))
            ('peak-hour
             (insert (format "- Most productive hour: %d:00-%d:00\n"
                           (plist-get pattern :value)
                           (1+ (plist-get pattern :value)))))
            ('preferred-context
             (insert (format "- Most used context: %s\n"
                           (plist-get pattern :value))))))
      (insert "- Gathering data... patterns will appear with more usage\n"))))

(defun codelahoma-gtd-insert-hour-heatmap ()
  "Insert hourly completion heatmap."
  (let* ((hour-counts (codelahoma-gtd-completion-by-hour))
         (max-count (seq-max hour-counts)))
    (if (> max-count 0)
        (progn
          (insert "```\n")
          (dotimes (hour 24)
            (when (= (mod hour 6) 0)
              (insert (format "\n%02d:00 " hour)))
            (let ((count (aref hour-counts hour)))
              (insert (codelahoma-gtd-heatmap-symbol count max-count))))
          (insert "\n```\n"))
      (insert "No completion data yet\n"))))

(defun codelahoma-gtd-heatmap-symbol (count max-count)
  "Return heatmap symbol for COUNT relative to MAX-COUNT."
  (let ((ratio (if (> max-count 0) (/ (float count) max-count) 0)))
    (cond
     ((= ratio 0) "⬜")
     ((< ratio 0.25) "🟦")
     ((< ratio 0.5) "🟩")
     ((< ratio 0.75) "🟨")
     (t "🟥"))))

(defun codelahoma-gtd-insert-context-distribution ()
  "Insert context distribution chart."
  (let ((context-counts (codelahoma-gtd-completion-by-context))
        (total 0))
    (when (> (hash-table-count context-counts) 0)
      (let ((contexts-list '()))
        (maphash (lambda (k v) 
                  (push (cons k v) contexts-list)
                  (cl-incf total v))
                context-counts)
        (setq contexts-list (seq-sort (lambda (a b) (> (cdr a) (cdr b))) 
                                    contexts-list))
        (dolist (context contexts-list)
          (let ((percentage (/ (* 100.0 (cdr context)) total)))
            (insert (format "- %s: %.1f%% (%d tasks)\n"
                           (car context) percentage (cdr context)))))))))

(defun codelahoma-gtd-insert-recommendations ()
  "Insert personalized recommendations."
  (let ((patterns (alist-get 'patterns codelahoma-gtd-analytics-data))
        (velocity (codelahoma-gtd-weekly-velocity)))
    
    ;; Review recommendations
    (when (< (codelahoma-gtd-review-streak 'weekly) 1)
      (insert "- 📅 Schedule your weekly review - it's been a while!\n"))
    
    (when (< (codelahoma-gtd-review-streak 'daily) 1)
      (insert "- 🌅 Try a morning review tomorrow to start fresh\n"))
    
    ;; Velocity recommendations
    (cond
     ((< velocity 10)
      (insert "- 📈 Consider breaking down tasks into smaller pieces\n"))
     ((> velocity 50)
      (insert "- 🎯 Great velocity! Make sure to maintain work-life balance\n")))
    
    ;; Pattern-based recommendations
    (let ((peak-hour (car (seq-find (lambda (p) (eq (plist-get p :type) 'peak-hour))
                                   patterns))))
      (when peak-hour
        (insert (format "- ⏰ Schedule important work during your peak hour (%d:00)\n"
                       (plist-get peak-hour :value)))))))

;;; Review Completion Tracking

(defun codelahoma-gtd-mark-review-complete (type)
  "Mark review of TYPE as complete."
  (codelahoma-gtd-record-review type)
  (message "%s review marked complete!" (capitalize (symbol-name type))))

(defun codelahoma-gtd-review-completion-rate (type days)
  "Calculate completion rate for review TYPE over DAYS."
  (codelahoma-gtd-ensure-analytics-loaded)
  (let* ((reviews (alist-get 'reviews codelahoma-gtd-analytics-data))
         (cutoff-date (time-subtract (current-time) (days-to-time days)))
         (recent-reviews (seq-filter 
                         (lambda (r)
                           (and (eq (plist-get r :type) type)
                                (time-less-p cutoff-date 
                                           (date-to-time (plist-get r :date)))))
                         reviews))
         (expected (pcase type
                    ('daily days)
                    ('weekly (/ days 7))
                    ('monthly (/ days 30))
                    (_ 1))))
    (if (> expected 0)
        (* 100.0 (/ (length recent-reviews) expected))
      0)))

;;; Export Functions

(defun codelahoma-gtd-export-analytics ()
  "Export analytics data to CSV."
  (interactive)
  (codelahoma-gtd-ensure-analytics-loaded)
  (let ((file (read-file-name "Export to CSV: " nil "gtd-analytics.csv")))
    (with-temp-file file
      ;; Task completions
      (insert "Type,Date,Time,Title,Context,Effort\n")
      (dolist (task (alist-get 'tasks codelahoma-gtd-analytics-data))
        (insert (format "task,%s,%s,\"%s\",%s,%s\n"
                       (plist-get task :date)
                       (plist-get task :time)
                       (plist-get task :title)
                       (or (plist-get task :context) "")
                       (or (plist-get task :effort) ""))))
      
      ;; Reviews
      (insert "\nType,Date,Time,Day,Duration\n")
      (dolist (review (alist-get 'reviews codelahoma-gtd-analytics-data))
        (insert (format "review-%s,%s,%s,%s,%s\n"
                       (plist-get review :type)
                       (plist-get review :date)
                       (plist-get review :time)
                       (plist-get review :day)
                       (or (plist-get review :duration) "")))))
    (message "Analytics exported to %s" file)))

;;; Initialize analytics on load
(add-hook 'after-init-hook 'codelahoma-gtd-load-analytics)

(provide 'codelahoma-gtd-analytics)
;;; codelahoma-gtd-analytics.el ends here