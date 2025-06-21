;;; codelahoma-gtd-monthly-review.el --- Monthly and quarterly review workflows -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Higher-level review processes for strategic planning and life design.
;; Implements monthly and quarterly reviews with goal tracking and area assessment.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)
(require 'codelahoma-gtd-daily-review)

;;; Monthly Review Configuration

(defcustom codelahoma-gtd-monthly-review-day 1
  "Day of month for monthly review (1-28)."
  :type 'integer
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-life-areas
  '(("Career & Work" . (:icon "💼" :weight 25))
    ("Health & Fitness" . (:icon "💪" :weight 20))
    ("Relationships" . (:icon "❤️" :weight 20))
    ("Personal Growth" . (:icon "🌱" :weight 15))
    ("Finance" . (:icon "💰" :weight 10))
    ("Home & Environment" . (:icon "🏠" :weight 5))
    ("Fun & Recreation" . (:icon "🎉" :weight 5)))
  "Life areas for monthly assessment with weights."
  :type '(alist :key-type string :value-type plist)
  :group 'codelahoma-gtd)

;;; Monthly Review Implementation

(defun codelahoma-gtd-monthly-review ()
  "Execute monthly review process."
  (interactive)
  (codelahoma-gtd-execute-monthly-review))

(defun codelahoma-gtd-execute-monthly-review ()
  "Execute the full monthly review process."
  (let ((review-buffer (get-buffer-create "*GTD Monthly Review*"))
        (month-name (format-time-string "%B %Y")))
    (with-current-buffer review-buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Monthly Review - " month-name "\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
      
      ;; Month in review
      (insert "* Month in Review\n")
      (codelahoma-gtd-monthly-accomplishments)
      (codelahoma-gtd-monthly-challenges)
      
      ;; Areas of focus review
      (insert "\n* Areas of Focus Assessment\n")
      (codelahoma-gtd-monthly-area-assessment)
      
      ;; Project portfolio review
      (insert "\n* Project Portfolio Analysis\n")
      (codelahoma-gtd-monthly-project-analysis)
      
      ;; Metrics and patterns
      (insert "\n* Monthly Metrics\n")
      (codelahoma-gtd-monthly-metrics)
      
      ;; Next month planning
      (insert "\n* Next Month Planning\n")
      (codelahoma-gtd-monthly-planning)
      
      ;; Reflection
      (insert "\n* Monthly Reflection\n")
      (insert "Key insights from this month:\n")
      (insert "1. \n2. \n3. \n\n")
      (insert "What patterns do I notice?\n- \n\n")
      (insert "What would make next month successful?\n- \n"))
    
    (switch-to-buffer review-buffer)
    (goto-char (point-min))
    (re-search-forward "^\\* Month in Review" nil t)
    (forward-line 1)))

(defun codelahoma-gtd-monthly-accomplishments ()
  "Generate monthly accomplishments section."
  (insert "\n** Major Accomplishments\n")
  (let ((completed-count (codelahoma-gtd-count-completed-this-month))
        (projects-completed (codelahoma-gtd-count-projects-completed-this-month)))
    (insert (format "- Total tasks completed: %d\n" completed-count))
    (insert (format "- Projects completed: %d\n" projects-completed))
    (insert "\nTop accomplishments:\n")
    (insert "1. [ ] \n2. [ ] \n3. [ ] \n")))

(defun codelahoma-gtd-monthly-challenges ()
  "Generate monthly challenges section."
  (insert "\n** Challenges & Obstacles\n")
  (insert "What got in the way this month?\n")
  (insert "1. \n2. \n3. \n\n")
  (insert "Lessons learned:\n- \n"))

(defun codelahoma-gtd-monthly-area-assessment ()
  "Assess each life area for the month."
  (dolist (area codelahoma-gtd-life-areas)
    (let* ((area-name (car area))
           (props (cdr area))
           (icon (plist-get props :icon)))
      (insert (format "\n** %s %s\n" icon area-name))
      (insert "Progress (1-10): [ ]\n")
      (insert "Key wins:\n- \n")
      (insert "Needs attention:\n- \n")
      (insert "Next month focus:\n- \n"))))

(defun codelahoma-gtd-monthly-project-analysis ()
  "Analyze project portfolio for the month."
  (let* ((all-projects (codelahoma-gtd-get-all-projects))
         (active-projects (seq-filter 
                          (lambda (p) (string= (plist-get p :status) "Active"))
                          all-projects))
         (completed-projects (seq-filter 
                             (lambda (p) (string= (plist-get p :status) "Completed"))
                             all-projects))
         (stalled-projects (seq-filter 
                           (lambda (p) (not (codelahoma-gtd-project-has-next-action-p p)))
                           active-projects)))
    (insert (format "- Active projects: %d\n" (length active-projects)))
    (insert (format "- Completed this month: %d\n" (length completed-projects)))
    (insert (format "- Stalled projects: %d\n" (length stalled-projects)))
    
    (when stalled-projects
      (insert "\nStalled projects needing attention:\n")
      (dolist (project stalled-projects)
        (insert (format "- %s\n" (plist-get project :title)))))
    
    (insert "\nProject health assessment:\n")
    (insert "- [ ] All projects have clear outcomes\n")
    (insert "- [ ] Next actions defined for each\n")
    (insert "- [ ] Priorities aligned with goals\n")))

(defun codelahoma-gtd-monthly-metrics ()
  "Calculate and display monthly metrics."
  (let* ((days-in-month (calendar-last-day-of-month 
                        (string-to-number (format-time-string "%m"))
                        (string-to-number (format-time-string "%Y"))))
         (completed (codelahoma-gtd-count-completed-this-month))
         (daily-average (/ (float completed) days-in-month))
         (weekly-reviews (codelahoma-gtd-count-weekly-reviews-this-month)))
    (insert (format "- Daily completion average: %.1f tasks/day\n" daily-average))
    (insert (format "- Weekly reviews completed: %d/4\n" weekly-reviews))
    (insert (format "- Inbox processing sessions: %d\n" 
                   (codelahoma-gtd-estimate-inbox-sessions)))
    
    ;; Productivity score
    (let ((score (codelahoma-gtd-calculate-monthly-score completed weekly-reviews)))
      (insert (format "\nProductivity Score: %d/100\n" score))
      (insert (codelahoma-gtd-score-interpretation score)))))

(defun codelahoma-gtd-monthly-planning ()
  "Plan for the next month."
  (insert "Top 3 outcomes for next month:\n")
  (insert "1. [ ] \n2. [ ] \n3. [ ] \n\n")
  
  (insert "Key projects to focus on:\n")
  (insert "- [ ] \n- [ ] \n- [ ] \n\n")
  
  (insert "Habits to build/maintain:\n")
  (insert "- [ ] \n- [ ] \n\n")
  
  (insert "Things to stop doing:\n")
  (insert "- \n- \n"))

;;; Quarterly Review Implementation

(defun codelahoma-gtd-quarterly-review ()
  "Execute quarterly review process."
  (interactive)
  (codelahoma-gtd-execute-quarterly-review))

(defun codelahoma-gtd-execute-quarterly-review ()
  "Execute the full quarterly review process."
  (let ((review-buffer (get-buffer-create "*GTD Quarterly Review*"))
        (quarter (1+ (/ (1- (string-to-number (format-time-string "%m"))) 3)))
        (year (string-to-number (format-time-string "%Y"))))
    (with-current-buffer review-buffer
      (erase-buffer)
      (org-mode)
      (insert (format "#+TITLE: Quarterly Review - Q%d %d\n" quarter year))
      (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
      
      ;; Quarter overview
      (insert "* Quarter Overview\n")
      (codelahoma-gtd-quarterly-summary quarter year)
      
      ;; Goal review
      (insert "\n* Quarterly Goals Review\n")
      (codelahoma-gtd-quarterly-goal-review)
      
      ;; Life wheel assessment
      (insert "\n* Life Balance Assessment\n")
      (codelahoma-gtd-quarterly-life-wheel)
      
      ;; Major wins and learnings
      (insert "\n* Major Wins & Learnings\n")
      (codelahoma-gtd-quarterly-wins-learnings)
      
      ;; Strategic planning
      (insert "\n* Next Quarter Planning\n")
      (codelahoma-gtd-quarterly-planning)
      
      ;; Annual progress check
      (insert "\n* Annual Progress Check\n")
      (codelahoma-gtd-quarterly-annual-progress quarter))
    
    (switch-to-buffer review-buffer)
    (goto-char (point-min))))

(defun codelahoma-gtd-quarterly-summary (quarter year)
  "Generate quarterly summary for QUARTER and YEAR."
  (insert (format "Reviewing Q%d %d performance and planning ahead.\n\n" quarter year))
  
  (let* ((months (codelahoma-gtd-quarter-months quarter))
         (total-completed 0))
    (insert "** Quarter Statistics\n")
    (dolist (month months)
      (let ((completed (codelahoma-gtd-count-completed-in-month month year)))
        (cl-incf total-completed completed)
        (insert (format "- %s: %d tasks completed\n" 
                       (calendar-month-name month)
                       completed))))
    (insert (format "\nTotal completed this quarter: %d\n" total-completed))))

(defun codelahoma-gtd-quarterly-goal-review ()
  "Review quarterly goals."
  (insert "Review each quarterly goal:\n\n")
  (insert "** Q1 Goals\n")
  (insert "1. [ ] Goal: \n   Progress: ___/10\n   Status: \n\n")
  (insert "2. [ ] Goal: \n   Progress: ___/10\n   Status: \n\n")
  (insert "3. [ ] Goal: \n   Progress: ___/10\n   Status: \n\n"))

(defun codelahoma-gtd-quarterly-life-wheel ()
  "Create life wheel assessment."
  (insert "Rate each area (1-10):\n\n")
  (dolist (area codelahoma-gtd-life-areas)
    (let* ((area-name (car area))
           (props (cdr area))
           (icon (plist-get props :icon))
           (weight (plist-get props :weight)))
      (insert (format "%s %s: ___/10 (weight: %d%%)\n" 
                     icon area-name weight))))
  
  (insert "\n** Balance Analysis\n")
  (insert "- Most satisfied area: \n")
  (insert "- Least satisfied area: \n")
  (insert "- Biggest improvement from last quarter: \n")
  (insert "- Area needing most attention: \n"))

(defun codelahoma-gtd-quarterly-wins-learnings ()
  "Capture major wins and learnings."
  (insert "** Top 5 Wins This Quarter\n")
  (dotimes (i 5)
    (insert (format "%d. \n" (1+ i))))
  
  (insert "\n** Key Learnings\n")
  (insert "What worked well:\n")
  (insert "1. \n2. \n3. \n\n")
  
  (insert "What didn't work:\n")
  (insert "1. \n2. \n3. \n\n")
  
  (insert "What I'll do differently:\n")
  (insert "1. \n2. \n3. \n"))

(defun codelahoma-gtd-quarterly-planning ()
  "Plan for next quarter."
  (insert "** Next Quarter Theme\n")
  (insert "Theme: \n")
  (insert "Why this theme: \n\n")
  
  (insert "** Quarterly Objectives (3-5 max)\n")
  (insert "1. [ ] \n   Key Result: \n   Key Result: \n\n")
  (insert "2. [ ] \n   Key Result: \n   Key Result: \n\n")
  (insert "3. [ ] \n   Key Result: \n   Key Result: \n\n")
  
  (insert "** Key Projects\n")
  (insert "Projects that will drive these objectives:\n")
  (insert "- [ ] \n- [ ] \n- [ ] \n\n")
  
  (insert "** Habits to Build\n")
  (insert "- [ ] \n- [ ] \n"))

(defun codelahoma-gtd-quarterly-annual-progress (quarter)
  "Check annual progress at QUARTER."
  (let ((progress-pct (* quarter 25)))
    (insert (format "We are %d%% through the year.\n\n" progress-pct))
    
    (insert "** Annual Goals Check\n")
    (insert "Review your annual goals and rate progress:\n")
    (insert "1. Goal: \n   Progress: ___% | On track? [ ] Yes [ ] No\n\n")
    (insert "2. Goal: \n   Progress: ___% | On track? [ ] Yes [ ] No\n\n")
    (insert "3. Goal: \n   Progress: ___% | On track? [ ] Yes [ ] No\n\n")
    
    (when (>= quarter 2)
      (insert "** Mid-Year Adjustments\n")
      (insert "Goals to adjust or abandon:\n- \n\n")
      (insert "New goals to add:\n- \n"))))

;;; Annual Review (bonus)

(defun codelahoma-gtd-annual-review ()
  "Execute annual review process."
  (interactive)
  (let ((review-buffer (get-buffer-create "*GTD Annual Review*"))
        (year (string-to-number (format-time-string "%Y"))))
    (with-current-buffer review-buffer
      (erase-buffer)
      (org-mode)
      (insert (format "#+TITLE: Annual Review - %d\n" year))
      (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
      
      (insert "* Year in Review\n")
      (insert "This has been a year of...\n\n")
      
      (insert "* Major Accomplishments\n")
      (insert "Top 10 accomplishments:\n")
      (dotimes (i 10)
        (insert (format "%d. \n" (1+ i))))
      
      (insert "\n* Gratitude\n")
      (insert "People I'm grateful for:\n- \n- \n- \n\n")
      (insert "Experiences I'm grateful for:\n- \n- \n- \n\n")
      
      (insert "* Next Year Vision\n")
      (insert "Theme for next year: \n")
      (insert "Why: \n\n")
      (insert "Three words to describe next year:\n")
      (insert "1. \n2. \n3. \n"))
    
    (switch-to-buffer review-buffer)))

;;; Utility Functions

(defun codelahoma-gtd-count-completed-this-month ()
  "Count tasks completed this month."
  (let* ((month-start (encode-time 0 0 0 1 
                                  (string-to-number (format-time-string "%m"))
                                  (string-to-number (format-time-string "%Y"))))
         (count 0))
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\*+ DONE" nil t)
            (let ((closed-time (org-entry-get nil "CLOSED")))
              (when (and closed-time
                        (time-less-p month-start (org-time-string-to-time closed-time)))
                (cl-incf count)))))))
    count))

(defun codelahoma-gtd-count-projects-completed-this-month ()
  "Count projects completed this month."
  (let* ((month-start (encode-time 0 0 0 1 
                                  (string-to-number (format-time-string "%m"))
                                  (string-to-number (format-time-string "%Y"))))
         (count 0))
    (with-current-buffer (find-file-noselect 
                         (expand-file-name "projects.org" codelahoma-gtd-directory))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\* COMPLETED" nil t)
          (let ((closed-time (org-entry-get nil "CLOSED")))
            (when (and closed-time
                      (time-less-p month-start (org-time-string-to-time closed-time)))
              (cl-incf count))))))
    count))

(defun codelahoma-gtd-count-weekly-reviews-this-month ()
  "Count weekly reviews completed this month."
  (let ((month (format-time-string "%Y-%m"))
        (count 0))
    (when (file-exists-p codelahoma-gtd-review-directory)
      (dolist (file (directory-files codelahoma-gtd-review-directory nil 
                                    ".*-weekly-review\\.org$"))
        (when (string-match month file)
          (cl-incf count))))
    count))

(defun codelahoma-gtd-count-completed-in-month (month year)
  "Count tasks completed in MONTH of YEAR."
  (let* ((month-start (encode-time 0 0 0 1 month year))
         (month-end (encode-time 0 0 0 1 (1+ month) year))
         (count 0))
    (dolist (file (directory-files codelahoma-gtd-directory t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\*+ DONE" nil t)
            (let ((closed-time (org-entry-get nil "CLOSED")))
              (when (and closed-time
                        (let ((time (org-time-string-to-time closed-time)))
                          (and (time-less-p month-start time)
                               (time-less-p time month-end))))
                (cl-incf count)))))))
    count))

(defun codelahoma-gtd-quarter-months (quarter)
  "Return list of months in QUARTER."
  (pcase quarter
    (1 '(1 2 3))
    (2 '(4 5 6))
    (3 '(7 8 9))
    (4 '(10 11 12))
    (_ '(1 2 3))))

(defun codelahoma-gtd-estimate-inbox-sessions ()
  "Estimate number of inbox processing sessions."
  ;; Simple heuristic - could be enhanced with actual tracking
  20)

(defun codelahoma-gtd-calculate-monthly-score (completed reviews)
  "Calculate monthly productivity score from COMPLETED tasks and REVIEWS."
  (let* ((task-score (min 70 (* 2 completed))) ; Max 70 points from tasks
         (review-score (* reviews 7.5))        ; Max 30 points from 4 reviews
         (total (+ task-score review-score)))
    (min 100 (round total))))

(defun codelahoma-gtd-score-interpretation (score)
  "Provide interpretation for productivity SCORE."
  (cond
   ((>= score 90) "🌟 Outstanding month! Keep up the excellent work.")
   ((>= score 75) "✨ Great month! You're in a good rhythm.")
   ((>= score 60) "👍 Solid month. Room for improvement.")
   ((>= score 40) "💪 Challenging month. Focus on consistency.")
   (t "🎯 Time to refocus. What can you simplify?")))

;;; Save Functions

(defun codelahoma-gtd-save-monthly-review ()
  "Save monthly review to history."
  (interactive)
  (when (get-buffer "*GTD Monthly Review*")
    (let* ((date (format-time-string "%Y-%m"))
           (filename (format "%s-monthly-review.org" date))
           (filepath (expand-file-name filename codelahoma-gtd-review-directory)))
      (unless (file-exists-p codelahoma-gtd-review-directory)
        (make-directory codelahoma-gtd-review-directory t))
      (with-current-buffer "*GTD Monthly Review*"
        (write-region (point-min) (point-max) filepath))
      (message "Monthly review saved to %s" filename))))

(defun codelahoma-gtd-save-quarterly-review ()
  "Save quarterly review to history."
  (interactive)
  (when (get-buffer "*GTD Quarterly Review*")
    (let* ((quarter (1+ (/ (1- (string-to-number (format-time-string "%m"))) 3)))
           (year (format-time-string "%Y"))
           (filename (format "%s-Q%d-quarterly-review.org" year quarter))
           (filepath (expand-file-name filename codelahoma-gtd-review-directory)))
      (unless (file-exists-p codelahoma-gtd-review-directory)
        (make-directory codelahoma-gtd-review-directory t))
      (with-current-buffer "*GTD Quarterly Review*"
        (write-region (point-min) (point-max) filepath))
      (message "Quarterly review saved to %s" filename))))

(provide 'codelahoma-gtd-monthly-review)
;;; codelahoma-gtd-monthly-review.el ends here