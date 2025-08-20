;;; codelahoma-gtd-reminders.el --- Review automation and reminders -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Automated reminders and notifications for GTD review cycles.
;; Provides timely nudges to maintain review discipline.

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)
(require 'codelahoma-gtd-analytics)

;;; Reminder Configuration

(defcustom codelahoma-gtd-enable-reminders t
  "Enable automatic review reminders."
  :type 'boolean
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-morning-review-time "08:00"
  "Time for morning review reminder (HH:MM)."
  :type 'string
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-evening-review-time "17:30"
  "Time for evening review reminder (HH:MM)."
  :type 'string
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-weekly-review-reminder-day 4
  "Day to remind about weekly review (0=Sunday, 4=Thursday)."
  :type 'integer
  :group 'codelahoma-gtd)

(defcustom codelahoma-gtd-reminder-advance-minutes 15
  "Minutes before review time to show reminder."
  :type 'integer
  :group 'codelahoma-gtd)

;;; Timer Management

(defvar codelahoma-gtd-reminder-timers nil
  "List of active reminder timers.")

(defun codelahoma-gtd-start-reminders ()
  "Start all GTD reminder timers."
  (interactive)
  (codelahoma-gtd-stop-reminders) ; Clear existing timers
  (when codelahoma-gtd-enable-reminders
    ;; Daily reminders
    (codelahoma-gtd-schedule-daily-reminders)
    ;; Weekly reminder
    (codelahoma-gtd-schedule-weekly-reminder)
    ;; Monthly reminder
    (codelahoma-gtd-schedule-monthly-reminder)
    (message "GTD reminders activated")))

(defun codelahoma-gtd-stop-reminders ()
  "Stop all GTD reminder timers."
  (interactive)
  (dolist (timer codelahoma-gtd-reminder-timers)
    (cancel-timer timer))
  (setq codelahoma-gtd-reminder-timers nil)
  (message "GTD reminders deactivated"))

;;; Daily Reminders

(defun codelahoma-gtd-schedule-daily-reminders ()
  "Schedule daily review reminders."
  ;; Morning review
  (let ((morning-timer (codelahoma-gtd-schedule-daily-timer
                       codelahoma-gtd-morning-review-time
                       'codelahoma-gtd-morning-review-reminder)))
    (when morning-timer
      (push morning-timer codelahoma-gtd-reminder-timers)))
  
  ;; Evening review
  (let ((evening-timer (codelahoma-gtd-schedule-daily-timer
                       codelahoma-gtd-evening-review-time
                       'codelahoma-gtd-evening-review-reminder)))
    (when evening-timer
      (push evening-timer codelahoma-gtd-reminder-timers))))

(defun codelahoma-gtd-schedule-daily-timer (time-string function)
  "Schedule FUNCTION to run daily at TIME-STRING."
  (let* ((time-parts (split-string time-string ":"))
         (hour (string-to-number (car time-parts)))
         (minute (string-to-number (cadr time-parts)))
         (now (decode-time))
         (today-time (encode-time 0 minute hour 
                                 (nth 3 now) (nth 4 now) (nth 5 now)))
         (tomorrow-time (time-add today-time (days-to-time 1))))
    ;; If time has passed today, schedule for tomorrow
    (when (time-less-p today-time (current-time))
      (setq today-time tomorrow-time))
    ;; Run daily at specified time
    (run-at-time today-time (* 24 60 60) function)))

(defun codelahoma-gtd-morning-review-reminder ()
  "Show morning review reminder."
  (when (codelahoma-gtd-should-show-reminder 'morning)
    (codelahoma-gtd-show-reminder
     "Morning Review Time"
     "Time for your morning GTD review!\n\nPress 'y' to start review, 'n' to skip."
     (lambda ()
       (when (featurep 'codelahoma-gtd-daily-review)
         (codelahoma-gtd-morning-review))))))

(defun codelahoma-gtd-evening-review-reminder ()
  "Show evening review reminder."
  (when (codelahoma-gtd-should-show-reminder 'evening)
    (codelahoma-gtd-show-reminder
     "Evening Review Time"
     "Time for your evening GTD review!\n\nPress 'y' to start review, 'n' to skip."
     (lambda ()
       (when (featurep 'codelahoma-gtd-daily-review)
         (codelahoma-gtd-evening-review))))))

;;; Weekly Reminder

(defun codelahoma-gtd-schedule-weekly-reminder ()
  "Schedule weekly review reminder."
  (let* ((days-until (- codelahoma-gtd-weekly-review-reminder-day
                       (nth 6 (decode-time))))
         (days-until (if (< days-until 0) (+ days-until 7) days-until))
         (reminder-time (time-add (current-time) (days-to-time days-until))))
    ;; Schedule for Thursday (or configured day) at 3pm
    (let ((timer (run-at-time
                  (format-time-string "%Y-%m-%d 15:00" reminder-time)
                  (* 7 24 60 60) ; Weekly
                  'codelahoma-gtd-weekly-review-reminder)))
      (push timer codelahoma-gtd-reminder-timers))))

(defun codelahoma-gtd-weekly-review-reminder ()
  "Show weekly review reminder."
  (when (codelahoma-gtd-should-show-reminder 'weekly)
    (let ((days-since (codelahoma-gtd-days-since-last-review 'weekly)))
      (codelahoma-gtd-show-reminder
       "Weekly Review Reminder"
       (format "It's been %d days since your last weekly review.\n\nSchedule time this week!"
               (or days-since 999))
       (lambda ()
         (when (featurep 'codelahoma-gtd-weekly-review)
           (codelahoma-gtd-weekly-review)))))))

;;; Monthly Reminder

(defun codelahoma-gtd-schedule-monthly-reminder ()
  "Schedule monthly review reminder."
  ;; Run on the 1st of each month at 10am
  (let* ((now (decode-time))
         (next-month (if (> (nth 3 now) 1)
                        (encode-time 0 0 10 1 (1+ (nth 4 now)) (nth 5 now))
                      (encode-time 0 0 10 1 (nth 4 now) (nth 5 now)))))
    (let ((timer (run-at-time next-month (* 30 24 60 60) ; Roughly monthly
                             'codelahoma-gtd-monthly-review-reminder)))
      (push timer codelahoma-gtd-reminder-timers))))

(defun codelahoma-gtd-monthly-review-reminder ()
  "Show monthly review reminder."
  (when (codelahoma-gtd-should-show-reminder 'monthly)
    (codelahoma-gtd-show-reminder
     "Monthly Review Time"
     "New month! Time for your monthly GTD review.\n\nThis helps maintain perspective."
     (lambda ()
       (when (featurep 'codelahoma-gtd-monthly-review)
         (codelahoma-gtd-monthly-review))))))

;;; Review Status Checks

(defun codelahoma-gtd-should-show-reminder (review-type)
  "Check if reminder should be shown for REVIEW-TYPE."
  (let ((days-since (codelahoma-gtd-days-since-last-review review-type)))
    (or (null days-since) ; Never done
        (pcase review-type
          ('morning (>= days-since 1))
          ('evening (>= days-since 1))
          ('weekly (>= days-since 7))
          ('monthly (>= days-since 30))
          (_ t)))))

(defun codelahoma-gtd-days-since-last-review (review-type)
  "Get days since last REVIEW-TYPE."
  (when (featurep 'codelahoma-gtd-analytics)
    (codelahoma-gtd-ensure-analytics-loaded)
    (let* ((reviews (alist-get 'reviews codelahoma-gtd-analytics-data))
           (last-review (seq-find (lambda (r) (eq (plist-get r :type) review-type))
                                 (reverse reviews))))
      (when last-review
        (let ((last-date (date-to-time (plist-get last-review :date))))
          (/ (float-time (time-subtract (current-time) last-date))
             (* 24 60 60)))))))

;;; Notification Interface

(defun codelahoma-gtd-show-reminder (title message action-fn)
  "Show reminder with TITLE and MESSAGE, calling ACTION-FN if accepted."
  (if (display-graphic-p)
      ;; GUI notification
      (codelahoma-gtd-show-gui-reminder title message action-fn)
    ;; Terminal notification
    (codelahoma-gtd-show-terminal-reminder title message action-fn)))

(defun codelahoma-gtd-show-gui-reminder (title message action-fn)
  "Show GUI reminder for TITLE with MESSAGE and ACTION-FN."
  ;; Use built-in notifications if available
  (when (fboundp 'notifications-notify)
    (notifications-notify
     :title title
     :body message
     :urgency 'normal
     :actions '("default" "Start Review")
     :on-action (lambda (id key) (funcall action-fn))))
  ;; Also show in minibuffer
  (when (yes-or-no-p (concat title "\n\n" message "\n\nStart review? "))
    (funcall action-fn)))

(defun codelahoma-gtd-show-terminal-reminder (title message action-fn)
  "Show terminal reminder for TITLE with MESSAGE and ACTION-FN."
  (with-current-buffer (get-buffer-create "*GTD Reminder*")
    (erase-buffer)
    (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    (insert (propertize title 'face 'bold) "\n")
    (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
    (insert message "\n\n")
    (insert "[y] Start Review  [n] Skip  [s] Snooze\n")
    (pop-to-buffer (current-buffer))
    (let ((response (read-char)))
      (kill-buffer)
      (pcase response
        (?y (funcall action-fn))
        (?s (codelahoma-gtd-snooze-reminder title message action-fn))
        (_ (message "Review skipped"))))))

(defun codelahoma-gtd-snooze-reminder (title message action-fn)
  "Snooze reminder for 30 minutes."
  (run-at-time (* 30 60) nil
               (lambda ()
                 (codelahoma-gtd-show-reminder title message action-fn)))
  (message "Reminder snoozed for 30 minutes"))

;;; Smart Reminders

(defun codelahoma-gtd-inbox-overflow-check ()
  "Check for inbox overflow and remind if needed."
  (let ((inbox-count (codelahoma-gtd-count-entries "inbox.org")))
    (when (> inbox-count 20)
      (codelahoma-gtd-show-reminder
       "Inbox Overflow Alert"
       (format "Your inbox has %d items!\n\nTime to process?" inbox-count)
       (lambda ()
         (when (featurep 'codelahoma-gtd-process)
           (codelahoma-gtd-process-inbox)))))))

(defun codelahoma-gtd-stalled-projects-check ()
  "Check for stalled projects and remind if needed."
  (let ((stalled (length (codelahoma-gtd-find-stalled-projects))))
    (when (> stalled 0)
      (codelahoma-gtd-show-reminder
       "Stalled Projects Alert"
       (format "You have %d stalled projects without next actions.\n\nReview them?" stalled)
       (lambda ()
         (codelahoma-gtd-find-stalled-projects))))))

;;; Review Streaks

(defun codelahoma-gtd-check-streak-status ()
  "Check review streaks and encourage continuation."
  (when (featurep 'codelahoma-gtd-analytics)
    (let ((daily-streak (codelahoma-gtd-review-streak 'daily))
          (weekly-streak (codelahoma-gtd-review-streak 'weekly)))
      (cond
       ;; About to break a good streak
       ((and (>= daily-streak 7)
             (>= (or (codelahoma-gtd-days-since-last-review 'daily) 0) 1))
        (message "🔥 Don't break your %d day review streak!" daily-streak))
       ;; Milestone reached
       ((member daily-streak '(7 14 30 60 90 180 365))
        (message "🎉 Amazing! %d day review streak!" daily-streak))
       ;; Building momentum
       ((= daily-streak 3)
        (message "🌱 3 days in a row! Keep building that habit!"))))))

;;; Automation Hooks

(defun codelahoma-gtd-enable-auto-reminders ()
  "Enable automatic reminder system."
  (interactive)
  (add-hook 'after-init-hook 'codelahoma-gtd-start-reminders)
  (codelahoma-gtd-start-reminders)
  (message "GTD auto-reminders enabled"))

(defun codelahoma-gtd-disable-auto-reminders ()
  "Disable automatic reminder system."
  (interactive)
  (remove-hook 'after-init-hook 'codelahoma-gtd-start-reminders)
  (codelahoma-gtd-stop-reminders)
  (message "GTD auto-reminders disabled"))

;;; Status Display

(defun codelahoma-gtd-reminder-status ()
  "Display current reminder configuration and status."
  (interactive)
  (with-output-to-temp-buffer "*GTD Reminder Status*"
    (princ "GTD Reminder System Status\n")
    (princ "==========================\n\n")
    
    (princ (format "Reminders: %s\n" 
                  (if codelahoma-gtd-enable-reminders "ENABLED" "DISABLED")))
    (princ (format "Active timers: %d\n\n" (length codelahoma-gtd-reminder-timers)))
    
    (princ "Schedule:\n")
    (princ (format "- Morning review: %s\n" codelahoma-gtd-morning-review-time))
    (princ (format "- Evening review: %s\n" codelahoma-gtd-evening-review-time))
    (princ (format "- Weekly reminder: %s\n" 
                  (nth codelahoma-gtd-weekly-review-reminder-day
                       '("Sunday" "Monday" "Tuesday" "Wednesday" 
                         "Thursday" "Friday" "Saturday"))))
    (princ "- Monthly reminder: 1st of month\n\n")
    
    (princ "Last Reviews:\n")
    (dolist (type '(daily weekly monthly))
      (let ((days (codelahoma-gtd-days-since-last-review type)))
        (princ (format "- %s: %s\n" 
                      (capitalize (symbol-name type))
                      (if days 
                          (format "%.1f days ago" days)
                        "Never")))))
    
    (princ "\nStreak Status:\n")
    (when (featurep 'codelahoma-gtd-analytics)
      (dolist (type '(daily weekly monthly))
        (let ((streak (codelahoma-gtd-review-streak type)))
          (princ (format "- %s streak: %d\n" 
                        (capitalize (symbol-name type))
                        streak)))))))

;;; Initialize reminders if enabled
(when codelahoma-gtd-enable-reminders
  (add-hook 'after-init-hook 'codelahoma-gtd-start-reminders))

(provide 'codelahoma-gtd-reminders)
;;; codelahoma-gtd-reminders.el ends here