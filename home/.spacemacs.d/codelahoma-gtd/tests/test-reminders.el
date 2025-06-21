;;; test-reminders.el --- Test GTD reminder functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying GTD reminder and automation functionality

;;; Code:

(require 'codelahoma-gtd-reminders)

(defun test-gtd-reminders ()
  "Test GTD reminder functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check timer management functions
    (push (cons "Start reminders exists" 
                (fboundp 'codelahoma-gtd-start-reminders))
          results)
    
    (push (cons "Stop reminders exists" 
                (fboundp 'codelahoma-gtd-stop-reminders))
          results)
    
    ;; Test 2: Check daily reminder functions
    (push (cons "Schedule daily reminders exists" 
                (fboundp 'codelahoma-gtd-schedule-daily-reminders))
          results)
    
    (push (cons "Morning reminder exists" 
                (fboundp 'codelahoma-gtd-morning-review-reminder))
          results)
    
    (push (cons "Evening reminder exists" 
                (fboundp 'codelahoma-gtd-evening-review-reminder))
          results)
    
    ;; Test 3: Check weekly/monthly reminders
    (push (cons "Schedule weekly reminder exists" 
                (fboundp 'codelahoma-gtd-schedule-weekly-reminder))
          results)
    
    (push (cons "Schedule monthly reminder exists" 
                (fboundp 'codelahoma-gtd-schedule-monthly-reminder))
          results)
    
    ;; Test 4: Check status functions
    (push (cons "Should show reminder exists" 
                (fboundp 'codelahoma-gtd-should-show-reminder))
          results)
    
    (push (cons "Days since last review exists" 
                (fboundp 'codelahoma-gtd-days-since-last-review))
          results)
    
    ;; Test 5: Check notification functions
    (push (cons "Show reminder exists" 
                (fboundp 'codelahoma-gtd-show-reminder))
          results)
    
    (push (cons "Snooze reminder exists" 
                (fboundp 'codelahoma-gtd-snooze-reminder))
          results)
    
    ;; Test 6: Check smart reminders
    (push (cons "Inbox overflow check exists" 
                (fboundp 'codelahoma-gtd-inbox-overflow-check))
          results)
    
    (push (cons "Stalled projects check exists" 
                (fboundp 'codelahoma-gtd-stalled-projects-check))
          results)
    
    (push (cons "Check streak status exists" 
                (fboundp 'codelahoma-gtd-check-streak-status))
          results)
    
    ;; Test 7: Check automation functions
    (push (cons "Enable auto reminders exists" 
                (fboundp 'codelahoma-gtd-enable-auto-reminders))
          results)
    
    (push (cons "Disable auto reminders exists" 
                (fboundp 'codelahoma-gtd-disable-auto-reminders))
          results)
    
    (push (cons "Reminder status exists" 
                (fboundp 'codelahoma-gtd-reminder-status))
          results)
    
    ;; Test 8: Test configuration
    (push (cons "Enable reminders configured" 
                (boundp 'codelahoma-gtd-enable-reminders))
          results)
    
    (push (cons "Morning time configured" 
                (and (boundp 'codelahoma-gtd-morning-review-time)
                     (stringp codelahoma-gtd-morning-review-time)))
          results)
    
    (push (cons "Evening time configured" 
                (and (boundp 'codelahoma-gtd-evening-review-time)
                     (stringp codelahoma-gtd-evening-review-time)))
          results)
    
    ;; Test 9: Test timer list
    (push (cons "Timer list initialized" 
                (and (boundp 'codelahoma-gtd-reminder-timers)
                     (listp codelahoma-gtd-reminder-timers)))
          results)
    
    ;; Test 10: Test time parsing
    (let ((parsed (codelahoma-gtd-schedule-daily-timer "10:30" 'ignore)))
      (push (cons "Daily timer scheduling works" 
                  (timerp parsed))
            results)
      (when (timerp parsed)
        (cancel-timer parsed)))
    
    ;; Report results
    (with-output-to-temp-buffer "*GTD Reminders Test Results*"
      (princ "GTD Reminders Test Results\n")
      (princ "==========================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-35s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*GTD Reminders Test Results*")))

;; Run the test
(test-gtd-reminders)

(provide 'test-reminders)
;;; test-reminders.el ends here