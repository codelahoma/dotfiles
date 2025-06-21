;;; test-analytics.el --- Test GTD analytics functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying GTD analytics and insights functionality

;;; Code:

(require 'codelahoma-gtd-analytics)

(defun test-gtd-analytics ()
  "Test GTD analytics functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check core analytics functions
    (push (cons "Load analytics exists" 
                (fboundp 'codelahoma-gtd-load-analytics))
          results)
    
    (push (cons "Save analytics exists" 
                (fboundp 'codelahoma-gtd-save-analytics))
          results)
    
    (push (cons "Record review exists" 
                (fboundp 'codelahoma-gtd-record-review))
          results)
    
    ;; Test 2: Check streak tracking
    (push (cons "Update streak exists" 
                (fboundp 'codelahoma-gtd-update-streak))
          results)
    
    (push (cons "Review streak exists" 
                (fboundp 'codelahoma-gtd-review-streak))
          results)
    
    (push (cons "Longest streak exists" 
                (fboundp 'codelahoma-gtd-longest-streak))
          results)
    
    ;; Test 3: Check task analytics
    (push (cons "Record task completion exists" 
                (fboundp 'codelahoma-gtd-record-task-completion))
          results)
    
    (push (cons "Weekly velocity exists" 
                (fboundp 'codelahoma-gtd-weekly-velocity))
          results)
    
    (push (cons "Completion by context exists" 
                (fboundp 'codelahoma-gtd-completion-by-context))
          results)
    
    ;; Test 4: Check pattern recognition
    (push (cons "Identify patterns exists" 
                (fboundp 'codelahoma-gtd-identify-patterns))
          results)
    
    (push (cons "Review day distribution exists" 
                (fboundp 'codelahoma-gtd-review-day-distribution))
          results)
    
    ;; Test 5: Check insights dashboard
    (push (cons "Insights dashboard exists" 
                (fboundp 'codelahoma-gtd-insights-dashboard))
          results)
    
    (push (cons "Export analytics exists" 
                (fboundp 'codelahoma-gtd-export-analytics))
          results)
    
    ;; Test 6: Test analytics loading
    (codelahoma-gtd-ensure-analytics-loaded)
    (push (cons "Analytics data structure loaded" 
                (and (boundp 'codelahoma-gtd-analytics-data)
                     (listp codelahoma-gtd-analytics-data)))
          results)
    
    ;; Test 7: Test review recording
    (let ((before-count (length (alist-get 'reviews codelahoma-gtd-analytics-data))))
      (codelahoma-gtd-record-review 'test)
      (let ((after-count (length (alist-get 'reviews codelahoma-gtd-analytics-data))))
        (push (cons "Review recording works" 
                    (= after-count (1+ before-count)))
              results)))
    
    ;; Test 8: Test streak calculation
    (let ((streak (codelahoma-gtd-review-streak 'daily)))
      (push (cons "Streak calculation returns number" 
                  (numberp streak))
            results))
    
    ;; Test 9: Test completion rate calculation
    (let ((rate (codelahoma-gtd-review-completion-rate 'daily 7)))
      (push (cons "Completion rate calculation" 
                  (and (numberp rate) (>= rate 0) (<= rate 100)))
            results))
    
    ;; Test 10: Test configuration
    (push (cons "Analytics file configured" 
                (and (boundp 'codelahoma-gtd-analytics-file)
                     (stringp codelahoma-gtd-analytics-file)))
          results)
    
    ;; Report results
    (with-output-to-temp-buffer "*GTD Analytics Test Results*"
      (princ "GTD Analytics Test Results\n")
      (princ "==========================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-35s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*GTD Analytics Test Results*")))

;; Run the test
(test-gtd-analytics)

(provide 'test-analytics)
;;; test-analytics.el ends here