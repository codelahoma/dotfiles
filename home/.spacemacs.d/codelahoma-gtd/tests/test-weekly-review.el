;;; test-weekly-review.el --- Test GTD weekly review functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying GTD weekly review system

;;; Code:

(require 'codelahoma-gtd-weekly-review)

(defun test-gtd-weekly-review ()
  "Test GTD weekly review functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check weekly review function
    (push (cons "Weekly review function exists" 
                (fboundp 'codelahoma-gtd-weekly-review))
          results)
    
    ;; Test 2: Check navigation functions
    (push (cons "Review next function exists" 
                (fboundp 'codelahoma-gtd-weekly-review-next))
          results)
    
    (push (cons "Review previous function exists" 
                (fboundp 'codelahoma-gtd-weekly-review-previous))
          results)
    
    ;; Test 3: Check content generators
    (push (cons "Inbox content generator exists" 
                (fboundp 'codelahoma-gtd-review-inbox-content))
          results)
    
    (push (cons "Projects content generator exists" 
                (fboundp 'codelahoma-gtd-review-projects-content))
          results)
    
    ;; Test 4: Check utility functions
    (push (cons "Progress bar function exists" 
                (fboundp 'codelahoma-gtd-review-progress-bar))
          results)
    
    (push (cons "Project next action check exists" 
                (fboundp 'codelahoma-gtd-project-has-next-action-p))
          results)
    
    ;; Test 5: Check save functionality
    (push (cons "Save weekly review function exists" 
                (fboundp 'codelahoma-gtd-save-weekly-review))
          results)
    
    ;; Test 6: Check metrics functions
    (push (cons "Count completed this week exists" 
                (fboundp 'codelahoma-gtd-count-completed-this-week))
          results)
    
    (push (cons "Quick weekly check exists" 
                (fboundp 'codelahoma-gtd-quick-weekly-check))
          results)
    
    ;; Test 7: Test progress bar generation
    (let ((progress-bar (codelahoma-gtd-review-progress-bar 5 10)))
      (push (cons "Progress bar generates correctly" 
                  (and (stringp progress-bar)
                       (string-match-p "\\[.*>.*\\]" progress-bar)))
            results))
    
    ;; Test 8: Test review steps configuration
    (push (cons "Review steps configured" 
                (and (boundp 'codelahoma-gtd-weekly-review-steps)
                     (> (length codelahoma-gtd-weekly-review-steps) 0)))
          results)
    
    ;; Report results
    (with-output-to-temp-buffer "*GTD Weekly Review Test Results*"
      (princ "GTD Weekly Review Test Results\n")
      (princ "==============================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-35s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*GTD Weekly Review Test Results*")))

;; Run the test
(test-gtd-weekly-review)

(provide 'test-weekly-review)
;;; test-weekly-review.el ends here