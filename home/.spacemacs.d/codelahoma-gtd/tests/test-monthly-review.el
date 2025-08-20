;;; test-monthly-review.el --- Test GTD monthly review functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying GTD monthly and quarterly review functionality

;;; Code:

(require 'codelahoma-gtd-monthly-review)

(defun test-gtd-monthly-review ()
  "Test GTD monthly review functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check monthly review functions
    (push (cons "Monthly review function exists" 
                (fboundp 'codelahoma-gtd-monthly-review))
          results)
    
    (push (cons "Execute monthly review exists" 
                (fboundp 'codelahoma-gtd-execute-monthly-review))
          results)
    
    ;; Test 2: Check quarterly review functions
    (push (cons "Quarterly review function exists" 
                (fboundp 'codelahoma-gtd-quarterly-review))
          results)
    
    (push (cons "Execute quarterly review exists" 
                (fboundp 'codelahoma-gtd-execute-quarterly-review))
          results)
    
    ;; Test 3: Check annual review
    (push (cons "Annual review function exists" 
                (fboundp 'codelahoma-gtd-annual-review))
          results)
    
    ;; Test 4: Check utility functions
    (push (cons "Count completed this month exists" 
                (fboundp 'codelahoma-gtd-count-completed-this-month))
          results)
    
    (push (cons "Count projects completed exists" 
                (fboundp 'codelahoma-gtd-count-projects-completed-this-month))
          results)
    
    ;; Test 5: Check metrics functions
    (push (cons "Calculate monthly score exists" 
                (fboundp 'codelahoma-gtd-calculate-monthly-score))
          results)
    
    (push (cons "Score interpretation exists" 
                (fboundp 'codelahoma-gtd-score-interpretation))
          results)
    
    ;; Test 6: Test quarter months calculation
    (let ((q1-months (codelahoma-gtd-quarter-months 1))
          (q4-months (codelahoma-gtd-quarter-months 4)))
      (push (cons "Quarter months calculation" 
                  (and (equal q1-months '(1 2 3))
                       (equal q4-months '(10 11 12))))
            results))
    
    ;; Test 7: Test score calculation
    (let ((score (codelahoma-gtd-calculate-monthly-score 25 3)))
      (push (cons "Monthly score calculation" 
                  (and (numberp score) (>= score 0) (<= score 100)))
            results))
    
    ;; Test 8: Test life areas configuration
    (push (cons "Life areas configured" 
                (and (boundp 'codelahoma-gtd-life-areas)
                     (> (length codelahoma-gtd-life-areas) 0)))
          results)
    
    ;; Test 9: Check save functions
    (push (cons "Save monthly review exists" 
                (fboundp 'codelahoma-gtd-save-monthly-review))
          results)
    
    (push (cons "Save quarterly review exists" 
                (fboundp 'codelahoma-gtd-save-quarterly-review))
          results)
    
    ;; Report results
    (with-output-to-temp-buffer "*GTD Monthly Review Test Results*"
      (princ "GTD Monthly Review Test Results\n")
      (princ "===============================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-35s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*GTD Monthly Review Test Results*")))

;; Run the test
(test-gtd-monthly-review)

(provide 'test-monthly-review)
;;; test-monthly-review.el ends here