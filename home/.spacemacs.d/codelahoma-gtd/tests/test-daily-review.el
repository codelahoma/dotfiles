;;; test-daily-review.el --- Test GTD daily review functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying GTD daily review workflows

;;; Code:

(require 'codelahoma-gtd-daily-review)

(defun test-gtd-daily-review ()
  "Test GTD daily review functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check morning review function
    (push (cons "Morning review function exists" 
                (fboundp 'codelahoma-gtd-morning-review))
          results)
    
    ;; Test 2: Check evening review function
    (push (cons "Evening review function exists" 
                (fboundp 'codelahoma-gtd-evening-review))
          results)
    
    ;; Test 3: Check quick check functions
    (push (cons "Quick morning check exists" 
                (fboundp 'codelahoma-gtd-quick-morning-check))
          results)
    
    (push (cons "Quick evening check exists" 
                (fboundp 'codelahoma-gtd-quick-evening-check))
          results)
    
    ;; Test 4: Check review save functions
    (push (cons "Save daily review function exists" 
                (fboundp 'codelahoma-gtd-save-daily-review))
          results)
    
    ;; Test 5: Check utility functions
    (push (cons "Get completed today function exists" 
                (fboundp 'codelahoma-gtd-get-completed-today))
          results)
    
    (push (cons "Get priority tasks function exists" 
                (fboundp 'codelahoma-gtd-get-priority-tasks))
          results)
    
    ;; Test 6: Check review directory creation
    (let ((review-dir codelahoma-gtd-review-directory))
      (unless (file-exists-p review-dir)
        (make-directory review-dir t))
      (push (cons "Review directory exists" 
                  (file-exists-p review-dir))
            results))
    
    ;; Test 7: Test journal integration
    (push (cons "Daily journal entry function exists" 
                (fboundp 'codelahoma-gtd-daily-journal-entry))
          results)
    
    ;; Test 8: Test metrics recording
    (codelahoma-gtd-record-daily-metric 'test-metric 42)
    (push (cons "Daily metrics recording works" 
                (equal (alist-get 'test-metric 
                                (alist-get (format-time-string "%Y-%m-%d") 
                                         codelahoma-gtd-daily-metrics nil nil 'equal))
                     42))
          results)
    
    ;; Report results
    (with-output-to-temp-buffer "*GTD Daily Review Test Results*"
      (princ "GTD Daily Review Test Results\n")
      (princ "=============================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-35s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*GTD Daily Review Test Results*")))

;; Run the test
(test-gtd-daily-review)

(provide 'test-daily-review)
;;; test-daily-review.el ends here