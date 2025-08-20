;;; test-decision.el --- Test GTD decision flow functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying GTD decision support functions

;;; Code:

(require 'codelahoma-gtd-core)

(defun test-gtd-decision ()
  "Test GTD decision flow functionality."
  (interactive)
  (let ((results '())
        (test-file "/tmp/test-decision.org"))
    
    ;; Create test file with sample tasks
    (with-temp-file test-file
      (insert "#+TITLE: Test Tasks\n\n")
      (insert "* TODO Task with deadline\nDEADLINE: <2025-01-25>\n")
      (insert "* NEXT High priority task\n")
      (insert ":PROPERTIES:\n:CONTEXT: @computer\n:Effort: 30m\n:END:\n")
      (insert "* TODO Low energy task\n")
      (insert ":PROPERTIES:\n:ENERGY_REQUIRED: low\n:END:\n"))
    
    ;; Test 1: Check smart next action function exists
    (push (cons "Smart next action function" 
                (fboundp 'codelahoma-gtd-smart-next-action))
          results)
    
    ;; Test 2: Check decision tree function exists
    (push (cons "Decision tree function" 
                (fboundp 'codelahoma-gtd-decision-tree))
          results)
    
    ;; Test 3: Test effort conversion
    (push (cons "Effort to minutes - 2h" 
                (= (codelahoma-gtd-effort-to-minutes "2h") 120))
          results)
    
    (push (cons "Effort to minutes - 30m" 
                (= (codelahoma-gtd-effort-to-minutes "30m") 30))
          results)
    
    ;; Test 4: Test available time estimation
    (push (cons "Available time estimate" 
                (numberp (codelahoma-gtd-estimate-available-time)))
          results)
    
    ;; Test 5: Test overdue counting
    (push (cons "Overdue count function" 
                (numberp (codelahoma-gtd-count-overdue)))
          results)
    
    ;; Clean up
    (delete-file test-file)
    
    ;; Report results
    (with-output-to-temp-buffer "*GTD Decision Test Results*"
      (princ "GTD Decision Flow Test Results\n")
      (princ "==============================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-30s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*GTD Decision Test Results*")))

;; Run the test
(test-gtd-decision)

(provide 'test-decision)
;;; test-decision.el ends here