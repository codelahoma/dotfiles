;;; test-contexts.el --- Test GTD context functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying GTD context system

;;; Code:

(require 'codelahoma-gtd-contexts)

(defun test-gtd-contexts ()
  "Test GTD context functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check contexts are defined
    (push (cons "Contexts defined" 
                (and codelahoma-gtd-contexts
                     (assoc "@computer" codelahoma-gtd-contexts)))
          results)
    
    ;; Test 2: Check time contexts
    (push (cons "Time contexts defined" 
                (and codelahoma-gtd-time-contexts
                     (assoc "@morning" codelahoma-gtd-time-contexts)))
          results)
    
    ;; Test 3: Test current contexts detection
    (let ((contexts (codelahoma-gtd-current-contexts)))
      (push (cons "Current contexts include @anywhere" 
                  (member "@anywhere" contexts))
            results))
    
    ;; Test 4: Check energy levels
    (push (cons "Energy levels defined" 
                (and codelahoma-gtd-energy-levels
                     (assoc "high" codelahoma-gtd-energy-levels)))
          results)
    
    ;; Test 5: Test energy matching
    (push (cons "Energy match function" 
                (codelahoma-gtd-energy-match-p "high" "medium"))
          results)
    
    ;; Test 6: Context indicator format
    (push (cons "Context indicator" 
                (stringp (codelahoma-gtd-context-indicator)))
          results)
    
    ;; Report results
    (with-output-to-temp-buffer "*GTD Contexts Test Results*"
      (princ "GTD Contexts Test Results\n")
      (princ "=========================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-30s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*GTD Contexts Test Results*")))

;; Run the test
(test-gtd-contexts)

(provide 'test-contexts)
;;; test-contexts.el ends here