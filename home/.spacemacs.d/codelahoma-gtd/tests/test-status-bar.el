;;; test-status-bar.el --- Test status bar functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying status bar integration functionality

;;; Code:

(require 'codelahoma-status-bar)

(defun test-status-bar ()
  "Test status bar functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check main functions
    (push (cons "Status bar enable exists" 
                (fboundp 'codelahoma-status-bar-enable))
          results)
    
    (push (cons "Status bar disable exists" 
                (fboundp 'codelahoma-status-bar-disable))
          results)
    
    (push (cons "Status bar update exists" 
                (fboundp 'codelahoma-status-bar-update))
          results)
    
    ;; Test 2: Check component functions
    (push (cons "Inbox component exists" 
                (fboundp 'codelahoma-status-bar-inbox))
          results)
    
    (push (cons "Next actions component exists" 
                (fboundp 'codelahoma-status-bar-next-actions))
          results)
    
    (push (cons "Context component exists" 
                (fboundp 'codelahoma-status-bar-context))
          results)
    
    (push (cons "Daily progress component exists" 
                (fboundp 'codelahoma-status-bar-daily-progress))
          results)
    
    ;; Test 3: Check keymap functions
    (push (cons "Inbox map exists" 
                (fboundp 'codelahoma-status-bar-inbox-map))
          results)
    
    (push (cons "Next actions map exists" 
                (fboundp 'codelahoma-status-bar-next-actions-map))
          results)
    
    (push (cons "Context map exists" 
                (fboundp 'codelahoma-status-bar-context-map))
          results)
    
    (push (cons "Progress map exists" 
                (fboundp 'codelahoma-status-bar-progress-map))
          results)
    
    ;; Test 4: Check helper functions
    (push (cons "Get cached exists" 
                (fboundp 'codelahoma-status-bar-get-cached))
          results)
    
    (push (cons "Count next actions exists" 
                (fboundp 'codelahoma-status-bar-count-next-actions))
          results)
    
    (push (cons "Count completed today exists" 
                (fboundp 'codelahoma-status-bar-count-completed-today))
          results)
    
    ;; Test 5: Check notification functions
    (push (cons "Notify exists" 
                (fboundp 'codelahoma-status-bar-notify))
          results)
    
    (push (cons "Task completed hook exists" 
                (fboundp 'codelahoma-status-bar-task-completed))
          results)
    
    (push (cons "Item captured hook exists" 
                (fboundp 'codelahoma-status-bar-item-captured))
          results)
    
    (push (cons "Context changed hook exists" 
                (fboundp 'codelahoma-status-bar-context-changed))
          results)
    
    ;; Test 6: Check sparkline functions
    (push (cons "Sparkline exists" 
                (fboundp 'codelahoma-status-bar-sparkline))
          results)
    
    (push (cons "Weekly sparkline exists" 
                (fboundp 'codelahoma-status-bar-weekly-sparkline))
          results)
    
    ;; Test 7: Test configuration
    (push (cons "Enabled configured" 
                (and (boundp 'codelahoma-status-bar-enabled)
                     (booleanp codelahoma-status-bar-enabled)))
          results)
    
    (push (cons "Update interval configured" 
                (and (boundp 'codelahoma-status-bar-update-interval)
                     (integerp codelahoma-status-bar-update-interval)))
          results)
    
    (push (cons "Format configured" 
                (and (boundp 'codelahoma-status-bar-format)
                     (listp codelahoma-status-bar-format)))
          results)
    
    (push (cons "Warning threshold configured" 
                (and (boundp 'codelahoma-status-bar-inbox-warning-threshold)
                     (integerp codelahoma-status-bar-inbox-warning-threshold)))
          results)
    
    ;; Test 8: Test mode line string
    (push (cons "Mode line string defined" 
                (boundp 'codelahoma-mode-line-string))
          results)
    
    ;; Test 9: Test minor mode
    (push (cons "Minor mode defined" 
                (fboundp 'codelahoma-status-bar-mode))
          results)
    
    ;; Test 10: Test sparkline generation
    (let ((sparkline (codelahoma-status-bar-sparkline '(1 3 5 7 9) 10 5)))
      (push (cons "Sparkline generation works" 
                  (and (stringp sparkline)
                       (> (length sparkline) 0)))
            results))
    
    ;; Report results
    (with-output-to-temp-buffer "*Status Bar Test Results*"
      (princ "Status Bar Test Results\n")
      (princ "=======================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-35s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*Status Bar Test Results*")))

;; Run the test
(test-status-bar)

(provide 'test-status-bar)
;;; test-status-bar.el ends here