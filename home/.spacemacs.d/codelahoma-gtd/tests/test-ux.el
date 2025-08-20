;;; test-ux.el --- Test UX polish functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying UX enhancement functionality

;;; Code:

(require 'codelahoma-ux-polish)

(defun test-ux ()
  "Test UX polish functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check setup functions
    (push (cons "UX setup exists" 
                (fboundp 'codelahoma-ux-setup))
          results)
    
    (push (cons "Setup tooltips exists" 
                (fboundp 'codelahoma-ux-setup-tooltips))
          results)
    
    (push (cons "Setup confirmations exists" 
                (fboundp 'codelahoma-ux-setup-confirmations))
          results)
    
    (push (cons "Setup help exists" 
                (fboundp 'codelahoma-ux-setup-help))
          results)
    
    ;; Test 2: Check onboarding functions
    (push (cons "First run check exists" 
                (fboundp 'codelahoma-ux-first-run-p))
          results)
    
    (push (cons "Onboarding exists" 
                (fboundp 'codelahoma-ux-onboarding))
          results)
    
    (push (cons "Setup files exists" 
                (fboundp 'codelahoma-ux-setup-files))
          results)
    
    (push (cons "Setup contexts exists" 
                (fboundp 'codelahoma-ux-setup-contexts))
          results)
    
    (push (cons "Show essential commands exists" 
                (fboundp 'codelahoma-ux-show-essential-commands))
          results)
    
    ;; Test 3: Check tutorial functions
    (push (cons "Interactive tutorial exists" 
                (fboundp 'codelahoma-ux-interactive-tutorial))
          results)
    
    (push (cons "Tutorial next step exists" 
                (fboundp 'codelahoma-ux-tutorial-next-step))
          results)
    
    (push (cons "Tutorial complete exists" 
                (fboundp 'codelahoma-ux-tutorial-complete))
          results)
    
    ;; Test 4: Check undo/redo functions
    (push (cons "Push undo exists" 
                (fboundp 'codelahoma-ux-push-undo))
          results)
    
    (push (cons "Undo exists" 
                (fboundp 'codelahoma-ux-undo))
          results)
    
    (push (cons "Redo exists" 
                (fboundp 'codelahoma-ux-redo))
          results)
    
    ;; Test 5: Check confirmation functions
    (push (cons "Confirm destructive exists" 
                (fboundp 'codelahoma-ux-confirm-destructive))
          results)
    
    (push (cons "Confirm wrapper exists" 
                (fboundp 'codelahoma-ux-confirm-wrapper))
          results)
    
    ;; Test 6: Check help functions
    (push (cons "Help exists" 
                (fboundp 'codelahoma-ux-help))
          results)
    
    (push (cons "Current context exists" 
                (fboundp 'codelahoma-ux-current-context))
          results)
    
    ;; Test 7: Check notification functions
    (push (cons "Notify exists" 
                (fboundp 'codelahoma-ux-notify))
          results)
    
    (push (cons "With progress exists" 
                (fboundp 'codelahoma-ux-with-progress))
          results)
    
    (push (cons "Safe operation exists" 
                (fboundp 'codelahoma-ux-safe-operation))
          results)
    
    ;; Test 8: Check welcome screen
    (push (cons "Welcome screen exists" 
                (fboundp 'codelahoma-ux-welcome-screen))
          results)
    
    (push (cons "Time greeting exists" 
                (fboundp 'codelahoma-ux-time-greeting))
          results)
    
    ;; Test 9: Test configuration
    (push (cons "Enable tooltips configured" 
                (and (boundp 'codelahoma-ux-enable-tooltips)
                     (booleanp codelahoma-ux-enable-tooltips)))
          results)
    
    (push (cons "Enable confirmations configured" 
                (and (boundp 'codelahoma-ux-enable-confirmations)
                     (booleanp codelahoma-ux-enable-confirmations)))
          results)
    
    (push (cons "Undo limit configured" 
                (and (boundp 'codelahoma-ux-undo-limit)
                     (integerp codelahoma-ux-undo-limit)))
          results)
    
    (push (cons "Show welcome configured" 
                (and (boundp 'codelahoma-ux-show-welcome-on-startup)
                     (booleanp codelahoma-ux-show-welcome-on-startup)))
          results)
    
    ;; Test 10: Test operation struct
    (let ((op (make-codelahoma-ux-operation
               :description "Test"
               :undo-fn (lambda () t)
               :redo-fn (lambda () t)
               :timestamp (current-time))))
      (push (cons "Operation struct works" 
                  (and (codelahoma-ux-operation-p op)
                       (string= (codelahoma-ux-operation-description op) "Test")))
            results))
    
    ;; Test 11: Test time greeting
    (push (cons "Time greeting works" 
                (member (codelahoma-ux-time-greeting) 
                       '("morning" "afternoon" "evening")))
          results)
    
    ;; Report results
    (with-output-to-temp-buffer "*UX Polish Test Results*"
      (princ "UX Polish Test Results\n")
      (princ "======================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-35s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*UX Polish Test Results*")))

;; Run the test
(test-ux)

(provide 'test-ux)
;;; test-ux.el ends here