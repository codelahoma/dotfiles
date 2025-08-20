;;; test-agenda.el --- Test GTD agenda functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying GTD custom agenda views

;;; Code:

(require 'codelahoma-gtd-agenda)

(defun test-gtd-agenda ()
  "Test GTD agenda functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check agenda files function
    (push (cons "Agenda files function exists" 
                (fboundp 'codelahoma-gtd-agenda-files))
          results)
    
    ;; Test 2: Check custom commands configured
    (push (cons "Custom agenda commands setup" 
                (and (boundp 'org-agenda-custom-commands)
                     (assoc "g" org-agenda-custom-commands)))
          results)
    
    ;; Test 3: Check daily dashboard function
    (push (cons "Daily dashboard function exists" 
                (fboundp 'codelahoma-gtd-daily-dashboard))
          results)
    
    ;; Test 4: Check focus session function
    (push (cons "Focus session function exists" 
                (fboundp 'codelahoma-gtd-focus-session))
          results)
    
    ;; Test 5: Check morning review function
    (push (cons "Morning review function exists" 
                (fboundp 'codelahoma-gtd-morning-review))
          results)
    
    ;; Test 6: Check agenda configuration
    (codelahoma-gtd-configure-agenda)
    (push (cons "Agenda configuration applied" 
                (eq org-agenda-window-setup 'current-window))
          results)
    
    ;; Report results
    (with-output-to-temp-buffer "*GTD Agenda Test Results*"
      (princ "GTD Agenda Test Results\n")
      (princ "=======================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-30s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*GTD Agenda Test Results*")))

;; Run the test
(test-gtd-agenda)

(provide 'test-agenda)
;;; test-agenda.el ends here