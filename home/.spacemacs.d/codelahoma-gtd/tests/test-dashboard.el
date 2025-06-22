;;; test-dashboard.el --- Test unified dashboard functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying unified dashboard functionality

;;; Code:

(require 'codelahoma-dashboard)

(defun test-dashboard ()
  "Test unified dashboard functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check main dashboard functions
    (push (cons "Dashboard function exists" 
                (fboundp 'codelahoma-dashboard))
          results)
    
    (push (cons "Dashboard refresh exists" 
                (fboundp 'codelahoma-dashboard-refresh))
          results)
    
    (push (cons "Dashboard mode exists" 
                (fboundp 'codelahoma-dashboard-mode))
          results)
    
    ;; Test 2: Check section insertion functions
    (push (cons "Insert header exists" 
                (fboundp 'codelahoma-dashboard-insert-header))
          results)
    
    (push (cons "Insert overview exists" 
                (fboundp 'codelahoma-dashboard-insert-overview))
          results)
    
    (push (cons "Insert inbox status exists" 
                (fboundp 'codelahoma-dashboard-insert-inbox-status))
          results)
    
    (push (cons "Insert next actions exists" 
                (fboundp 'codelahoma-dashboard-insert-next-actions))
          results)
    
    (push (cons "Insert projects exists" 
                (fboundp 'codelahoma-dashboard-insert-projects))
          results)
    
    (push (cons "Insert recent notes exists" 
                (fboundp 'codelahoma-dashboard-insert-recent-notes))
          results)
    
    (push (cons "Insert metrics exists" 
                (fboundp 'codelahoma-dashboard-insert-metrics))
          results)
    
    (push (cons "Insert quick links exists" 
                (fboundp 'codelahoma-dashboard-insert-quick-links))
          results)
    
    ;; Test 3: Check counting functions
    (push (cons "Count next actions exists" 
                (fboundp 'codelahoma-dashboard-count-next-actions))
          results)
    
    (push (cons "Count waiting for exists" 
                (fboundp 'codelahoma-dashboard-count-waiting-for))
          results)
    
    (push (cons "Count active projects exists" 
                (fboundp 'codelahoma-dashboard-count-active-projects))
          results)
    
    (push (cons "Count task links exists" 
                (fboundp 'codelahoma-dashboard-count-task-links))
          results)
    
    (push (cons "Count completed today exists" 
                (fboundp 'codelahoma-dashboard-count-completed-today))
          results)
    
    ;; Test 4: Check getter functions
    (push (cons "Get inbox items exists" 
                (fboundp 'codelahoma-dashboard-get-inbox-items))
          results)
    
    (push (cons "Get next actions exists" 
                (fboundp 'codelahoma-dashboard-get-next-actions))
          results)
    
    (push (cons "Get active projects exists" 
                (fboundp 'codelahoma-dashboard-get-active-projects))
          results)
    
    ;; Test 5: Check auto-refresh functions
    (push (cons "Start refresh timer exists" 
                (fboundp 'codelahoma-dashboard-start-refresh-timer))
          results)
    
    (push (cons "Stop refresh timer exists" 
                (fboundp 'codelahoma-dashboard-stop-refresh-timer))
          results)
    
    (push (cons "Refresh if visible exists" 
                (fboundp 'codelahoma-dashboard-refresh-if-visible))
          results)
    
    ;; Test 6: Check helper functions
    (push (cons "Widget header exists" 
                (fboundp 'codelahoma-dashboard-widget-header))
          results)
    
    (push (cons "Insert stat exists" 
                (fboundp 'codelahoma-dashboard-insert-stat))
          results)
    
    (push (cons "Insert link exists" 
                (fboundp 'codelahoma-dashboard-insert-link))
          results)
    
    ;; Test 7: Test configuration
    (push (cons "Dashboard sections configured" 
                (and (boundp 'codelahoma-dashboard-sections)
                     (listp codelahoma-dashboard-sections)
                     (> (length codelahoma-dashboard-sections) 0)))
          results)
    
    (push (cons "Auto refresh configured" 
                (and (boundp 'codelahoma-dashboard-auto-refresh)
                     (booleanp codelahoma-dashboard-auto-refresh)))
          results)
    
    (push (cons "Refresh interval configured" 
                (and (boundp 'codelahoma-dashboard-refresh-interval)
                     (integerp codelahoma-dashboard-refresh-interval)))
          results)
    
    ;; Test 8: Test buffer name
    (push (cons "Buffer name defined" 
                (and (boundp 'codelahoma-dashboard-buffer-name)
                     (stringp codelahoma-dashboard-buffer-name)))
          results)
    
    ;; Test 9: Test dashboard creation
    (let ((created nil))
      (save-window-excursion
        (codelahoma-dashboard)
        (setq created (string= (buffer-name) "*GTD-Zettelkasten Dashboard*"))
        (when created
          (kill-buffer)))
      (push (cons "Can create dashboard buffer" created) results))
    
    ;; Test 10: Test mode map
    (push (cons "Dashboard mode map exists" 
                (and (boundp 'codelahoma-dashboard-mode-map)
                     (keymapp codelahoma-dashboard-mode-map)))
          results)
    
    ;; Report results
    (with-output-to-temp-buffer "*Dashboard Test Results*"
      (princ "Dashboard Test Results\n")
      (princ "======================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-35s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*Dashboard Test Results*")))

;; Run the test
(test-dashboard)

(provide 'test-dashboard)
;;; test-dashboard.el ends here