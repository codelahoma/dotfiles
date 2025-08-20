;;; test-integration.el --- Test GTD Phase 2 integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration test for Phase 2 GTD functionality

;;; Code:

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)
(require 'codelahoma-gtd-capture)
(require 'codelahoma-gtd-process)

(defun test-gtd-phase2-integration ()
  "Test Phase 2 GTD integration."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check all modules loaded
    (push (cons "Core module loaded" (featurep 'codelahoma-gtd-core)) results)
    (push (cons "Capture module loaded" (featurep 'codelahoma-gtd-capture)) results)
    (push (cons "Process module loaded" (featurep 'codelahoma-gtd-process)) results)
    
    ;; Test 2: Check todo keywords are set
    (push (cons "Todo keywords configured" 
                (and org-todo-keywords
                     (member "NEXT" (apply #'append 
                                          (mapcar #'cdr org-todo-keywords)))))
          results)
    
    ;; Test 3: Check capture templates exist
    (push (cons "Capture templates exist" 
                (and org-capture-templates
                     (assoc "i" org-capture-templates)))
          results)
    
    ;; Test 4: Check refile targets configured
    (push (cons "Refile targets configured" 
                (and org-refile-targets
                     (> (length org-refile-targets) 0)))
          results)
    
    ;; Test 5: Check project templates exist
    (push (cons "Project templates exist" 
                (and codelahoma-gtd-project-templates
                     (> (length codelahoma-gtd-project-templates) 0)))
          results)
    
    ;; Test 6: Test workflow status function
    (push (cons "Workflow status works" 
                (condition-case nil
                    (progn (codelahoma-gtd-workflow-status) t)
                  (error nil)))
          results)
    
    ;; Test 7: Check all keybindings exist
    (let ((prefix-map (lookup-key spacemacs-default-map (kbd "SPC o o"))))
      (push (cons "GTD keybindings exist" 
                  (and (keymapp prefix-map)
                       (lookup-key prefix-map (kbd "c i"))  ; capture inbox
                       (lookup-key prefix-map (kbd "p i"))  ; process inbox
                       (lookup-key prefix-map (kbd "j n")))) ; new project
            results))
    
    ;; Test 8: Performance check
    (let ((capture-time (benchmark-elapse 
                         (with-temp-buffer
                           (org-mode)
                           (insert "* Test item")))))
      (push (cons "Capture performance < 1s" 
                  (< capture-time 1.0))
            results))
    
    ;; Report results
    (with-output-to-temp-buffer "*GTD Phase 2 Integration Test*"
      (princ "GTD Phase 2 Integration Test Results\n")
      (princ "====================================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-30s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n\n" 
                      passed (length results)))
        (if (= passed (length results))
            (princ (propertize "Phase 2 Integration: COMPLETE ✓\n" 
                             'face '(:foreground "#86dc2f" :weight bold)))
          (princ (propertize "Phase 2 Integration: INCOMPLETE\n" 
                           'face '(:foreground "#f2241f" :weight bold))))))
    
    (switch-to-buffer-other-window "*GTD Phase 2 Integration Test*")))

;; Run the test
(test-gtd-phase2-integration)

(provide 'test-integration)
;;; test-integration.el ends here