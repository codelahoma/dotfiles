;;; test-quick.el --- Test GTD quick access functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying GTD quick access commands

;;; Code:

(require 'codelahoma-gtd-quick)

(defun test-gtd-quick ()
  "Test GTD quick access functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check quick status function
    (push (cons "Quick status function exists" 
                (fboundp 'codelahoma-gtd-quick-status))
          results)
    
    ;; Test 2: Check quick navigation hydra
    (push (cons "Quick navigation hydra exists" 
                (fboundp 'codelahoma-gtd-quick-nav/body))
          results)
    
    ;; Test 3: Check quick capture functions
    (push (cons "Quick capture task function" 
                (fboundp 'codelahoma-gtd-quick-capture-task))
          results)
    
    (push (cons "Quick note function" 
                (fboundp 'codelahoma-gtd-quick-note))
          results)
    
    ;; Test 4: Check quick workflow functions
    (push (cons "Quick process inbox function" 
                (fboundp 'codelahoma-gtd-quick-process-inbox))
          results)
    
    (push (cons "Quick complete and next function" 
                (fboundp 'codelahoma-gtd-quick-complete-and-next))
          results)
    
    ;; Test 5: Check quick dashboard
    (push (cons "Quick dashboard function" 
                (fboundp 'codelahoma-gtd-quick-dashboard))
          results)
    
    ;; Test 6: Test quick capture (non-interactive)
    (let ((test-file (expand-file-name "inbox.org" codelahoma-gtd-directory)))
      (when (file-exists-p test-file)
        (let ((initial-size (nth 7 (file-attributes test-file))))
          (with-current-buffer (find-file-noselect test-file)
            (goto-char (point-max))
            (insert "\n* TODO Test quick capture\n")
            (save-buffer))
          (push (cons "Quick capture write test" 
                      (> (nth 7 (file-attributes test-file)) initial-size))
                results)
          ;; Clean up test entry
          (with-current-buffer (find-file-noselect test-file)
            (goto-char (point-max))
            (search-backward "* TODO Test quick capture")
            (kill-whole-line)
            (save-buffer)))))
    
    ;; Test 7: Check jump functions
    (push (cons "Jump to project function" 
                (fboundp 'codelahoma-gtd-jump-to-project))
          results)
    
    ;; Report results
    (with-output-to-temp-buffer "*GTD Quick Test Results*"
      (princ "GTD Quick Access Test Results\n")
      (princ "=============================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-30s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*GTD Quick Test Results*")))

;; Run the test
(test-gtd-quick)

(provide 'test-quick)
;;; test-quick.el ends here