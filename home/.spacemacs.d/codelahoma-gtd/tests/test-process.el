;;; test-process.el --- Test GTD processing functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying GTD inbox processing workflow

;;; Code:

(require 'codelahoma-gtd-process)

(defun test-gtd-process ()
  "Test GTD processing functionality."
  (interactive)
  (let ((results '())
        (test-inbox "/tmp/test-process-inbox.org"))
    
    ;; Create test inbox file with sample items
    (with-temp-file test-inbox
      (insert "#+TITLE: Test Inbox\n\n")
      (insert "* Call John about project :PHONE:\n")
      (insert "* Follow up on proposal\n")
      (insert "* Someday learn Spanish\n")
      (insert "* Email response to client\n")
      (insert "* Project planning meeting\n"))
    
    ;; Test 1: Check refile targets are configured
    (push (cons "Refile targets defined" 
                (and codelahoma-gtd-refile-targets
                     (> (length codelahoma-gtd-refile-targets) 0)))
          results)
    
    ;; Test 2: Check processing mode keymap
    (push (cons "Processing mode keymap exists" 
                (keymapp codelahoma-gtd-processing-mode-map))
          results)
    
    ;; Test 3: Test refile suggestions
    (with-temp-buffer
      (org-mode)
      (insert "* Project planning meeting")
      (goto-char (point-min))
      (let ((suggestion (codelahoma-gtd-suggest-refile-target 
                        "Project planning meeting")))
        (push (cons "Suggest project refile" 
                    (string= suggestion "projects.org"))
              results)))
    
    ;; Test 4: Test inbox item counting
    (with-current-buffer (find-file-noselect test-inbox)
      (push (cons "Count inbox items" 
                  (= (codelahoma-gtd-total-inbox-items) 5))
            results)
      (kill-buffer))
    
    ;; Test 5: Check processing mode can be enabled
    (with-current-buffer (find-file-noselect test-inbox)
      (codelahoma-gtd-processing-mode 1)
      (push (cons "Processing mode enabled" 
                  codelahoma-gtd-processing-mode)
            results)
      (codelahoma-gtd-processing-mode -1)
      (kill-buffer))
    
    ;; Clean up
    (delete-file test-inbox)
    
    ;; Report results
    (with-output-to-temp-buffer "*GTD Process Test Results*"
      (princ "GTD Process Test Results\n")
      (princ "========================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-30s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*GTD Process Test Results*")))

;; Run the test
(test-gtd-process)

(provide 'test-process)
;;; test-process.el ends here