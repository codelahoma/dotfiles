;;; test-capture.el --- Test GTD capture functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying GTD capture system

;;; Code:

(require 'codelahoma-gtd-capture)

(defun test-gtd-capture ()
  "Test GTD capture functionality."
  (interactive)
  (let ((results '())
        (test-inbox "/tmp/test-inbox.org"))
    
    ;; Create test inbox file
    (with-temp-file test-inbox
      (insert "#+TITLE: Test Inbox\n\n"))
    
    ;; Test 1: Check capture templates are defined
    (push (cons "Capture templates defined" 
                (assoc "i" codelahoma-gtd-capture-templates))
          results)
    
    ;; Test 2: Check context detection
    (with-temp-buffer
      (emacs-lisp-mode)
      (push (cons "Context detection in prog-mode" 
                  (string-match-p "coding:" (or (codelahoma-gtd-detect-context) "")))
            results))
    
    ;; Test 3: Check auto-tagging
    (with-temp-buffer
      (org-mode)
      (insert "* Call John about project")
      (goto-char (point-min))
      (codelahoma-gtd-auto-tag)
      (push (cons "Auto-tag phone call" 
                  (member "PHONE" (org-get-tags)))
            results))
    
    ;; Test 4: Check all capture templates exist
    (dolist (template-key '("i" "t" "p" "m" "e" "w"))
      (push (cons (format "Template '%s' exists" template-key)
                  (assoc template-key codelahoma-gtd-capture-templates))
            results))
    
    ;; Clean up
    (delete-file test-inbox)
    
    ;; Report results
    (with-output-to-temp-buffer "*GTD Capture Test Results*"
      (princ "GTD Capture Test Results\n")
      (princ "========================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-30s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*GTD Capture Test Results*")))

;; Run the test
(test-gtd-capture)

(provide 'test-capture)
;;; test-capture.el ends here