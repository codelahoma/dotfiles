;;; test-bridge.el --- Test GTD-Zettelkasten bridge functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying bidirectional linking between GTD and Zettelkasten

;;; Code:

(require 'codelahoma-bridge)

(defun test-gtd-bridge ()
  "Test GTD-Zettelkasten bridge functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check core linking functions
    (push (cons "Link task to note exists" 
                (fboundp 'codelahoma-bridge-link-task-to-note))
          results)
    
    (push (cons "Extract tasks from note exists" 
                (fboundp 'codelahoma-bridge-extract-tasks-from-note))
          results)
    
    (push (cons "Navigate link exists" 
                (fboundp 'codelahoma-bridge-navigate-link))
          results)
    
    ;; Test 2: Check helper functions
    (push (cons "Find todos exists" 
                (fboundp 'codelahoma-bridge-find-todos))
          results)
    
    (push (cons "Add task backlink exists" 
                (fboundp 'codelahoma-bridge-add-task-backlink))
          results)
    
    (push (cons "Create linked tasks exists" 
                (fboundp 'codelahoma-bridge-create-linked-tasks))
          results)
    
    ;; Test 3: Check navigation functions
    (push (cons "Show linked tasks exists" 
                (fboundp 'codelahoma-bridge-show-linked-tasks))
          results)
    
    (push (cons "Find linked tasks exists" 
                (fboundp 'codelahoma-bridge-find-linked-tasks))
          results)
    
    (push (cons "Display linked tasks exists" 
                (fboundp 'codelahoma-bridge-display-linked-tasks))
          results)
    
    ;; Test 4: Check management functions
    (push (cons "Unlink task exists" 
                (fboundp 'codelahoma-bridge-unlink-task))
          results)
    
    (push (cons "Update links exists" 
                (fboundp 'codelahoma-bridge-update-links))
          results)
    
    (push (cons "Count links exists" 
                (fboundp 'codelahoma-bridge-count-links))
          results)
    
    ;; Test 5: Check quick access functions
    (push (cons "Create linked note exists" 
                (fboundp 'codelahoma-bridge-create-linked-note))
          results)
    
    (push (cons "Jump to linked exists" 
                (fboundp 'codelahoma-bridge-jump-to-linked))
          results)
    
    ;; Test 6: Check integration helpers
    (push (cons "Task has note predicate exists" 
                (fboundp 'codelahoma-bridge-task-has-note-p))
          results)
    
    (push (cons "Note has tasks predicate exists" 
                (fboundp 'codelahoma-bridge-note-has-tasks-p))
          results)
    
    ;; Test 7: Test configuration
    (push (cons "Auto-link configured" 
                (boundp 'codelahoma-bridge-auto-link))
          results)
    
    (push (cons "Backlink section configured" 
                (and (boundp 'codelahoma-bridge-backlink-section)
                     (stringp codelahoma-bridge-backlink-section)))
          results)
    
    (push (cons "Link property configured" 
                (and (boundp 'codelahoma-bridge-link-property)
                     (stringp codelahoma-bridge-link-property)))
          results)
    
    ;; Test 8: Test org-roam availability
    (push (cons "Org-roam loaded" 
                (featurep 'org-roam))
          results)
    
    (push (cons "Org-id loaded" 
                (featurep 'org-id))
          results)
    
    ;; Test 9: Create test task and verify structure
    (let ((test-passed nil))
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test task\n")
        (goto-char (point-min))
        (setq test-passed (org-entry-get nil "TODO")))
      (push (cons "Can create test task" test-passed) results))
    
    ;; Test 10: Find TODOs in buffer
    (let ((todos nil))
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Task 1\n** DONE Task 2\n* NEXT Task 3\n")
        (setq todos (codelahoma-bridge-find-todos)))
      (push (cons "Find todos works" 
                  (= (length todos) 2))
            results))
    
    ;; Report results
    (with-output-to-temp-buffer "*GTD Bridge Test Results*"
      (princ "GTD-Zettelkasten Bridge Test Results\n")
      (princ "====================================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-35s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*GTD Bridge Test Results*")))

;; Run the test
(test-gtd-bridge)

(provide 'test-bridge)
;;; test-bridge.el ends here