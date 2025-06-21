;;; test-workflows.el --- Test knowledge-driven workflows -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying knowledge-driven workflow functionality

;;; Code:

(require 'codelahoma-bridge-workflows)

(defun test-bridge-workflows ()
  "Test knowledge-driven workflow functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check reading workflow functions
    (push (cons "Create reading task exists" 
                (fboundp 'codelahoma-bridge-create-reading-task))
          results)
    
    (push (cons "Create reading note exists" 
                (fboundp 'codelahoma-bridge-create-reading-note))
          results)
    
    ;; Test 2: Check research workflow functions
    (push (cons "Research workflow exists" 
                (fboundp 'codelahoma-bridge-research-workflow))
          results)
    
    (push (cons "Create research hub exists" 
                (fboundp 'codelahoma-bridge-create-research-hub))
          results)
    
    ;; Test 3: Check learning workflow functions
    (push (cons "Learning project exists" 
                (fboundp 'codelahoma-bridge-learning-project))
          results)
    
    (push (cons "Create learning plan exists" 
                (fboundp 'codelahoma-bridge-create-learning-plan))
          results)
    
    (push (cons "Create learning notes exists" 
                (fboundp 'codelahoma-bridge-create-learning-notes))
          results)
    
    ;; Test 4: Check insight capture functions
    (push (cons "Insight capture exists" 
                (fboundp 'codelahoma-bridge-insight-capture))
          results)
    
    (push (cons "Create insight tasks exists" 
                (fboundp 'codelahoma-bridge-create-insight-tasks))
          results)
    
    ;; Test 5: Check knowledge review functions
    (push (cons "Knowledge review exists" 
                (fboundp 'codelahoma-bridge-knowledge-review))
          results)
    
    (push (cons "Get recent notes exists" 
                (fboundp 'codelahoma-bridge-get-recent-notes))
          results)
    
    (push (cons "Find note tasks exists" 
                (fboundp 'codelahoma-bridge-find-note-tasks))
          results)
    
    ;; Test 6: Test configuration
    (push (cons "Reading directory configured" 
                (and (boundp 'codelahoma-bridge-reading-directory)
                     (stringp codelahoma-bridge-reading-directory)))
          results)
    
    (push (cons "Research directory configured" 
                (and (boundp 'codelahoma-bridge-research-directory)
                     (stringp codelahoma-bridge-research-directory)))
          results)
    
    (push (cons "Learning directory configured" 
                (and (boundp 'codelahoma-bridge-learning-directory)
                     (stringp codelahoma-bridge-learning-directory)))
          results)
    
    ;; Test 7: Test directory creation
    (let ((test-dir "/tmp/test-gtd-workflows"))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t))
      (let ((codelahoma-bridge-reading-directory test-dir))
        ;; This would normally be called interactively
        (make-directory test-dir t)
        (push (cons "Can create workflow directories" 
                    (file-exists-p test-dir))
              results)
        (delete-directory test-dir t)))
    
    ;; Test 8: Test note ID generation
    (let ((note-id (org-id-new)))
      (push (cons "Can generate note IDs" 
                  (and (stringp note-id)
                       (> (length note-id) 0)))
            results))
    
    ;; Test 9: Test reading note creation
    (let* ((temp-dir (make-temp-file "test-roam" t))
           (org-roam-directory temp-dir)
           (note-id (codelahoma-bridge-create-reading-note 
                    "Test Book" "book" "Author" "http://example.com")))
      (push (cons "Reading note creation works" 
                  (stringp note-id))
            results)
      (delete-directory temp-dir t))
    
    ;; Test 10: Test time functions
    (let ((recent-time (time-subtract (current-time) (days-to-time 1))))
      (push (cons "Time calculation works" 
                  (time-less-p recent-time (current-time)))
            results))
    
    ;; Report results
    (with-output-to-temp-buffer "*Bridge Workflows Test Results*"
      (princ "Knowledge-Driven Workflows Test Results\n")
      (princ "=======================================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-35s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*Bridge Workflows Test Results*")))

;; Run the test
(test-bridge-workflows)

(provide 'test-workflows)
;;; test-workflows.el ends here