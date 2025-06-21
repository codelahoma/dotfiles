;;; test-bridge-projects.el --- Test project knowledge integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying project knowledge integration functionality

;;; Code:

(require 'codelahoma-bridge-projects)

(defun test-bridge-projects ()
  "Test project knowledge integration functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check project wiki functions
    (push (cons "Create project wiki exists" 
                (fboundp 'codelahoma-bridge-create-project-wiki))
          results)
    
    (push (cons "Open project wiki exists" 
                (fboundp 'codelahoma-bridge-open-project-wiki))
          results)
    
    ;; Test 2: Check reference management
    (push (cons "Add project reference exists" 
                (fboundp 'codelahoma-bridge-add-project-reference))
          results)
    
    (push (cons "Create reference note exists" 
                (fboundp 'codelahoma-bridge-create-reference-note))
          results)
    
    ;; Test 3: Check decision logging
    (push (cons "Decision log exists" 
                (fboundp 'codelahoma-bridge-decision-log))
          results)
    
    (push (cons "Add to wiki section exists" 
                (fboundp 'codelahoma-bridge-add-to-wiki-section))
          results)
    
    ;; Test 4: Check lessons learned
    (push (cons "Project lessons learned exists" 
                (fboundp 'codelahoma-bridge-project-lessons-learned))
          results)
    
    (push (cons "Save lessons learned exists" 
                (fboundp 'codelahoma-bridge-save-lessons-learned))
          results)
    
    (push (cons "Insert project decisions exists" 
                (fboundp 'codelahoma-bridge-insert-project-decisions))
          results)
    
    ;; Test 5: Check knowledge dashboard
    (push (cons "Project knowledge summary exists" 
                (fboundp 'codelahoma-bridge-project-knowledge-summary))
          results)
    
    (push (cons "List project references exists" 
                (fboundp 'codelahoma-bridge-list-project-references))
          results)
    
    (push (cons "List project decisions exists" 
                (fboundp 'codelahoma-bridge-list-project-decisions))
          results)
    
    ;; Test 6: Check helper functions
    (push (cons "Get project references exists" 
                (fboundp 'codelahoma-bridge-get-project-references))
          results)
    
    (push (cons "Count project references exists" 
                (fboundp 'codelahoma-bridge-count-project-references))
          results)
    
    (push (cons "Get project knowledge exists" 
                (fboundp 'codelahoma-bridge-get-project-knowledge))
          results)
    
    ;; Test 7: Test configuration
    (push (cons "Wiki template configured" 
                (and (boundp 'codelahoma-bridge-project-wiki-template)
                     (stringp codelahoma-bridge-project-wiki-template)))
          results)
    
    (push (cons "Decision template configured" 
                (and (boundp 'codelahoma-bridge-decision-template)
                     (stringp codelahoma-bridge-decision-template)))
          results)
    
    ;; Test 8: Test project detection
    (let ((is-project nil))
      (with-temp-buffer
        (org-mode)
        (insert "* PROJECT Test Project\n")
        (goto-char (point-min))
        (setq is-project (codelahoma-gtd-project-p)))
      (push (cons "Project detection works" is-project) results))
    
    ;; Test 9: Test ID generation
    (let ((id (org-id-new)))
      (push (cons "Can generate project IDs" 
                  (and (stringp id) (> (length id) 0)))
            results))
    
    ;; Test 10: Test template formatting
    (let ((formatted (format codelahoma-bridge-project-wiki-template
                           "Test project description")))
      (push (cons "Wiki template formatting works" 
                  (string-match "Test project description" formatted))
            results))
    
    ;; Report results
    (with-output-to-temp-buffer "*Bridge Projects Test Results*"
      (princ "Project Knowledge Integration Test Results\n")
      (princ "==========================================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-35s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*Bridge Projects Test Results*")))

;; Run the test
(test-bridge-projects)

(provide 'test-bridge-projects)
;;; test-bridge-projects.el ends here