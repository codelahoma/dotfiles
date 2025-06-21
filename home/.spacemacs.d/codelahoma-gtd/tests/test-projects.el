;;; test-projects.el --- Test GTD project functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying GTD project and area structure

;;; Code:

(require 'codelahoma-gtd-core)

(defun test-gtd-projects ()
  "Test GTD project functionality."
  (interactive)
  (let ((results '())
        (test-file "/tmp/test-projects.org"))
    
    ;; Create test file
    (with-temp-file test-file
      (insert "#+TITLE: Test Projects\n\n"))
    
    ;; Test 1: Check areas of focus are defined
    (push (cons "Areas of focus defined" 
                (and codelahoma-gtd-areas-of-focus
                     (> (length codelahoma-gtd-areas-of-focus) 0)))
          results)
    
    ;; Test 2: Check project properties are defined
    (push (cons "Project properties defined" 
                (and codelahoma-gtd-project-properties
                     (member "STATUS" codelahoma-gtd-project-properties)))
          results)
    
    ;; Test 3: Test area to tag conversion
    (push (cons "Area to tag conversion" 
                (string= (codelahoma-gtd-area-to-tag "Health & Fitness") 
                        "HEALTH_FITNESS"))
          results)
    
    ;; Test 4: Check project templates exist
    (push (cons "Project templates defined" 
                (and codelahoma-gtd-project-templates
                     (assoc "software" codelahoma-gtd-project-templates)))
          results)
    
    ;; Test 5: Test project template sections
    (let ((software-template (alist-get "software" codelahoma-gtd-project-templates 
                                       nil nil #'string=)))
      (push (cons "Software template has sections" 
                  (and software-template
                       (member "Technical Design" 
                              (alist-get 'sections software-template))))
            results))
    
    ;; Clean up
    (delete-file test-file)
    
    ;; Report results
    (with-output-to-temp-buffer "*GTD Projects Test Results*"
      (princ "GTD Projects Test Results\n")
      (princ "=========================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-30s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*GTD Projects Test Results*")))

;; Run the test
(test-gtd-projects)

(provide 'test-projects)
;;; test-projects.el ends here