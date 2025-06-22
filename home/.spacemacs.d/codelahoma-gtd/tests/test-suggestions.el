;;; test-suggestions.el --- Test smart knowledge suggestions -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying smart knowledge suggestion functionality

;;; Code:

(require 'codelahoma-bridge-suggestions)

(defun test-bridge-suggestions ()
  "Test smart knowledge suggestion functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check core suggestion functions
    (push (cons "Suggest related knowledge exists" 
                (fboundp 'codelahoma-bridge-suggest-related-knowledge))
          results)
    
    (push (cons "Analyze context exists" 
                (fboundp 'codelahoma-bridge-analyze-context))
          results)
    
    (push (cons "Find suggestions exists" 
                (fboundp 'codelahoma-bridge-find-suggestions))
          results)
    
    ;; Test 2: Check scoring functions
    (push (cons "Score suggestions exists" 
                (fboundp 'codelahoma-bridge-score-suggestions))
          results)
    
    (push (cons "Score title exists" 
                (fboundp 'codelahoma-bridge-score-title))
          results)
    
    (push (cons "Score tags exists" 
                (fboundp 'codelahoma-bridge-score-tags))
          results)
    
    (push (cons "Score recency exists" 
                (fboundp 'codelahoma-bridge-score-recency))
          results)
    
    (push (cons "Score frequency exists" 
                (fboundp 'codelahoma-bridge-score-frequency))
          results)
    
    ;; Test 3: Check utility functions
    (push (cons "Extract keywords exists" 
                (fboundp 'codelahoma-bridge-extract-keywords))
          results)
    
    (push (cons "Filter suggestions exists" 
                (fboundp 'codelahoma-bridge-filter-suggestions))
          results)
    
    (push (cons "Show suggestions exists" 
                (fboundp 'codelahoma-bridge-show-suggestions))
          results)
    
    ;; Test 4: Check learning functions
    (push (cons "Learn from link exists" 
                (fboundp 'codelahoma-bridge-learn-from-link))
          results)
    
    (push (cons "Learn from rejection exists" 
                (fboundp 'codelahoma-bridge-learn-from-rejection))
          results)
    
    (push (cons "Save learning data exists" 
                (fboundp 'codelahoma-bridge-save-learning-data))
          results)
    
    ;; Test 5: Check auto-suggest mode
    (push (cons "Auto-suggest mode exists" 
                (fboundp 'codelahoma-bridge-auto-suggest-mode))
          results)
    
    (push (cons "Check suggestions exists" 
                (fboundp 'codelahoma-bridge-check-suggestions))
          results)
    
    ;; Test 6: Check preset functions
    (push (cons "Suggest for project exists" 
                (fboundp 'codelahoma-bridge-suggest-for-project))
          results)
    
    (push (cons "Suggest by context exists" 
                (fboundp 'codelahoma-bridge-suggest-by-context))
          results)
    
    ;; Test 7: Test configuration
    (push (cons "Suggestion threshold configured" 
                (and (boundp 'codelahoma-bridge-suggestion-threshold)
                     (numberp codelahoma-bridge-suggestion-threshold)))
          results)
    
    (push (cons "Max suggestions configured" 
                (and (boundp 'codelahoma-bridge-max-suggestions)
                     (integerp codelahoma-bridge-max-suggestions)))
          results)
    
    (push (cons "Suggestion weights configured" 
                (and (boundp 'codelahoma-bridge-suggestion-weights)
                     (listp codelahoma-bridge-suggestion-weights)))
          results)
    
    ;; Test 8: Test keyword extraction
    (let ((keywords (codelahoma-bridge-extract-keywords 
                    "Implement the new feature for project management")))
      (push (cons "Keyword extraction works" 
                  (and (member "implement" keywords)
                       (member "feature" keywords)
                       (member "project" keywords)
                       (member "management" keywords)
                       (not (member "the" keywords))
                       (not (member "for" keywords))))
            results))
    
    ;; Test 9: Test context analysis
    (let ((context nil))
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task :work:coding:\n")
        (insert "This is a test task body\n")
        (goto-char (point-min))
        (setq context (codelahoma-bridge-analyze-context)))
      (push (cons "Context analysis works" 
                  (and (hash-table-p context)
                       (gethash 'heading context)
                       (gethash 'tags context)))
            results))
    
    ;; Test 10: Test scoring range
    (let ((score (codelahoma-bridge-score-recency 
                 (make-temp-file "test-node"))))
      (push (cons "Recency scoring works" 
                  (and (numberp score)
                       (>= score 0)
                       (<= score 1)))
            results))
    
    ;; Report results
    (with-output-to-temp-buffer "*Bridge Suggestions Test Results*"
      (princ "Smart Knowledge Suggestions Test Results\n")
      (princ "========================================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-35s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*Bridge Suggestions Test Results*")))

;; Run the test
(test-bridge-suggestions)

(provide 'test-suggestions)
;;; test-suggestions.el ends here