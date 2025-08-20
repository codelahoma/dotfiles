;;; test-command-palette.el --- Test command palette functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying command palette functionality

;;; Code:

(require 'codelahoma-command-palette)

(defun test-command-palette ()
  "Test command palette functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check main functions
    (push (cons "Command palette exists" 
                (fboundp 'codelahoma-command-palette))
          results)
    
    (push (cons "Command register exists" 
                (fboundp 'codelahoma-command-register))
          results)
    
    (push (cons "Command register all exists" 
                (fboundp 'codelahoma-command-register-all))
          results)
    
    ;; Test 2: Check retrieval functions
    (push (cons "Get all commands exists" 
                (fboundp 'codelahoma-command-get-all))
          results)
    
    (push (cons "Get capture templates exists" 
                (fboundp 'codelahoma-command-get-capture-templates))
          results)
    
    (push (cons "Get agenda views exists" 
                (fboundp 'codelahoma-command-get-agenda-views))
          results)
    
    (push (cons "Get recent files exists" 
                (fboundp 'codelahoma-command-get-recent-files))
          results)
    
    ;; Test 3: Check formatting functions
    (push (cons "Format for selection exists" 
                (fboundp 'codelahoma-command-format-for-selection))
          results)
    
    (push (cons "Extract name exists" 
                (fboundp 'codelahoma-command-extract-name))
          results)
    
    ;; Test 4: Check sorting functions
    (push (cons "Sort by relevance exists" 
                (fboundp 'codelahoma-command-sort-by-relevance))
          results)
    
    (push (cons "Calculate score exists" 
                (fboundp 'codelahoma-command-calculate-score))
          results)
    
    (push (cons "Get usage count exists" 
                (fboundp 'codelahoma-command-get-usage-count))
          results)
    
    (push (cons "Get recency exists" 
                (fboundp 'codelahoma-command-get-recency))
          results)
    
    ;; Test 5: Check usage tracking
    (push (cons "Record usage exists" 
                (fboundp 'codelahoma-command-record-usage))
          results)
    
    (push (cons "Save history exists" 
                (fboundp 'codelahoma-command-save-history))
          results)
    
    (push (cons "Load history exists" 
                (fboundp 'codelahoma-command-load-history))
          results)
    
    ;; Test 6: Check category functions
    (push (cons "By category exists" 
                (fboundp 'codelahoma-command-by-category))
          results)
    
    (push (cons "Get by category exists" 
                (fboundp 'codelahoma-command-get-by-category))
          results)
    
    ;; Test 7: Check search functions
    (push (cons "Fuzzy search exists" 
                (fboundp 'codelahoma-command-fuzzy-search))
          results)
    
    (push (cons "Recent commands exists" 
                (fboundp 'codelahoma-command-recent))
          results)
    
    ;; Test 8: Test configuration
    (push (cons "Max recent configured" 
                (and (boundp 'codelahoma-command-palette-max-recent)
                     (integerp codelahoma-command-palette-max-recent)))
          results)
    
    (push (cons "Show keys configured" 
                (and (boundp 'codelahoma-command-palette-show-keys)
                     (booleanp codelahoma-command-palette-show-keys)))
          results)
    
    (push (cons "Categories configured" 
                (and (boundp 'codelahoma-command-palette-categories)
                     (listp codelahoma-command-palette-categories)
                     (> (length codelahoma-command-palette-categories) 0)))
          results)
    
    ;; Test 9: Test command registration
    (codelahoma-command-register-all)
    (push (cons "Commands registered" 
                (and (boundp 'codelahoma-command-registry)
                     (> (length codelahoma-command-registry) 0)))
          results)
    
    ;; Test 10: Test command formatting
    (let ((test-commands '(("Test Command" 
                           :function 'test-func
                           :category "Test"
                           :description "Test description"
                           :keybinding "C-t"))))
      (let ((formatted (codelahoma-command-format-for-selection test-commands)))
        (push (cons "Command formatting works" 
                    (and (listp formatted)
                         (stringp (car formatted))
                         (string-match "Test Command" (car formatted))))
              results)))
    
    ;; Test 11: Test name extraction
    (let ((formatted "[Test] Test Command - Description (C-t)"))
      (push (cons "Name extraction works" 
                  (string= (codelahoma-command-extract-name formatted)
                          "Test Command"))
            results))
    
    ;; Report results
    (with-output-to-temp-buffer "*Command Palette Test Results*"
      (princ "Command Palette Test Results\n")
      (princ "============================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-35s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*Command Palette Test Results*")))

;; Run the test
(test-command-palette)

(provide 'test-command-palette)
;;; test-command-palette.el ends here