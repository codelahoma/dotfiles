;;; test-search.el --- Test unified search functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying unified search functionality

;;; Code:

(require 'codelahoma-unified-search)

(defun test-search ()
  "Test unified search functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check main search functions
    (push (cons "Search function exists" 
                (fboundp 'codelahoma-search))
          results)
    
    (push (cons "Search execute exists" 
                (fboundp 'codelahoma-search-execute))
          results)
    
    (push (cons "Search display results exists" 
                (fboundp 'codelahoma-search-display-results))
          results)
    
    ;; Test 2: Check search type functions
    (push (cons "Search tasks exists" 
                (fboundp 'codelahoma-search-tasks))
          results)
    
    (push (cons "Search notes exists" 
                (fboundp 'codelahoma-search-notes))
          results)
    
    (push (cons "Search projects exists" 
                (fboundp 'codelahoma-search-projects))
          results)
    
    (push (cons "Search inbox exists" 
                (fboundp 'codelahoma-search-inbox))
          results)
    
    (push (cons "Search archives exists" 
                (fboundp 'codelahoma-search-archives))
          results)
    
    (push (cons "Search linked exists" 
                (fboundp 'codelahoma-search-linked))
          results)
    
    ;; Test 3: Check result functions
    (push (cons "Create result exists" 
                (fboundp 'codelahoma-search-create-result))
          results)
    
    (push (cons "Get context exists" 
                (fboundp 'codelahoma-search-get-context))
          results)
    
    (push (cons "Insert result exists" 
                (fboundp 'codelahoma-search-insert-result))
          results)
    
    ;; Test 4: Check advanced search
    (push (cons "Advanced search exists" 
                (fboundp 'codelahoma-search-advanced))
          results)
    
    (push (cons "Build criteria exists" 
                (fboundp 'codelahoma-search-build-criteria))
          results)
    
    (push (cons "Execute advanced exists" 
                (fboundp 'codelahoma-search-execute-advanced))
          results)
    
    (push (cons "Match criteria exists" 
                (fboundp 'codelahoma-search-match-criteria-p))
          results)
    
    ;; Test 5: Check saved searches
    (push (cons "Save search exists" 
                (fboundp 'codelahoma-search-save))
          results)
    
    (push (cons "Load search exists" 
                (fboundp 'codelahoma-search-load))
          results)
    
    (push (cons "Persist saved exists" 
                (fboundp 'codelahoma-search-persist-saved))
          results)
    
    (push (cons "Load saved exists" 
                (fboundp 'codelahoma-search-load-saved))
          results)
    
    ;; Test 6: Check highlighting
    (push (cons "Highlight match exists" 
                (fboundp 'codelahoma-search-highlight-match))
          results)
    
    (push (cons "Type face exists" 
                (fboundp 'codelahoma-search-type-face))
          results)
    
    ;; Test 7: Test configuration
    (push (cons "Default scope configured" 
                (and (boundp 'codelahoma-search-default-scope)
                     (stringp codelahoma-search-default-scope)))
          results)
    
    (push (cons "Max results configured" 
                (and (boundp 'codelahoma-search-max-results)
                     (integerp codelahoma-search-max-results)))
          results)
    
    (push (cons "Context lines configured" 
                (and (boundp 'codelahoma-search-context-lines)
                     (integerp codelahoma-search-context-lines)))
          results)
    
    (push (cons "Save history configured" 
                (and (boundp 'codelahoma-search-save-history)
                     (booleanp codelahoma-search-save-history)))
          results)
    
    ;; Test 8: Test buffer name
    (push (cons "Results buffer name defined" 
                (and (boundp 'codelahoma-search-results-buffer)
                     (stringp codelahoma-search-results-buffer)))
          results)
    
    ;; Test 9: Test search result creation
    (let ((result (codelahoma-search-create-result 
                  :type "Test"
                  :title "Test Result"
                  :file "/test/file.org"
                  :pos 100)))
      (push (cons "Result creation works" 
                  (and (plist-get result :type)
                       (plist-get result :title)
                       (plist-get result :file)
                       (plist-get result :pos)))
            results))
    
    ;; Test 10: Test highlighting
    (let ((highlighted (codelahoma-search-highlight-match 
                       "This is a test string" "test")))
      (push (cons "Highlighting works" 
                  (string-match-p 'highlight highlighted))
            results))
    
    ;; Test 11: Test mode map
    (push (cons "Results mode map exists" 
                (and (boundp 'codelahoma-search-results-mode-map)
                     (keymapp codelahoma-search-results-mode-map)))
          results)
    
    ;; Report results
    (with-output-to-temp-buffer "*Search Test Results*"
      (princ "Unified Search Test Results\n")
      (princ "===========================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-35s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*Search Test Results*")))

;; Run the test
(test-search)

(provide 'test-search)
;;; test-search.el ends here