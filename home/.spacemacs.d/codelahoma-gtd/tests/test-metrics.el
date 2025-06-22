;;; test-metrics.el --- Test knowledge metrics functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying knowledge metrics and insights functionality

;;; Code:

(require 'codelahoma-bridge-metrics)

(defun test-bridge-metrics ()
  "Test knowledge metrics functionality."
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Check dashboard functions
    (push (cons "Knowledge dashboard exists" 
                (fboundp 'codelahoma-bridge-knowledge-dashboard))
          results)
    
    (push (cons "Refresh dashboard exists" 
                (fboundp 'codelahoma-bridge-refresh-dashboard))
          results)
    
    ;; Test 2: Check metrics calculation functions
    (push (cons "Calculate overview metrics exists" 
                (fboundp 'codelahoma-bridge-calculate-overview-metrics))
          results)
    
    (push (cons "Calculate connection stats exists" 
                (fboundp 'codelahoma-bridge-calculate-connection-stats))
          results)
    
    (push (cons "Calculate growth data exists" 
                (fboundp 'codelahoma-bridge-calculate-growth-data))
          results)
    
    ;; Test 3: Check gap analysis functions
    (push (cons "Knowledge gaps exists" 
                (fboundp 'codelahoma-bridge-knowledge-gaps))
          results)
    
    (push (cons "Find orphaned tasks exists" 
                (fboundp 'codelahoma-bridge-find-orphaned-tasks))
          results)
    
    (push (cons "Find isolated notes exists" 
                (fboundp 'codelahoma-bridge-find-isolated-notes))
          results)
    
    (push (cons "Find stale notes exists" 
                (fboundp 'codelahoma-bridge-find-stale-notes))
          results)
    
    (push (cons "Find missing refs exists" 
                (fboundp 'codelahoma-bridge-find-missing-refs))
          results)
    
    ;; Test 4: Check usage pattern analysis
    (push (cons "Analyze usage patterns exists" 
                (fboundp 'codelahoma-bridge-analyze-usage-patterns))
          results)
    
    (push (cons "Insert usage patterns exists" 
                (fboundp 'codelahoma-bridge-insert-usage-patterns))
          results)
    
    ;; Test 5: Check export functions
    (push (cons "Export metrics exists" 
                (fboundp 'codelahoma-bridge-export-metrics))
          results)
    
    (push (cons "Export to org exists" 
                (fboundp 'codelahoma-bridge-export-to-org))
          results)
    
    ;; Test 6: Check visualization
    (push (cons "Visualize graph exists" 
                (fboundp 'codelahoma-bridge-visualize-graph))
          results)
    
    (push (cons "Generate dot graph exists" 
                (fboundp 'codelahoma-bridge-generate-dot-graph))
          results)
    
    ;; Test 7: Check helper functions
    (push (cons "Get all tasks exists" 
                (fboundp 'codelahoma-bridge-get-all-tasks))
          results)
    
    (push (cons "Note creation rate exists" 
                (fboundp 'codelahoma-bridge-note-creation-rate))
          results)
    
    (push (cons "Connection rate exists" 
                (fboundp 'codelahoma-bridge-connection-rate))
          results)
    
    ;; Test 8: Test configuration
    (push (cons "Cache duration configured" 
                (and (boundp 'codelahoma-bridge-metrics-cache-duration)
                     (integerp codelahoma-bridge-metrics-cache-duration)))
          results)
    
    (push (cons "Stale knowledge days configured" 
                (and (boundp 'codelahoma-bridge-stale-knowledge-days)
                     (integerp codelahoma-bridge-stale-knowledge-days)))
          results)
    
    (push (cons "Graph export configured" 
                (and (boundp 'codelahoma-bridge-enable-graph-export)
                     (booleanp codelahoma-bridge-enable-graph-export)))
          results)
    
    ;; Test 9: Test metrics calculation
    (let ((growth-data (codelahoma-bridge-calculate-growth-data)))
      (push (cons "Growth data structure valid" 
                  (and (plist-get growth-data :daily)
                       (plist-get growth-data :weekly)
                       (plist-get growth-data :monthly)))
            results))
    
    ;; Test 10: Test usage patterns
    (let ((patterns (codelahoma-bridge-analyze-usage-patterns)))
      (push (cons "Usage patterns structure valid" 
                  (and (plist-get patterns :popular-tags)
                       (plist-get patterns :active-projects)
                       (plist-get patterns :peak-times)
                       (plist-get patterns :workflow-stats)))
            results))
    
    ;; Report results
    (with-output-to-temp-buffer "*Bridge Metrics Test Results*"
      (princ "Knowledge Metrics Test Results\n")
      (princ "==============================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-35s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*Bridge Metrics Test Results*")))

;; Run the test
(test-bridge-metrics)

(provide 'test-metrics)
;;; test-metrics.el ends here