;;; test-task-states.el --- Test GTD task state management -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying GTD task state functionality

;;; Code:

(require 'codelahoma-gtd-core)

(defun test-gtd-task-states ()
  "Test GTD task state management."
  (interactive)
  (let ((results '())
        (test-file "/tmp/gtd-task-state-test.org"))
    
    ;; Create test file
    (with-temp-file test-file
      (insert "#+TITLE: Task State Test\n\n")
      (insert "* TODO Test task\n")
      (insert "** TODO Subtask\n"))
    
    ;; Test 1: Check todo keywords are set
    (push (cons "Todo keywords configured" 
                (equal org-todo-keywords codelahoma-gtd-todo-keywords))
          results)
    
    ;; Test 2: Check todo keyword faces
    (push (cons "Todo keyword faces configured" 
                (equal org-todo-keyword-faces codelahoma-gtd-todo-keyword-faces))
          results)
    
    ;; Test 3: Test state transitions
    (with-current-buffer (find-file-noselect test-file)
      (goto-char (point-min))
      (org-next-visible-heading 1)
      
      ;; Test set-next-action
      (codelahoma-gtd-set-next-action)
      (push (cons "Set NEXT action" 
                  (string= (org-get-todo-state) "NEXT"))
            results)
      
      ;; Test delegate
      (let ((inhibit-message t))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Test Person")))
          (codelahoma-gtd-delegate-task)))
      (push (cons "Delegate task" 
                  (and (string= (org-get-todo-state) "WAITING")
                       (string= (org-entry-get nil "DELEGATED_TO") "Test Person")))
            results)
      
      ;; Test convert to project
      (goto-char (point-min))
      (re-search-forward "\\*\\* TODO Subtask")
      (codelahoma-gtd-convert-to-project)
      (push (cons "Convert to project" 
                  (string= (org-get-todo-state) "PROJECT"))
            results)
      
      (kill-buffer))
    
    ;; Clean up
    (delete-file test-file)
    
    ;; Report results
    (with-output-to-temp-buffer "*GTD Task State Test Results*"
      (princ "GTD Task State Test Results\n")
      (princ "===========================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-30s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*GTD Task State Test Results*")))

;; Run the test
(test-gtd-task-states)

(provide 'test-task-states)
;;; test-task-states.el ends here