;;; test-autosave.el --- Test GTD auto-save functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script for verifying GTD auto-save and data integrity features

;;; Code:

(require 'codelahoma-gtd-autosave)

(defun test-gtd-autosave ()
  "Test GTD auto-save functionality."
  (interactive)
  (let ((results '())
        (test-file (expand-file-name "test-autosave.org" codelahoma-gtd-directory)))
    
    ;; Test 1: Check auto-save enable function
    (push (cons "Auto-save enable function exists" 
                (fboundp 'codelahoma-gtd-enable-auto-save))
          results)
    
    ;; Test 2: Check backup functions
    (push (cons "Create backup function exists" 
                (fboundp 'codelahoma-gtd-create-backup))
          results)
    
    (push (cons "List backups function exists" 
                (fboundp 'codelahoma-gtd-list-backups))
          results)
    
    ;; Test 3: Check data integrity functions
    (push (cons "Validate file function exists" 
                (fboundp 'codelahoma-gtd-validate-file))
          results)
    
    (push (cons "Check all files function exists" 
                (fboundp 'codelahoma-gtd-check-all-files))
          results)
    
    ;; Test 4: Check recovery functions
    (push (cons "Restore backup function exists" 
                (fboundp 'codelahoma-gtd-restore-backup))
          results)
    
    (push (cons "Emergency backup function exists" 
                (fboundp 'codelahoma-gtd-emergency-backup))
          results)
    
    ;; Test 5: Check backup directory creation
    (unless (file-exists-p codelahoma-gtd-backup-directory)
      (make-directory codelahoma-gtd-backup-directory t))
    (push (cons "Backup directory exists" 
                (file-exists-p codelahoma-gtd-backup-directory))
          results)
    
    ;; Test 6: Test backup filename generation
    (let ((backup-name (codelahoma-gtd-backup-filename test-file)))
      (push (cons "Backup filename generation" 
                  (and (stringp backup-name)
                       (string-match-p "test-autosave\\.org\\." backup-name)))
            results))
    
    ;; Test 7: Test file validation
    (with-temp-file test-file
      (insert "#+TITLE: Test File\n\n* TODO Test item\n"))
    (let ((issues (codelahoma-gtd-validate-file test-file)))
      (push (cons "File validation works" 
                  (null issues))
            results))
    
    ;; Clean up test file
    (when (file-exists-p test-file)
      (delete-file test-file))
    
    ;; Report results
    (with-output-to-temp-buffer "*GTD Auto-save Test Results*"
      (princ "GTD Auto-save Test Results\n")
      (princ "==========================\n\n")
      (dolist (result (nreverse results))
        (princ (format "%-30s: %s\n" 
                      (car result) 
                      (if (cdr result) "✓ PASS" "✗ FAIL"))))
      (princ "\n")
      (let ((passed (cl-count-if #'cdr results)))
        (princ (format "Total: %d/%d tests passed\n" 
                      passed (length results)))))
    
    (switch-to-buffer-other-window "*GTD Auto-save Test Results*")))

;; Run the test
(test-gtd-autosave)

(provide 'test-autosave)
;;; test-autosave.el ends here