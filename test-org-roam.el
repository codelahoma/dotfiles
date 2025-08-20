;;; test-org-roam.el --- Test org-roam configuration -*- lexical-binding: t; -*-

;; Test script for verifying org-roam installation and configuration

(defun test-org-roam-setup ()
  "Test org-roam setup and configuration."
  (interactive)
  (let ((results '()))
    ;; Test 1: Check if org-roam is loaded
    (push (format "1. Org-roam loaded: %s" 
                  (if (featurep 'org-roam) "✓ YES" "✗ NO"))
          results)
    
    ;; Test 2: Check directory configuration
    (push (format "2. Knowledge directory: %s (exists: %s)"
                  codelahoma-knowledge-directory
                  (if (file-exists-p codelahoma-knowledge-directory) "✓" "✗"))
          results)
    
    ;; Test 3: Check database location
    (when (boundp 'org-roam-db-location)
      (push (format "3. Database location: %s (exists: %s)"
                    org-roam-db-location
                    (if (file-exists-p org-roam-db-location) "✓" "✗"))
            results))
    
    ;; Test 4: Check subdirectories
    (let ((subdirs '("permanent" "literature" "references" "projects" "daily")))
      (push (format "4. Subdirectories:")
            results)
      (dolist (dir subdirs)
        (let ((full-path (expand-file-name dir codelahoma-knowledge-directory)))
          (push (format "   - %s: %s" 
                        dir 
                        (if (file-exists-p full-path) "✓" "✗"))
                results))))
    
    ;; Test 5: Test keybindings
    (push (format "5. Keybindings defined: %s"
                  (if (lookup-key spacemacs-default-map (kbd "SPC o o z n"))
                      "✓ YES"
                    "✗ NO"))
          results)
    
    ;; Display results
    (with-output-to-temp-buffer "*Org-Roam Test Results*"
      (dolist (result (reverse results))
        (princ (concat result "\n"))))
    
    ;; Return overall status
    (message "Org-roam tests completed. Check *Org-Roam Test Results* buffer.")))

;; Quick test functions
(defun test-org-roam-create-note ()
  "Test creating a new org-roam note."
  (interactive)
  (org-roam-node-find))

(defun test-org-roam-db-sync ()
  "Test database synchronization."
  (interactive)
  (org-roam-db-sync)
  (message "Database sync completed. Check *Messages* for details."))

(provide 'test-org-roam)
;;; test-org-roam.el ends here