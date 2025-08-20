;;; verify-gtd-structure.el --- Comprehensive GTD directory structure verification -*- lexical-binding: t; -*-

;; Verification script for GTD-Zettelkasten directory structure

(defun verify-gtd-complete-structure ()
  "Comprehensively verify all GTD and Zettelkasten directories and files."
  (interactive)
  (let ((base-dir (expand-file-name "~/personal/org-files/"))
        (results '())
        (missing-dirs '())
        (missing-files '()))
    
    ;; Check base directory
    (push (format "=== GTD Directory Structure Verification ===\n") results)
    (push (format "Base directory: %s %s\n" 
                  base-dir 
                  (if (file-exists-p base-dir) "✓" "✗ MISSING")) results)
    
    ;; Check all directories
    (push (format "\n--- Required Directories ---\n") results)
    (let ((dirs '(("gtd" . "GTD root directory")
                  ("gtd/archive" . "Archive for completed items")
                  ("gtd/reviews" . "Review templates and records")
                  ("knowledge" . "Zettelkasten root")
                  ("knowledge/permanent" . "Permanent notes")
                  ("knowledge/literature" . "Literature notes")
                  ("knowledge/references" . "Reference materials")
                  ("knowledge/projects" . "Project-specific notes")
                  ("knowledge/daily" . "Daily notes")
                  ("areas" . "Life areas")
                  ("resources" . "Resources root")
                  ("resources/templates" . "Templates")
                  ("resources/checklists" . "Checklists"))))
      (dolist (dir-info dirs)
        (let* ((dir (car dir-info))
               (desc (cdr dir-info))
               (full-path (expand-file-name dir base-dir))
               (exists (file-exists-p full-path)))
          (push (format "%-30s: %s - %s\n" 
                        dir 
                        (if exists "✓" "✗")
                        desc) results)
          (unless exists
            (push full-path missing-dirs)))))
    
    ;; Check GTD files
    (push (format "\n--- Required GTD Files ---\n") results)
    (let ((files '(("gtd/inbox.org" . "Capture inbox")
                   ("gtd/projects.org" . "Active projects")
                   ("gtd/next-actions.org" . "Next actions list")
                   ("gtd/waiting-for.org" . "Waiting for items")
                   ("gtd/someday.org" . "Someday/maybe items")
                   ("gtd/calendar.org" . "Calendar entries")
                   ("gtd/media.org" . "Media to consume"))))
      (dolist (file-info files)
        (let* ((file (car file-info))
               (desc (cdr file-info))
               (full-path (expand-file-name file base-dir))
               (exists (file-exists-p full-path)))
          (push (format "%-30s: %s - %s\n"
                        file
                        (if exists "✓" "✗")
                        desc) results)
          (unless exists
            (push full-path missing-files)))))
    
    ;; Check optional files
    (push (format "\n--- Optional/Template Files ---\n") results)
    (let ((optional-files '(("resources/templates/weekly-review.org" . "Weekly review template")
                           ("resources/templates/project-template.org" . "Project template")
                           ("resources/checklists/travel.org" . "Travel checklist")
                           ("resources/checklists/meeting.org" . "Meeting checklist"))))
      (dolist (file-info optional-files)
        (let* ((file (car file-info))
               (desc (cdr file-info))
               (full-path (expand-file-name file base-dir))
               (exists (file-exists-p full-path)))
          (push (format "%-40s: %s - %s\n"
                        file
                        (if exists "✓" "(optional)")
                        desc) results))))
    
    ;; Summary
    (push (format "\n=== Summary ===\n") results)
    (push (format "Missing directories: %d\n" (length missing-dirs)) results)
    (push (format "Missing required files: %d\n" (length missing-files)) results)
    
    ;; Display results
    (with-output-to-temp-buffer "*GTD Structure Verification*"
      (dolist (result (reverse results))
        (princ result)))
    
    ;; Offer to create missing items
    (when (or missing-dirs missing-files)
      (when (y-or-n-p "Would you like to create missing directories and files? ")
        (create-missing-gtd-structure missing-dirs missing-files)))
    
    ;; Return status
    (list :missing-dirs missing-dirs :missing-files missing-files)))

(defun create-missing-gtd-structure (missing-dirs missing-files)
  "Create missing directories and files."
  (dolist (dir missing-dirs)
    (make-directory dir t)
    (message "Created directory: %s" dir))
  
  (dolist (file missing-files)
    (let ((title (capitalize (replace-regexp-in-string 
                             "-" " " 
                             (file-name-sans-extension 
                              (file-name-nondirectory file))))))
      (with-temp-buffer
        (insert (format "#+TITLE: %s\n" title))
        (insert "#+FILETAGS: :gtd:\n")
        (insert "#+STARTUP: overview\n")
        (insert "\n")
        (write-file file))
      (message "Created file: %s" file))))

(defun verify-gtd-permissions ()
  "Check permissions on GTD directories."
  (interactive)
  (let ((base-dir (expand-file-name "~/personal/org-files/"))
        (results '()))
    (push (format "=== Directory Permissions ===\n") results)
    (when (file-exists-p base-dir)
      (let ((attrs (file-attributes base-dir)))
        (push (format "Base directory: %s\n" base-dir) results)
        (push (format "  Readable: %s\n" (if (file-readable-p base-dir) "✓" "✗")) results)
        (push (format "  Writable: %s\n" (if (file-writable-p base-dir) "✓" "✗")) results)
        (push (format "  Executable: %s\n" (if (file-executable-p base-dir) "✓" "✗")) results)))
    
    (with-output-to-temp-buffer "*GTD Permissions*"
      (dolist (result (reverse results))
        (princ result)))))

;; Run verification
(verify-gtd-complete-structure)

(provide 'verify-gtd-structure)
;;; verify-gtd-structure.el ends here