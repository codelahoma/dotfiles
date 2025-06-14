;; GTD Directory Setup Function
;; This can be added to codelahoma-org.org or run standalone

(defun rk/org-gtd-setup-directories ()
  "Create GTD directory structure under rk/org-directory."
  (interactive)
  (let ((base-dir (expand-file-name rk/org-directory)))
    ;; Create subdirectories
    (dolist (dir '("work" "personal"))
      (let ((dir-path (expand-file-name dir base-dir)))
        (unless (file-exists-p dir-path)
          (make-directory dir-path t)
          (message "Created directory: %s" dir-path))))
    
    ;; Create initial files if they don't exist
    (dolist (file '("inbox.org"
                    "archive.org"
                    "work/gtd.org"
                    "work/projects.org"
                    "work/someday.org"
                    "personal/gtd.org"
                    "personal/projects.org"
                    "personal/someday.org"))
      (let ((file-path (rk/org-file file)))
        (unless (file-exists-p file-path)
          (with-temp-buffer
            (insert (format "#+TITLE: %s\n#+STARTUP: overview\n\n"
                           (file-name-base file)))
            (write-file file-path))
          (message "Created file: %s" file-path))))
    
    (message "GTD directory structure setup complete!")))

;; Example capture templates using rk/org-file
(setq org-capture-templates
      `(("i" "Inbox" entry (file ,(rk/org-file "inbox.org"))
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
        
        ("w" "Work Templates")
        ("wt" "Work Task" entry (file ,(rk/org-file "work/gtd.org"))
         "* TODO %? :@work:\n  %U\n  %a\n  %i" :empty-lines 1)
        ("wp" "Work Project" entry (file ,(rk/org-file "work/projects.org"))
         "* TODO %? [/] :@work:project:\n  DEADLINE: %^{Deadline}t\n  %U\n  %a\n  %i")
        
        ("p" "Personal Templates")
        ("pt" "Personal Task" entry (file ,(rk/org-file "personal/gtd.org"))
         "* TODO %? :@personal:\n  %U\n  %a\n  %i" :empty-lines 1)
        ("pp" "Personal Project" entry (file ,(rk/org-file "personal/projects.org"))
         "* TODO %? [/] :@personal:project:\n  DEADLINE: %^{Deadline}t\n  %U\n  %a\n  %i")))

;; Context switching functions using rk/org-file
(defun org-work-mode ()
  "Switch to work-focused org environment."
  (interactive)
  (setq org-agenda-files (list (rk/org-file "work/") 
                               (rk/org-file "inbox.org")))
  (message "Switched to Work Mode"))

(defun org-personal-mode ()
  "Switch to personal-focused org environment."
  (interactive)
  (setq org-agenda-files (list (rk/org-file "personal/") 
                               (rk/org-file "inbox.org")))
  (message "Switched to Personal Mode"))

(defun org-unified-mode ()
  "Switch to unified org environment (everything)."
  (interactive)
  (setq org-agenda-files (list rk/org-directory))
  (message "Switched to Unified Mode"))