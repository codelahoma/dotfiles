(with-eval-after-load 'org
  ;; Org config goes here
  ;;
  (require 'ox-gfm nil t)
  (setq org-jira-working-dir "~/Dropbox/org/")
  (setq org-agenda-files  (list "~/Dropbox/org/"))

  (global-set-key "\C-cb" 'org-switchb)
  (setq jiralib-url "https://summitesp.atlassian.net")

  ;; Refiling refinements
  ;; source: https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html

  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  )
