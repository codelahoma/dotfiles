(with-eval-after-load 'org
  ;; Org config goes here
  ;;

  (add-to-list 'org-modules 'org-protocol)
  (setq org-jira-working-dir "~/Dropbox/org/")
  (setq org-agenda-files  (list "~/Dropbox/org/" "~/summit/summit-knowledge/.rodk/"))

  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
    )

  (setq org-directory "~/Dropbox/org/")
  (setq org-capture-templates `(
                                ("t" "Todo" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
                                 "* TODO %?\n  %i\n  %a")
                                ("j" "Journal" entry (file+datetree ,(concat org-directory "journal.org"))
                                 "* %?\nEntered on %U\n  %i\n  %a")
                                ("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
                                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                                ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
                                 "* %? [[%:link][%:description]] \nCaptured On: %U")
                                ))

  (global-set-key "\C-cb" 'org-switchb)
  (setq jiralib-url "https://summitesp.atlassian.net")

  ;; Refiling refinements
  ;; source: https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html

  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  )
