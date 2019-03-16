(with-eval-after-load 'org
  ;; Org config goes here
  ;;

  (add-to-list 'org-modules 'org-protocol)
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-modules 'ox-jira)
  (setq org-jira-working-dir "~/Dropbox/org/")
  (setq org-agenda-files  (append (list "~/Dropbox/org/") (file-expand-wildcards "~/summit/*/.rodk")))

  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
    )

  (setq org-directory "~/Dropbox/org/")
  (setq org-capture-templates `(
                                ("t" "Todo" entry (file+headline ,(concat org-directory "inbox.org") "INBOX")
                                 "* TODO %?\n  %i\n  %a")
                                ("j" "Journal" entry (file+datetree ,(concat org-directory "journal.org"))
                                 "* %?\nEntered on %U\n  %i\n  %a" :unnarrowed t)
                                ("p" "Protocol" entry (file+headline ,(concat org-directory "inbox.org") "INBOX")
                                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                                ("L" "Protocol Link" entry (file+headline ,(concat org-directory "inbox.org") "INBOX")
                                 "* %? [[%:link][%:description]] \nCaptured On: %U")
                                ))

  (global-set-key "\C-cb" 'org-switchb)

  ;; (setq org-startup-indented t)
  (add-to-list 'org-file-apps '(directory . emacs))

  ;; Org-Jira
  (setq jiralib-url "https://summitesp.atlassian.net")
  (setq org-jira-use-status-as-todo t)


  ;; Refiling refinements
  ;; source: https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html

  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "BACKLOG(b)" "IN-PROGRESS(i!)" "WAITING(w@\!)" "CODE-COMPLETE(c!)" "CHANGES-REQUESTED(f!/!)" "|" "QA(q!)" "RELEASED(r!)" "CLOSED(x@)")
          (sequence "MEETING(m)" "|" "CANCELLED(l@)")))

  (setq org-catch-invisible-edits t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (http . t)
     (python . t)))

  ;; Fix mangling of org-structure-template-alist by ox-reveal
  (setq org-structure-template-alist
        (delete-duplicates(append (cdr org-structure-template-alist)
                                  (list '("n" . "notes")))))

  )
