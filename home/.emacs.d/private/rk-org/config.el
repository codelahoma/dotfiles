(with-eval-after-load 'org
  ;; Org config goes here
  ;;

  (setq org-directory "~/Dropbox/org/")
  (setq gtd-directory (concat org-directory "gtd/"))

  (add-to-list 'org-modules 'org-protocol)
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-modules 'ox-jira)
  (setq org-jira-working-dir org-directory)
  (setq org-agenda-files  (append (list org-jira-working-dir) (list gtd-directory)))

  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform))
    )

  (setq org-capture-templates `(
                                ("t" "Todos")
                                ("tl" "Todo with Link" entry (file+headline ,(concat gtd-directory "inbox.org") "INBOX") "* TODO %?\n  %i\n  %a")
                                ("tt" "Todo" entry (file+headline ,(concat gtd-directory "inbox.org") "INBOX") "* TODO %?\n  %i\n")
                                ("ts" "Summit Todo" entry (file+olp  ,(concat gtd-directory "gtd.org")"Summit" "INBOX")
                                 ("tT" "Tickler" entry
                                  (file+headline "~/gtd/tickler.org" "Tickler")
                                  "* %i%? \n %U")
                                ("j" "Journal" entry (file+datetree  "journal.org")
                                 "* %?\nEntered on %U\n  %i\n  %a" :unnarrowed t)
                                )))

  (global-set-key "\C-cb" 'org-switchb)

  (setq org-agenda-custom-commands
        '(("w" "Work"
           ((agenda "" ((org-agenda-span 1)))
            (tags-todo "@summit" ((org-agenda-overriding-header "Summit")))
            (tags-todo "@phone" ((org-agenda-overriding-header "Calls")))
            (todo "WAITING" ((org-agenda-overriding-header "Waiting")))
            (todo "TODO" ((org-agenda-overriding-header "Todo")))
            ()))))

  ;; (setq org-startup-indented t)
  (add-to-list 'org-file-apps '(directory . emacs))

  ;; Org-Jira
  (setq jiralib-url "https://summitesp.atlassian.net")
  (setq org-jira-use-status-as-todo t)

  ;; Org-Journal
  (setq org-journal-dir "~/Dropbox/org/journal/")



  ;; Refiling refinements
  ;; source: https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html

  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d@)" "CANCELLED(c@)")
          ;; keyword in org-jira files.
          (sequence "BACKLOG"
                    "IN-PROGRESS"
                    "WAITING"
                    "CODE-COMPLETE"
                    "CHANGES-REQUESTED"
                    "ASG-TESTING"
                    "READY-FOR_TEST"
                    "TESTING"
                    "|"
                    "QA"
                    "RELEASED"
                    "CLOSED"
                    "COMPLETE"
                    "MERGED")

          (sequence "MEETING(m)" "|" "CANCELLED(l@)")))

  (setq org-catch-invisible-edits t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (http . t)
     (lua . t)
     (python . t)
     (R . t)))
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  ;; Fix mangling of org-structure-template-alist by ox-reveal
  (setq org-structure-template-alist
        (delete-duplicates(append (cdr org-structure-template-alist)
                                  (list '("n" . "notes")))))

  )
