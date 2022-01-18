(with-eval-after-load 'org
  ;; Org config goes here
  ;;

  (setq org-directory "~/Dropbox/org/")
  (setq org-brain-path (concat org-directory "brain/"))
  (setq gtd-directory (concat org-directory "gtd/"))
  (setq org-id-track-globally t)
  (defalias `rk/org-file (apply-partially 'concat org-directory))
  (defalias `rk/gtd-file (apply-partially 'concat gtd-directory))


  (add-to-list 'org-modules 'org-protocol)
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-modules 'ox-jira)
  (add-to-list 'org-modules 'org-checklist)

  (setq org-tags-exclude-from-inheritance (list "project"))
  (setq org-list-allow-alphabetical t)
  (setq org-enable-org-brain-support t)

  (setq org-jira-working-dir org-directory)
  (setq org-agenda-files  (append (list org-jira-working-dir) (list gtd-directory)))

  (defun my-org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (org-current-is-todo)
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (org-current-is-todo)
            (setq should-skip-entry t))))
      (when should-skip-entry
        (or (outline-next-heading)
            (goto-char (point-max))))))
	
  (defun org-current-is-todo ()
    (string= "TODO" (org-get-todo-state)))

  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform))
    )

  (setq org-capture-templates `(
                                ("t" "Todos")
                                ("tl" "Todo with Link" entry (file ,(rk/gtd-file "inbox.org")) "* TODO %?\n  %i\n  %a")
                                ("tt" "Todo" entry (file ,(rk/gtd-file "inbox.org")) "* TODO %?\n  %i\n")
                                ("tT" "Tickler" entry (file+headline ,(rk/gtd-file "tickler.org") "Tickler") "* %i%? \n %U"))
        )

  (global-set-key "\C-cb" 'org-switchb)

  (setq rk/work-org-files (-flatten (list

                                     (rk/gtd-file "inbox.org")
                                     (rk/gtd-file "gtd.org")
                                     (rk/gtd-file "tickler.org")
                                     (rk/gtd-file "someday.org")
                                     (rk/gtd-file "reference.org")
                                     )))

  (setq rk/home-org-files (list
                           (rk/gtd-file "inbox.org")
                           (rk/gtd-file "gtd.org")
                           (rk/gtd-file "tickler.org")
                           (rk/gtd-file "someday.org")
                           ))

  (setq org-agenda-custom-commands
        '(("h" "Home"
           ((agenda "" ((org-agenda-span 3)))
            (tags-todo "@phone" ((org-agenda-overriding-header "Calls")))
            (tags "-@kitewire+TODO=\"WAITING\"" ((org-agenda-overriding-header "Waiting")))
            (tags-todo "-@kitewire" (
                                   (org-agenda-overriding-header "Todo")
                                   (org-agenda-files rk/home-org-files)
                                   (org-agenda-skip-function 'my-org-agenda-skip-all-siblings-but-first)))
            ()))
          ("k" . "Kitewire Views")
          ("kk" "Kitewire"
           (
            (agenda)
            (tags-todo "+kitewire-reading-home-@home-30days-60days-90days/-MEETING" ((org-agenda-overriding-header "Kitewire") (org-agenda-files rk/work-org-files) ))
            (tags-todo "@phone" ((org-agenda-overriding-header "Calls")))
            (tags "-@home-home+TODO=\"WAITING\"" ((org-agenda-overriding-header "Waiting")))
            (tags "30days" ((org-agenda-overriding-header "30 Day Plan")))
            (tags "60days" ((org-agenda-overriding-header "60 Day Plan")))
            (tags "90days" ((org-agenda-overriding-header "90 Day Plan")))
            (tags "project" ((org-agenda-overriding-header "Projects")))
            ;; (tags "-@home-home+TODO=\"IN-PROGRESS\"" ((org-agenda-overriding-header "Todo") (org-agenda-files rk/work-org-files)))
            ()))
          ("kW" "Weekly review"
           agenda ""
           ((org-agenda-span 'week)
            (org-agenda-start-on-weekday 0)
            (org-agenda-start-with-log-mode '(closed clock))
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if 'nottodo 'done))
            )
           )))

  (add-to-list 'org-agenda-custom-commands
               '("W" "Weekly review"
                 agenda ""
                 ((org-agenda-span 'week)
                  (org-agenda-start-on-weekday 0)
                  (org-agenda-start-with-log-mode '(closed clock))
                  (org-agenda-skip-function
                   '(org-agenda-skip-entry-if 'nottodo 'done))
                  )
                 ))
  ;; (setq org-startup-indented t)
  (add-to-list 'org-file-apps '(directory . emacs))

  ;; Org-Jira
  (setq jiralib-url "https://summitesp.atlassian.net")
  (setq org-jira-use-status-as-todo t)

  ;; Org-Journal
  (setq org-journal-dir "~/Dropbox/org/journal/")
  (setq org-journal-file-type 'weekly)


  ;; Org-reveal
  (setq org-re-reveal-title-slide "<h1 class='title'>%t</h1><h2 class='author'>%a</h2><p class='email'>%e</p>")
  (setq org-re-reveal-root "file:///Users/rodk/.emacs.d/private/reveal.js-4.1.0")

  ;; Refiling refinements
  ;; source: https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html

  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c@)")
          ;; keyword in org-jira files.
          (sequence "BACKLOG"
                    "TO-DO"
                    "IN-PROGRESS"
                    "WAITING"
                    "PAUSED"
                    "CHANGES-REQUESTED"
                    "CODE-COMPLETE"
                    "ASG-TESTING"
                    "READY-FOR_TEST"
                    "TESTING"
                    "QA"
                    "|"
                    "RELEASED"
                    "CLOSED"
                    "COMPLETE"
                    "MERGED")

          (sequence "MEETING(m)" "|" "ATTENDED(a@)" "IGNORED(t)" "CANCELLED(l@)")))

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
