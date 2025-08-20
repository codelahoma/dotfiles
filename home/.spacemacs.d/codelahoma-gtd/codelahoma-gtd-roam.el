;;; codelahoma-gtd-roam.el --- Org-roam configuration for GTD-Zettelkasten -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Rod Knowlton
;; Author: Rod Knowlton
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org-roam "2.0"))

;;; Commentary:
;; Org-roam configuration and integration for the GTD-Zettelkasten system.
;; Handles knowledge base setup, capture templates, and directory structure.

;;; Code:

(require 'codelahoma-gtd-config)

(defun codelahoma-gtd-roam-setup ()
  "Configure org-roam for the GTD-Zettelkasten system."
  (use-package org-roam
    :ensure t
    :custom
    (org-roam-directory codelahoma-knowledge-directory)
    (org-roam-db-location (expand-file-name ".org-roam.db" "~/personal/org-files/"))
    (org-roam-completion-everywhere t)
    :config
    ;; Node display template
    (setq org-roam-node-display-template 
          (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    
    ;; Enable automatic database syncing
    (org-roam-db-autosync-mode)
    
    ;; Personal capture templates
    (setq org-roam-capture-templates
          '(("d" "default" plain "%?"
             :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+created: %U\n#+filetags: \n\n")
             :unnarrowed t)
            ("p" "permanent" plain "%?"
             :target (file+head "permanent/%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+created: %U\n#+type: permanent\n#+filetags: \n\n")
             :unnarrowed t)
            ("l" "literature" plain "%?"
             :target (file+head "literature/%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+author: %^{Author}\n#+created: %U\n#+type: literature\n#+filetags: \n\n")
             :unnarrowed t)
            ("r" "reference" plain "%?"
             :target (file+head "references/%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+source: %^{Source URL}\n#+created: %U\n#+type: reference\n#+filetags: \n\n")
             :unnarrowed t)
            ("j" "project note" plain "%?"
             :target (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+project: %^{Project}\n#+created: %U\n#+type: project-note\n#+filetags: \n\n")
             :unnarrowed t)))
    
    ;; Daily notes configuration
    (setq org-roam-dailies-directory "daily/")
    (setq org-roam-dailies-capture-templates
          '(("d" "default" entry
             "* %?"
             :target (file+head "%<%Y-%m-%d>.org"
                                "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n* Morning Pages\n\n* Today's Focus\n\n* Captured Thoughts\n\n"))))))

(defun codelahoma-gtd-roam-ensure-directories ()
  "Ensure all org-roam subdirectories exist."
  (let ((subdirs '("permanent" "literature" "references" "projects" "daily")))
    (dolist (dir subdirs)
      (let ((full-path (expand-file-name dir codelahoma-knowledge-directory)))
        (unless (file-exists-p full-path)
          (make-directory full-path t)
          (message "Created org-roam directory: %s" full-path))))))

(defun codelahoma-gtd-roam-initialize ()
  "Initialize org-roam database and ensure directories exist."
  (interactive)
  (codelahoma-gtd-roam-ensure-directories)
  (when (featurep 'org-roam)
    (org-roam-db-sync)
    (message "Org-roam database synchronized")))

;; Quick capture functions
(defun codelahoma-gtd-roam-capture-permanent ()
  "Capture a permanent note."
  (interactive)
  (org-roam-capture- :keys "p"))

(defun codelahoma-gtd-roam-capture-literature ()
  "Capture a literature note."
  (interactive)
  (org-roam-capture- :keys "l"))

(defun codelahoma-gtd-roam-capture-reference ()
  "Capture a reference note."
  (interactive)
  (org-roam-capture- :keys "r"))

(defun codelahoma-gtd-roam-capture-project ()
  "Capture a project-related note."
  (interactive)
  (org-roam-capture- :keys "j"))

;; Search and navigation helpers
(defun codelahoma-gtd-roam-find-permanent ()
  "Find permanent notes."
  (interactive)
  (org-roam-node-find nil nil (lambda (node)
                                 (member "permanent" (org-roam-node-tags node)))))

(defun codelahoma-gtd-roam-find-literature ()
  "Find literature notes."
  (interactive)
  (org-roam-node-find nil nil (lambda (node)
                                 (member "literature" (org-roam-node-tags node)))))

(defun codelahoma-gtd-roam-find-by-project (project)
  "Find notes related to a specific PROJECT."
  (interactive "sProject name: ")
  (org-roam-node-find nil nil (lambda (node)
                                 (string-match-p project (or (org-roam-node-title node) "")))))

(provide 'codelahoma-gtd-roam)
;;; codelahoma-gtd-roam.el ends here