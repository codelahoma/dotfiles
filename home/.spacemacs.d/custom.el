;; -*- mode: emacs-lisp -*-
;; This file is where Emacs writes custom variables.
;; Spacemacs will copy its content to your dotfile automatically in the
;; function `dotspacemacs/emacs-custom-settings'.
;; Do not alter this file, use Emacs customize interface instead.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values
   '((eval font-lock-add-keywords nil
           `
           ((,(concat "("
                      (regexp-opt
                       '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op"
                         "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                       t)
                      "\\_>")
             1 'font-lock-variable-name-face)))))
 '(org-agenda-files
   '("/Users/rodk/personal/org-files/work/gtd.org"
     "/Users/rodk/personal/org-files/work/projects.org"
     "/Users/rodk/personal/org-files/work/someday.org"
     "/Users/rodk/personal/org-files/personal/gtd.org"
     "/Users/rodk/personal/org-files/personal/projects.org"
     "/Users/rodk/personal/org-files/personal/someday.org"))
 '(package-selected-packages nil)
 '(safe-local-variable-values
   '((projectile-project-name . "org-files") (typescript-backend . tide)
     (typescript-backend . lsp) (javascript-backend . tide)
     (javascript-backend . tern) (javascript-backend . lsp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(fixed-pitch ((t (:family "FiraMono Nerd Font" :height 1.0))))
 '(org-block ((t (:inherit fixed-pitch :height 0.8))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-date ((t (:inherit (font-lock-comment-face fixed-pitch) :height 0.9))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit default :weight normal :inherit fixed-pitch :height 2.5 :underline nil))))
 '(org-done ((t (:font "Fira Sans" :height 1.0 :weight bold))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit default :weight normal :inherit fixed-pitch :height 1.8))))
 '(org-level-2 ((t (:inherit default :weight normal :inherit fixed-pitch :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight normal :inherit fixed-pitch :height 1.4))))
 '(org-level-4 ((t (:inherit default :weight normal :inherit fixed-pitch :height 1.3))))
 '(org-level-5 ((t (:inherit default :weight normal :inherit fixed-pitch :height 1.2))))
 '(org-level-6 ((t (:inherit default :weight normal :inherit fixed-pitch :height 1.2))))
 '(org-level-7 ((t (:inherit default :weight normal :inherit fixed-pitch :height 1.2))))
 '(org-level-8 ((t (:inherit default :weight normal :inherit fixed-pitch :height 1.2))))
 '(org-link ((t (:underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :height 0.5))))
 '(org-todo ((t (:font "Fira Sans" :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "Source Sans Pro" :height 1.1)))))
