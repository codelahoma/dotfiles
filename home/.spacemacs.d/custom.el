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
   '("/Users/rodk/personal/org-files/gtd/inbox.org"
     "/Users/rodk/personal/org-files/gtd/projects.org"
     "/Users/rodk/personal/org-files/gtd/next-actions.org"
     "/Users/rodk/personal/org-files/gtd/waiting-for.org"
     "/Users/rodk/personal/org-files/gtd/someday.org"
     "/Users/rodk/personal/org-files/gtd/calendar.org"
     "/Users/rodk/personal/org-files/gtd/media.org"))
 '(org-fold-core-style 'overlays)
 '(package-selected-packages
   '(ace-jump-helm-line ace-link add-node-modules-path afternoon-theme
                        aggressive-indent alect-themes all-the-icons ample-theme
                        ample-zen-theme ansible ansible-doc anti-zenburn-theme
                        apropospriate-theme atomic-chrome auto-compile
                        auto-highlight-symbol auto-yasnippet badwolf-theme
                        better-jumper birds-of-paradise-plus-theme blacken bm
                        browse-at-remote bubbleberry-theme busybee-theme
                        centered-cursor-mode cherry-blossom-theme
                        chocolate-theme clean-aindent-mode clues-theme
                        cmake-mode code-cells code-review color-identifiers-mode
                        color-theme-sanityinc-solarized
                        color-theme-sanityinc-tomorrow column-enforce-mode
                        command-log-mode company-ansible company-box
                        company-emoji company-posframe company-quickhelp
                        company-restclient company-statistics company-web
                        copy-as-format csv-mode cyberpunk-theme cython-mode
                        dakrone-theme darkmine-theme darkokai-theme
                        darktooth-theme devdocs diff-hl diminish
                        dired-quick-sort direnv disable-mouse django-theme
                        docker dockerfile-mode doom-themes dotenv-mode
                        dracula-theme drag-stuff dumb-jump eat edit-indirect
                        edit-server ef-themes elfeed-goodies elfeed-org
                        elisp-def elisp-demos elisp-slime-nav emmet-mode
                        emoji-cheat-sheet-plus emr engine-mode esh-help
                        eshell-prompt-extras eshell-z espresso-theme
                        eval-sexp-fu evil-anzu evil-args evil-cleverparens
                        evil-collection evil-easymotion evil-escape
                        evil-evilified-state evil-exchange evil-goggles
                        evil-iedit-state evil-indent-plus evil-lion
                        evil-lisp-state evil-matchit evil-nerd-commenter
                        evil-numbers evil-org evil-surround evil-textobj-line
                        evil-tutor evil-unimpaired evil-visual-mark-mode
                        evil-visualstar exotica-theme expand-region eyebrowse
                        eziam-themes fancy-battery farmhouse-themes
                        fira-code-mode flatland-theme flatui-theme flycheck-elsa
                        flycheck-package flycheck-pos-tip flymd
                        flyspell-correct-helm flyspell-popup fold-this
                        gandalf-theme gh-md git-link git-messenger git-modes
                        git-timemachine gitignore-templates gmail-message-mode
                        gnuplot golden-ratio google-translate gotham-theme gptel
                        grandshell-theme gruber-darker-theme gruvbox-theme
                        hc-zenburn-theme helm-ag helm-c-yasnippet helm-comint
                        helm-company helm-css-scss helm-ctest helm-descbinds
                        helm-git-grep helm-ls-git helm-lsp helm-make
                        helm-mode-manager helm-org helm-org-rifle helm-pass
                        helm-projectile helm-purpose helm-pydoc helm-swoop
                        helm-themes helm-xref helpful hemisu-theme heroku-theme
                        hide-comnt highlight-indent-guides highlight-indentation
                        highlight-numbers highlight-parentheses hl-todo
                        holy-mode hungry-delete hybrid-mode hyperbole
                        ibuffer-projectile impatient-mode indent-guide info+
                        inkpot-theme inspector ir-black-theme jazz-theme
                        jbeans-theme jinja2-mode jira-markup-mode js-doc
                        js2-refactor json-mode json-navigator json-reformat
                        kaolin-themes keycast keychain-environment launchctl
                        light-soap-theme live-py-mode livid-mode lorem-ipsum
                        lsp-origami lsp-pyright lsp-treemacs lsp-ui lua-mode
                        lush-theme macrostep madhat2r-theme markdown-toc
                        material-theme mermaid-mode minimal-theme modus-themes
                        moe-theme molokai-theme monochrome-theme monokai-theme
                        multi-line multi-term multi-vterm mustang-theme nameless
                        naquadah-theme noctilux-theme nodejs-repl npm-mode
                        ob-http ob-mermaid ob-restclient obsidian-theme
                        occidental-theme oldlace-theme omtose-phellack-themes
                        open-junk-file org org-cliplink org-contrib org-download
                        org-mime org-noter-pdftools org-pomodoro org-present
                        org-projectile org-rich-yank org-roam-bibtex org-roam-ui
                        org-sticky-header org-superstar org-transclusion
                        organic-green-theme orgit-forge osx-clipboard
                        osx-dictionary osx-trash overseer page-break-lines
                        paradox password-generator password-store-otp pcre2el
                        pdf-view-restore persistent-scratch pet
                        phoenix-dark-mono-theme phoenix-dark-pink-theme pinboard
                        pip-requirements pipenv pippel planet-theme poetry
                        prettier-js professional-theme pug-mode
                        purple-haze-theme py-isort pydoc pyenv-mode pylookup
                        pytest quickrun railscasts-theme rainbow-delimiters
                        rainbow-identifiers rainbow-mode rebecca-theme
                        restart-emacs restclient-helm reveal-in-osx-finder
                        reverse-theme rjsx-mode ron-mode rustic sass-mode
                        scss-mode seti-theme shell-pop sicp slim-mode smeargle
                        smyx-theme soft-charcoal-theme soft-morning-theme
                        soft-stone-theme solarized-theme soothe-theme space-doc
                        spacegray-theme spaceline spacemacs-purpose-popwin
                        spacemacs-whitespace-cleanup sphinx-doc sqlite3
                        string-edit-at-point string-inflection subatomic-theme
                        subatomic256-theme sublime-themes sunny-day-theme
                        symbol-overlay symon tagedit tango-2-theme
                        tango-plus-theme tangotango-theme tao-theme term-cursor
                        terminal-here texfrag tide toc-org toml-mode toxi-theme
                        tree-sitter-langs treemacs-evil treemacs-icons-dired
                        treemacs-magit treemacs-persp treemacs-projectile
                        ts-fold twilight-anti-bright-theme twilight-bright-theme
                        twilight-theme typescript-mode ujelly-theme
                        underwater-theme undo-fu undo-fu-session
                        unkillable-scratch vi-tilde-fringe volatile-highlights
                        vundo wakatime-mode web-beautify web-mode wgrep
                        white-sand-theme winum writeroom-mode ws-butler wsd-mode
                        yaml-mode yasnippet-snippets zen-and-art-theme
                        zenburn-theme zonokai-emacs))
 '(paradox-github-token t)
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
