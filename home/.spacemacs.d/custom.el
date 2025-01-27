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
 '(package-selected-packages
   '(zonokai-emacs zenburn-theme zen-and-art-theme yasnippet-snippets yaml-mode wsd-mode ws-butler writeroom-mode winum white-sand-theme which-key wgrep web-mode web-beautify wakatime-mode vundo volatile-highlights vim-powerline vi-tilde-fringe unkillable-scratch undo-fu-session undo-fu underwater-theme ujelly-theme typescript-mode twilight-theme twilight-bright-theme twilight-anti-bright-theme ts-fold treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil tree-sitter-langs toxi-theme toml-mode toc-org tide texfrag terminal-here term-cursor tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit symon symbol-overlay sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection string-edit-at-point sqlite3 sql-indent sphinx-doc spacemacs-whitespace-cleanup spacemacs-purpose-popwin spaceline spacegray-theme space-doc soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode sicp shell-pop seti-theme scss-mode sass-mode rustic ron-mode rjsx-mode reverse-theme reveal-in-osx-finder restclient-helm restart-emacs rebecca-theme rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme quickrun pytest pylookup pyenv-mode pydoc py-isort purple-haze-theme pug-mode professional-theme prettier-js poetry plantuml-mode planet-theme pippel pipenv pip-requirements pinboard phoenix-dark-pink-theme phoenix-dark-mono-theme persistent-scratch pdf-view-restore pcre2el password-store-otp password-generator paradox ox-twbs ox-slack overseer osx-trash osx-dictionary osx-clipboard orgit-forge organic-green-theme org-wild-notifier org-trello org-transclusion org-superstar org-sticky-header org-roam-ui org-roam-bibtex org-rich-yank org-re-reveal org-projectile org-present org-pomodoro org-noter-pdftools org-modern org-mime org-journal org-download org-contrib org-cliplink org-appear open-junk-file omtose-phellack-themes oldlace-theme occidental-theme obsidian-theme ob-restclient ob-mermaid ob-hy ob-http ob-async npm-mode nodejs-repl noctilux-theme naquadah-theme nameless mustang-theme multi-vterm multi-term multi-line mu4e-alert monokai-theme monochrome-theme molokai-theme moe-theme modus-themes minimal-theme mermaid-mode material-theme markdown-toc majapahit-themes madhat2r-theme macrostep lush-theme lua-mode lsp-ui lsp-treemacs lsp-pyright lsp-origami lorem-ipsum livid-mode live-py-mode light-soap-theme launchctl keychain-environment keycast kaolin-themes json-reformat json-navigator json-mode js2-refactor js-doc jira-markup-mode jinja2-mode jbeans-theme jazz-theme ir-black-theme inspector inkpot-theme info+ indent-guide impatient-mode ibuffer-projectile hyperbole hybrid-mode hy-mode hungry-delete holy-mode hl-todo highlight-parentheses highlight-numbers highlight-indentation highlight-indent-guides hide-comnt heroku-theme hemisu-theme helpful helm-xref helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-pass helm-org-rifle helm-org helm-mu helm-mode-manager helm-make helm-lsp helm-ls-git helm-git-grep helm-descbinds helm-ctest helm-css-scss helm-company helm-comint helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme graphviz-dot-mode grandshell-theme gptel gotham-theme google-translate golden-ratio gnuplot gmail-message-mode gitignore-templates git-timemachine git-modes git-messenger git-link gh-md gandalf-theme fold-this flyspell-popup flyspell-correct-helm flymd flycheck-pos-tip flycheck-package flycheck-elsa flx-ido flatui-theme flatland-theme fira-code-mode farmhouse-themes fancy-battery eziam-themes eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-evilified-state evil-escape evil-easymotion evil-collection evil-cleverparens evil-args evil-anzu eval-sexp-fu ess-R-data-view espresso-theme eshell-z eshell-prompt-extras esh-help engine-mode emr emoji-cheat-sheet-plus emmet-mode elisp-slime-nav elisp-demos elisp-def elfeed-org elfeed-goodies ef-themes edit-server edit-indirect ediprolog eat dumb-jump drag-stuff dracula-theme dotenv-mode doom-themes dockerfile-mode docker django-theme disable-mouse direnv dired-quick-sort diminish diff-hl devdocs darktooth-theme darkokai-theme darkmine-theme dakrone-theme cython-mode cyberpunk-theme csv-mode copy-as-format copilot company-web company-statistics company-restclient company-quickhelp company-posframe company-emoji company-box company-ansible command-log-mode column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode code-review code-cells cmake-mode clues-theme clean-aindent-mode chocolate-theme cherry-blossom-theme centered-cursor-mode busybee-theme bubbleberry-theme browse-at-remote bm blacken birds-of-paradise-plus-theme better-jumper badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile atomic-chrome apropospriate-theme anti-zenburn-theme ansible-doc ansible ample-zen-theme ample-theme all-the-icons alect-themes aggressive-indent afternoon-theme add-node-modules-path ace-link ace-jump-helm-line))
 '(paradox-github-token t)
 '(safe-local-variable-values
   '((org-enforce-todo-checkbox-dependencies . t)
     (projectile-project-name . "org-files")
     (typescript-backend . tide)
     (typescript-backend . lsp)
     (javascript-backend . tide)
     (javascript-backend . tern)
     (javascript-backend . lsp))))
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
 '(org-document-title ((t (:inherit default :weight normal :font "Source Sans Pro" :height 2.5 :underline nil))))
 '(org-done ((t (:font "Fira Sans" :height 1.0 :weight bold))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit default :weight normal :font "Source Sans Pro" :height 1.8))))
 '(org-level-2 ((t (:inherit default :weight normal :font "Source Sans Pro" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight normal :font "Source Sans Pro" :height 1.4))))
 '(org-level-4 ((t (:inherit default :weight normal :font "Source Sans Pro" :height 1.3))))
 '(org-level-5 ((t (:inherit default :weight normal :font "Source Sans Pro" :height 1.2))))
 '(org-level-6 ((t (:inherit default :weight normal :font "Source Sans Pro" :height 1.2))))
 '(org-level-7 ((t (:inherit default :weight normal :font "Source Sans Pro" :height 1.2))))
 '(org-level-8 ((t (:inherit default :weight normal :font "Source Sans Pro" :height 1.2))))
 '(org-link ((t (:underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :height 0.5))))
 '(org-todo ((t (:font "Fira Sans" :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "Source Sans Pro" :height 1.1)))))
