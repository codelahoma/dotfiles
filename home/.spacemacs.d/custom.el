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
   '(ace-jump-helm-line ace-link add-node-modules-path afternoon-theme
                        aggressive-indent alect-themes all-the-icons ample-theme
                        ample-zen-theme ansible ansible-doc anti-zenburn-theme
                        apropospriate-theme atomic-chrome auto-compile
                        auto-dictionary auto-highlight-symbol auto-yasnippet
                        badwolf-theme better-jumper birds-of-paradise-plus-theme
                        blacken bm browse-at-remote bubbleberry-theme
                        busybee-theme centered-cursor-mode cherry-blossom-theme
                        chocolate-theme clean-aindent-mode clues-theme
                        cmake-mode code-cells code-review color-identifiers-mode
                        color-theme-sanityinc-solarized
                        color-theme-sanityinc-tomorrow column-enforce-mode
                        command-log-mode company-anaconda company-ansible
                        company-box company-emoji company-lua company-quickhelp
                        company-restclient company-statistics company-web
                        copilot copy-as-format csv-mode cyberpunk-theme
                        cython-mode dakrone-theme dap-mode darkmine-theme
                        darkokai-theme darktooth-theme devdocs diff-hl diminish
                        dired-quick-sort direnv disable-mouse django-theme
                        docker dockerfile-mode doom-themes dotenv-mode
                        dracula-theme drag-stuff dumb-jump eat ediprolog
                        edit-server ef-themes elfeed-goodies elfeed-org
                        elisp-def elisp-demos elisp-slime-nav emmet-mode
                        emoji-cheat-sheet-plus emr engine-mode esh-help
                        eshell-prompt-extras eshell-z espresso-theme
                        ess-R-data-view eval-sexp-fu evil-anzu evil-args
                        evil-cleverparens evil-collection evil-easymotion
                        evil-escape evil-evilified-state evil-exchange
                        evil-goggles evil-iedit-state evil-indent-plus evil-lion
                        evil-lisp-state evil-matchit evil-mc evil-nerd-commenter
                        evil-numbers evil-org evil-surround evil-textobj-line
                        evil-tutor evil-unimpaired evil-visual-mark-mode
                        evil-visualstar exotica-theme expand-region eyebrowse
                        eziam-themes fancy-battery farmhouse-themes
                        fira-code-mode flatland-theme flatui-theme flx-ido
                        flycheck-elsa flycheck-package flycheck-pos-tip flymd
                        flyspell-correct-helm fold-this gandalf-theme gh-md
                        git-link git-messenger git-modes git-timemachine
                        gitignore-templates gmail-message-mode gnuplot
                        golden-ratio google-translate gotham-theme gptel
                        grandshell-theme graphviz-dot-mode gruber-darker-theme
                        gruvbox-theme hc-zenburn-theme helm-ag helm-c-yasnippet
                        helm-comint helm-company helm-css-scss helm-ctest
                        helm-descbinds helm-git-grep helm-ls-git helm-lsp
                        helm-make helm-mode-manager helm-mu helm-org-ql
                        helm-org-rifle helm-pass helm-projectile helm-purpose
                        helm-pydoc helm-swoop helm-themes helm-xref helpful
                        hemisu-theme heroku-theme hide-comnt
                        highlight-indent-guides highlight-indentation
                        highlight-numbers highlight-parentheses hl-todo
                        holy-mode hungry-delete hy-mode hybrid-mode
                        ibuffer-projectile impatient-mode importmagic
                        indent-guide info+ inkpot-theme inspector ir-black-theme
                        jazz-theme jbeans-theme jinja2-mode jira-markup-mode
                        js-doc js2-refactor json-mode json-navigator
                        json-reformat kaolin-themes keycast keychain-environment
                        launchctl light-soap-theme live-py-mode livid-mode
                        lorem-ipsum lsp-origami lsp-pyright lsp-ui lush-theme
                        macrostep madhat2r-theme majapahit-themes markdown-toc
                        material-theme mermaid-mode minimal-theme modus-themes
                        moe-theme molokai-theme monochrome-theme monokai-theme
                        mu4e-alert multi-line multi-term multi-vterm
                        mustang-theme nameless naquadah-theme noctilux-theme
                        nodejs-repl npm-mode nyan-mode ob-async ob-http ob-hy
                        ob-mermaid ob-restclient obsidian-theme occidental-theme
                        oldlace-theme omtose-phellack-themes open-junk-file
                        org-appear org-cliplink org-contrib org-download
                        org-jira org-journal org-mime org-modern org-pomodoro
                        org-present org-projectile org-re-reveal org-rich-yank
                        org-roam-ui org-sticky-header org-superstar
                        org-transclusion org-wild-notifier organic-green-theme
                        orgit-forge osx-clipboard osx-dictionary osx-trash
                        overseer ox-hugo ox-jira ox-slack ox-twbs paradox
                        password-generator password-store-otp pcre2el
                        pdf-view-restore persistent-scratch
                        phoenix-dark-mono-theme phoenix-dark-pink-theme pinboard
                        pip-requirements pipenv pippel planet-theme
                        plantuml-mode poetry prettier-js professional-theme
                        pug-mode purple-haze-theme py-isort pydoc pyenv-mode
                        pylookup pytest quickrun railscasts-theme
                        rainbow-delimiters rainbow-identifiers rainbow-mode
                        rebecca-theme restart-emacs restclient-helm
                        reveal-in-osx-finder reverse-theme rjsx-mode ron-mode
                        rustic sass-mode scss-mode seti-theme shell-pop sicp
                        slim-mode smeargle smyx-theme soft-charcoal-theme
                        soft-morning-theme soft-stone-theme solarized-theme
                        soothe-theme space-doc spacegray-theme spaceline
                        spacemacs-purpose-popwin spacemacs-whitespace-cleanup
                        sphinx-doc sql-indent sqlite3 string-edit-at-point
                        string-inflection subatomic-theme subatomic256-theme
                        sublime-themes sunny-day-theme symbol-overlay symon
                        tagedit tango-2-theme tango-plus-theme tangotango-theme
                        tao-theme term-cursor terminal-here texfrag tide toc-org
                        toml-mode toxi-theme tree-sitter-langs treemacs-evil
                        treemacs-icons-dired treemacs-magit treemacs-persp
                        treemacs-projectile ts-fold twilight-anti-bright-theme
                        twilight-bright-theme twilight-theme typescript-mode
                        ujelly-theme underwater-theme undo-fu undo-fu-session
                        unkillable-scratch vi-tilde-fringe vim-powerline
                        volatile-highlights vundo wakatime-mode web-beautify
                        web-mode white-sand-theme winum writeroom-mode ws-butler
                        wsd-mode yaml-mode yapfify yasnippet-snippets
                        zen-and-art-theme zenburn-theme zonokai-emacs))
 '(paradox-github-token t)
 '(safe-local-variable-values
   '((evil-shift-width . 4) (projectile-project-name . "org-files")
     (typescript-backend . tide) (typescript-backend . lsp)
     (javascript-backend . tide) (javascript-backend . tern)
     (javascript-backend . lsp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
