;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ansible
     bm
     cmake
     elm
     graphviz
     rust
     sql
     spacemacs-modeline
     csv
     html
     (javascript :variables
                 javascript-repl 'nodejs) ;; includes Coffeescript support
     (typescript :variables
                 typescript-backend 'tide
                 typescript-linter 'tslint
                 tide-tsserver-executable "/Users/rodk/.asdf/installs/nodejs/14.19.0/.npm/bin/tsserver")
     react
     coffeescript
     (lua :variables
          lua-backend 'lsp-emmy
          lua-lsp-emmy-jar-path "~/.emacs.d/EmmyLua-LS-all.jar" ; default path
          lua-lsp-emmy-java-path "java"                         ; default path
          lua-lsp-emmy-enable-file-watchers t)                  ; enabled default

     markdown
     ;; (csharp :variables csharp-backend 'lsp)
     (lsp :variables
          lsp-file-watch-threshold 2000
          lsp-navigation 'peek
          lsp-enable-symbol-highlighting nil
          lsp-headerline-breadcrumb-enable t
          lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)
          )
     (python :variables
             python-fill-column 99
             ;; python-sort-imports-on-save t
             python-auto-set-local-pyenv-version 'on-visit
             python-test-runner 'pytest
             python-backend 'lsp
             python-lsp-server 'pylsp
             ;; python-lsp-server 'pyright
             ;; python-lsp-server 'mspyls
             ;; python-lsp-git-root "~/github/python-language-server"
             python-formatter 'black
             ;; python-format-on-save t
             )
     ipython-notebook
     semantic
     emacs-lisp

     (yaml :variables
           yaml-enable-lsp t)
     (plantuml :variables
               plantuml-jar-path "/opt/homebrew/opt/plantuml/libexec/plantuml.jar"
               org-plantuml-jar-path "/opt/homebrew/opt/plantuml/libexec/plantuml.jar")

     command-log
     restclient
     (elfeed :variables
             elfeed-db-directory "~/Dropbox/elfeed/"
             rmh-elfeed-org-files (list "~/Dropbox/org/elfeed.org"))

     (colors :variables
             colors-colorize-identifiers 'all)
     emoji
     theming
     ibuffer
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     imenu-list
     (osx :variables
          osx-command-as nil)
     chrome
     helm
     pdf
     xkcd
     docker
     (auto-completion :variables
                      ;; auto-completion-enable-help-tooltip 'manual
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-use-company-box t
                      )
     (better-defaults :variables
                      better-defaults-move-to-beginning-of-code-first t
                      better-defaults-move-to-end-of-code-first t)
     git
     (shell :variables
            shell-default-shell 'ansi-term
            shell-default-term-shell "/bin/zsh"
            shell-default-height 50
            shell-default-position 'right
            shell-enable-smart-eshell t
            close-window-with-terminal t)

     (syntax-checking)
     copy-as-format
     (version-control :variables
                      version-control-diff-side 'left)
     themes-megapack
     (treemacs :variables
               treemacs-sorting 'alphabetic-asc
               ;; treemacs-use-follow-mode 'tag
               treemacs-use-git-mode 'deferred
               treemacs-use-scope-type 'Perspectives
               treemacs-use-filewatch-mode nil)

     (wakatime :variables
               wakatime-api-key "c3241a98-9066-4792-87de-163047db98b3"
               wakatime-cli-path "/opt/homebrew/bin/wakatime-cli")

     (org :variables
          org-enable-bootstrap-support t
          org-enable-github-support t
          ;; org-enable-jira-support t
          org-enable-notifications t
          org-enable-org-journal-support t
          org-enable-reveal-js-support t
          org-start-notification-daemon-on-startup t
          org-enable-roam-support t
          org-enable-roam-ui t)

     (unicode-fonts :variables
                    unicode-fonts-enable-ligatures t
                    )
     ;; Private layers
     rk-layout
     ;; rk-org
     jekyll
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      atomic-chrome
                                      brutalist-theme
                                      direnv
                                      editorconfig
                                      eink-theme
                                      evil-easymotion
                                      github-review
                                      highlight-indent-guides
                                      fira-code-mode
                                      fold-this
                                      jira-markup-mode
                                      keychain-environment
                                      ob-async
                                      org-jira
                                      polymode
                                      pipenv
                                      salt-mode
                                      sicp
                                      s3ed
                                      wsd-mode
                                      xcscope
                                      yasnippet-snippets
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    ;; all-the-icons
                                    ;; spaceline
                                    ;; spaceline-all-the-icons
                                    ;; forge
                                    ;; closql
                                    ;; ghub
                                    )

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024 8)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent t

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable t

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         material
                         naquadah
                         professional
                         phoenix-dark-pink
                         soft-charcoal
                         soft-morning
                         ujelly
                         wheatgrass
                         spacemacs-light
                         spacemacs-dark
                         tangotango
                         cyberpunk
                         )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   ;; dotspacemacs-default-font '("Source Code Pro"
   dotspacemacs-default-font '(
                               ("Inconsolata Nerd Font"
                                :size 22.0
                                :weight normal
                                :width normal)
                               ("Monoid Nerd Font"
                                :size 13.0
                                :weight normal
                                :width normal)
                               ("VictorMono Nerd Font"
                                :size 14.0
                                :weight normal
                                :width normal)
                               ("Iosevka Nerd Font"
                                :size 14.0
                                :weight normal
                                :width normal)
                               ("Cousine Nerd Font"
                                :size 13.0
                                :weight normal
                                :width normal)
                               ("IMWritingMonoS Nerd Font"
                                :size 14.0
                                :weight normal
                                :width normal)
                               ("Hack Nerd Font"
                               :size 14.0
                               :weight normal
                               :width normal)
                               ("Hack"
                                :size 14.0
                                :weight normal
                                :width normal)
                               ("MesloLGS NF"
                                :size 14.0
                                :weight normal
                                :width normal)
                               ("SauceCodePro Nerd Font"
                                :size 14.0
                                :weight normal
                                :width normal)
                               )

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names t

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state t

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers '(:relative nil
                              :visible t
                              :disabled-for-modes dired-mode
                                                  doc-view-mode
                                                  markdown-mode
                                                  org-mode
                                                  pdf-view-mode
                                                  text-mode
                                                  xml-mode
                                                  sgml-mode
                              :size-limit-kb 1000)
   ;; dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   ;; dotspacemacs-server-socket-dir "~/.emacs.d/server"
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I | %t:%f"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  ;; (define-derived-mode anaconda-view-mode special-mode "Anaconda-View")
  (add-to-list 'configuration-layer-elpa-archives '("melpa-stable" . "stable.melpa.org/packages/"))
  ;; (add-to-list 'package-pinned-packages '(spaceline . "melpa-stable"))
  ;; (add-to-list 'package-pinned-packages '(spaceline-all-the-icons . "melpa-stable"))
  ;; (add-to-list 'package-pinned-packages '(all-the-icons . "melpa-stable"))
  (defun markdown-html (buffer)
    (princ (with-current-buffer buffer
             (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
           (current-buffer)))

  (defun markdown-preview-like-god ()
    (interactive)
    (impatient-mode 1)
    (setq imp-user-filter #'markdown-html)
    (cl-incf imp-last-state)
    (imp--notify-clients))

  (advice-add 'evil-avy-goto-line :after #'evil-scroll-line-to-center)
  (advice-add 'org-open-at-point :after #'evil-scroll-line-to-center)
  (advice-add 'evil-ex-search-next :after #'evil-scroll-line-to-center)

  (with-eval-after-load 'completion
    (defun spacemacs/helm-files-do-rg (&optional dir)
      "Search in files with `rg'."
      (interactive)
      ;; --line-number forces line numbers (disabled by default on windows)
      ;; no --vimgrep because it adds column numbers that wgrep can't handle
      ;; see https://github.com/syl20bnr/spacemacs/pull/8065
      (let* ((root-helm-ag-base-command "rg --smart-case --pcre2 --no-heading --color=never --line-number")
             (helm-ag-base-command (if spacemacs-helm-rg-max-column-number
                                       (concat root-helm-ag-base-command " --max-columns=" (number-to-string spacemacs-helm-rg-max-column-number))
                                     root-helm-ag-base-command)))
        (helm-do-ag dir)))
    )

  (with-eval-after-load 'unicode-fonts
    
    ;; Enable the www ligature in every possible major mode
    (ligature-set-ligatures 't '("www"))

    ;; Enable ligatures in programming modes                                                           
    (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                        ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                        "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                        "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                        "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                        "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                        "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                        "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                        "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                        "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

(global-ligature-mode 't)

    )

  (with-eval-after-load 'org
    ;; Org config goes here
    ;;

    (setq org-directory "~/Dropbox/org/")
    (setq gtd-directory (concat org-directory "gtd/"))
    (setq org-id-track-globally t)
    (defalias `rk/org-file (apply-partially 'concat org-directory))
    (defalias `rk/gtd-file (apply-partially 'concat gtd-directory))


    (setq org-enable-roam-support t)
    (setq org-roam-directory (concat org-directory "roam-notes/"))
    (setq org-roam-completion-everywhere t)
    (add-to-list 'spacemacs-default-company-backends 'company-capf)
    (setq org-roam-dailies-capture-templates
          '(("d" "default" entry
             "\n* %?"
             :target (file+head "%<%Y-%m-%d>.org"
                                "#+title: %<%Y-%m-%d>\n"))))
    (org-roam-db-autosync-mode)

    (add-to-list 'org-modules 'org-protocol)
    (add-to-list 'org-modules 'org-tempo)
    (add-to-list 'org-modules 'ox-jira)
    (add-to-list 'org-modules 'org-checklist)

    (setq org-tags-exclude-from-inheritance (list "project"))
    (setq org-list-allow-alphabetical t)

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
              ;; (tags "30days" ((org-agenda-overriding-header "30 Day Plan")))
              ;; (tags "60days" ((org-agenda-overriding-header "60 Day Plan")))
              ;; (tags "90days" ((org-agenda-overriding-header "90 Day Plan")))
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

    (setq org-roam-dailies-capture-templates
          '(("d" "default" entry "* %<%I:%M %p>: %?"
             :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

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
                                    (list '("n" . "notes"))))))
  (with-eval-after-load 'direnv
    (direnv-mode))

  (with-eval-after-load 'ansible
    (add-hook 'ansible-hook 'ansible-auto-decrypt-encrypt)
    (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
    (add-to-list 'company-backends 'company-ansible))

  (add-hook 'term-mode-hook
            (lambda ()
              (setq term-buffer-maximum-size 100000)))

  (add-hook 'nxml-mode-hook (lambda() (hs-minor-mode 1)))

  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>" ;; regexp for start block
                 "-->\\|</[^/>]*[^/]>" ;; regexp for end block
                 "<!--"
                 nxml-forward-element
                 nil))

  (with-eval-after-load 'elfeed
    (defun elfeed-goodies/search-header-draw ()
  "Returns the string to be used as the Elfeed header."
  (if (zerop (elfeed-db-last-update))
      (elfeed-search--intro-header)
    (let* ((separator-left (intern (format "powerline-%s-%s"
                                           elfeed-goodies/powerline-default-separator
                                           (car powerline-default-separator-dir))))
           (separator-right (intern (format "powerline-%s-%s"
                                            elfeed-goodies/powerline-default-separator
                                            (cdr powerline-default-separator-dir))))
           (db-time (seconds-to-time (elfeed-db-last-update)))
           (stats (-elfeed/feed-stats))
           (search-filter (cond
                           (elfeed-search-filter-active
                            "")
                           (elfeed-search-filter
                            elfeed-search-filter)
                           (""))))
      (if (>= (window-width) (* (frame-width) elfeed-goodies/wide-threshold))
          (search-header/draw-wide separator-left separator-right search-filter stats db-time)
        (search-header/draw-tight separator-left separator-right search-filter stats db-time)))))

    (defun elfeed-goodies/entry-line-draw (entry)
      "Print ENTRY to the buffer."

      (let* ((title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
            (date (elfeed-search-format-date (elfeed-entry-date entry)))
            (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
            (feed (elfeed-entry-feed entry))
            (feed-title
              (when feed
                (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
            (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
            (tags-str (concat "[" (mapconcat 'identity tags ",") "]"))
            (title-width (- (window-width) elfeed-goodies/feed-source-column-width
                            elfeed-goodies/tag-column-width 4))
            (title-column (elfeed-format-column
                            title (elfeed-clamp
                                  elfeed-search-title-min-width
                                  title-width
                                  title-width)
                            :left))
            (tag-column (elfeed-format-column
                          tags-str (elfeed-clamp (length tags-str)
                                                elfeed-goodies/tag-column-width
                                                elfeed-goodies/tag-column-width)
                          :left))
            (feed-column (elfeed-format-column
                          feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
                                                    elfeed-goodies/feed-source-column-width
                                                    elfeed-goodies/feed-source-column-width)
                          :left)))

        (if (>= (window-width) (* (frame-width) elfeed-goodies/wide-threshold))
            (progn
              (insert (propertize date 'face 'elfeed-search-date-face) " ")
              (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
              (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
              (insert (propertize title 'face title-faces 'kbd-help title)))
          (insert (propertize title 'face title-faces 'kbd-help title))))))

  (defun codelahoma/insert-random-uid ()
    (interactive)
    (shell-command "printf %s \"$(uuidgen)\"" t))


  (add-hook 'projectile-after-switch-project-hook
               #'codelahoma/switch-node-version)

  (load-file "/Users/rodk/.emacs.d/private/local/narrow-indirect.el")

  (defun apply-function-to-region (fn)
    "Apply a function to a region."
    (interactive "Function to apply to region: ")
    (save-excursion
      (let* ((beg (region-beginning))
             (end (region-end))
             (resulting-text
              (funcall fn
                       (buffer-substring-no-properties beg end))))
        (kill-region beg end)
        (insert resulting-text))))

  (defun sort-csv (txt)
    "Sort a comma separated string."
    (mapconcat 'identity
               (sort (split-string txt ",") 'string< ) ","))

  (defun sort-csv-region ()
    "Sort a region of comma separated text."
    (interactive)
    (apply-function-to-region 'sort-csv))

  (defun copy-lines-matching-re (re)
    "find all lines matching the regexp RE in the current buffer
putting the matching lines in a buffer named *matching*"
    (interactive "sRegexp to match: ")
    (let ((result-buffer (get-buffer-create "*matching*")))
      (with-current-buffer result-buffer
        (erase-buffer))
      (save-match-data
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward re nil t)
            (princ (buffer-substring-no-properties (line-beginning-position)
                                                   (line-beginning-position 2))
                   result-buffer))))
      (pop-to-buffer result-buffer)))

  (defun xah-title-case-region-or-line (@begin @end)
    "Title case text between nearest brackets, or current line, or text selection.
  Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

  When called in a elisp program, *begin *end are region boundaries.
  URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
  Version 2017-01-11"
    (interactive
    (if (use-region-p)
        (list (region-beginning) (region-end))
      (let (
            $p1
            $p2
            ($skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
        (progn
          (skip-chars-backward $skipChars (line-beginning-position))
          (setq $p1 (point))
          (skip-chars-forward $skipChars (line-end-position))
          (setq $p2 (point)))
        (list $p1 $p2))))
    (let* (
          ($strPairs [
                      [" A " " a "]
                      [" And " " and "]
                      [" At " " at "]
                      [" As " " as "]
                      [" By " " by "]
                      [" Be " " be "]
                      [" Into " " into "]
                      [" In " " in "]
                      [" Is " " is "]
                      [" It " " it "]
                      [" For " " for "]
                      [" Of " " of "]
                      [" Or " " or "]
                      [" On " " on "]
                      [" Via " " via "]
                      [" The " " the "]
                      [" That " " that "]
                      [" To " " to "]
                      [" Vs " " vs "]
                      [" With " " with "]
                      [" From " " from "]
                      ["'S " "'s "]
                      ["'T " "'t "]
                      ]))
      (save-excursion
        (save-restriction
          (narrow-to-region @begin @end)
          (upcase-initials-region (point-min) (point-max))
          (let ((case-fold-search nil))
            (mapc
            (lambda ($x)
              (goto-char (point-min))
              (while
                  (search-forward (aref $x 0) nil t)
                (replace-match (aref $x 1) "FIXEDCASE" "LITERAL")))
            $strPairs))))))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (setq hyde-home "~/github/codelahoma.github.io")

  (add-to-list 'load-path "/Users/rodk/.emacs.d/private/")
  (require 'asdf)
  (asdf-enable)
  (setq x-select-enable-clipboard nil)
  (define-key evil-visual-state-map (kbd "s-c") (kbd "\"+y"))
  ;; (define-key evil-insert-state-map  (kbd "s-v") (kbd "+"))
  ;; (define-key evil-ex-completion-map (kbd "s-v") (kbd "+"))
  ;; (define-key evil-ex-search-keymap  (kbd "s-v") (kbd "+"))

  (setq easy-jekyll-basedir "~/github/codelahoma.github.io")
  (setq easy-jekyll-url "https://codelahoma.com")

  (evil-leader/set-key "q q" 'spacemacs/frame-killer)
  (evil-leader/set-key "/" 'spacemacs/helm-project-do-ag)


  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  (use-package evil-easymotion
    :init (evilem-default-keybindings "\\"))

  (setq avy-all-windows nil)

  (setq max-specpdl-size 6000)
  (setq lsp-message-project-root-warning t)
  (setq lsp-python-python-executable-cmd "~/.pyenv/shims/python")
  ;; (setq javascript-backend 'tern)

  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))

  (setq vc-handled-backends (delq 'Git vc-handled-backends))

  (setq ghub-use-workaround-for-emacs-bug 'force)

  (setq helm-dash-common-docsets
        '(
          "AngularJS"
          "Bash"
          "Django"
          "Docker"
          "Emacs_Lisp"
          "JavaScript"
          "Lo-Dash"
          "MySQL"
          "Python_2"
          "Python_3"
          ;; "django-rest-framework-3.8.2"
          ))

  ;; (setq-default git-magit-status-fullscreen t)

  (setq helm-ag-base-command "/opt/homebrew/bin/rg --vimgrep --no-heading --smart-case")

  (setq multi-term-program "/bin/zsh")

  (setq backup-directory-alist
        `(,(concat user-emacs-directory "backups")))

  (setq create-lockfiles nil)

  ;; (defun rk-python-hook ()
  ;;   (progn 
  ;;     (highlight-lines-matching-regexp "^ *class " 'hi-salmon)
  ;;     (highlight-lines-matching-regexp "^ *def " 'hi-aquamarine)))

  ;; (add-hook 'python-mode-hook #'rk-python-hook)

  (with-eval-after-load 'python-mode
    (flycheck-select-checker 'python-flake8))


  ;; (load "~/Dropbox/emacs/.slack_auth.el")

  (setq projectile-enable-caching t)
  (set-face-attribute 'mode-line nil :height 1.08)

  (defun rk-bump-mode-fonts()
    "Increase the mode-line font sizes for my old eyes"
    (let ((faces '(mode-line
                   mode-line-buffer-id
                   mode-line-emphasis
                   mode-line-highlight
                   mode-line-inactive)))
      (mapc
       (lambda (face) (set-face-attribute face nil :font "Inconsolata for Powerline-18"))
       faces)))

  (add-hook 'spacemacs-post-theme-change-hook
            'rk-bump-mode-fonts)

  (with-eval-after-load 'sql-mode
    (setq sql-mysql-login-params (append sql-mysql-login-params '(port)))
    )

  (with-eval-after-load 'coffee-mode
    (add-hook 'coffee-mode-hook
              'editorconfig-apply t))

  (defun my-flymd-browser-function (url)
    (let ((process-environment (browse-url-process-environment)))
      (apply 'start-process
             (concat "firefox " url)
             nil
             "/usr/bin/open"
             (list "-a" "firefox" url))))
  (setq flymd-browser-open-function 'my-flymd-browser-function)


  (add-hook 'rjsx-mode #'lsp-javascript-typescript-enable)

  (setq js2-strict-missing-semi-warning nil)

  (defun rk/reset-hammerspoon ()
    (interactive)
    (shell-command "hs -c \"hs.reload()\""))


  (load-framegeometry)

  (spacemacs/declare-prefix "ob" "buffer")
  (spacemacs/set-leader-keys "obn" 'spacemacs/new-empty-buffer)

  (spacemacs/declare-prefix "oc" "copy")
  (spacemacs/set-leader-keys "ocl" 'avy-copy-line)
  (spacemacs/set-leader-keys "ocp" 'forge-copy-url-at-point-as-kill)

  (spacemacs/declare-prefix "of" "folding")
  (spacemacs/set-leader-keys
    "off" 'fold-this
    "ofm" 'fold-this-all
    "ofr" 'fold-this-unfold-all)

  (spacemacs/declare-prefix "ox" "text")
  (spacemacs/set-leader-keys "oxt" 'xah-title-case-region-or-line)

  (spacemacs/declare-prefix "oh" "Hammerspoon")
  (spacemacs/set-leader-keys "ohr" 'rk/reset-hammerspoon)

  (spacemacs/declare-prefix "oo" "org")
  (spacemacs/set-leader-keys "oos" 'org-save-all-org-buffers)
  (spacemacs/declare-prefix "oor" "org-roam")


  (spacemacs/declare-prefix "ooj" "jira/journal")
  (spacemacs/declare-prefix "oojp" "projects")
  (spacemacs/declare-prefix "ooji" "issues")
  (spacemacs/declare-prefix "oojs" "subtasks")
  (spacemacs/declare-prefix "oojc" "comments")
  (spacemacs/declare-prefix "oojt" "todos")
  (spacemacs/set-leader-keys
    "oojj" 'org-roam-dailies-capture-today
    "oojf" 'org-roam-dailies-goto-today
    "oojpg" 'org-jira-get-projects
    "oojib" 'org-jira-browse-issue
    "oojig" 'org-jira-get-issues
    "oojih" 'org-jira-get-issues-headonly
    "oojif" 'org-jira-get-issues-from-filter-headonly
    "oojiu" 'org-jira-update-issue
    "oojiw" 'org-jira-progress-issue
    "oojir" 'org-jira-refresh-issue
    "oojic" 'org-jira-create-issue
    "oojiy" 'org-jira-copy-current-issue-key
    "oojsc" 'org-jira-create-subtask
    "oojsg" 'org-jira-get-subtasks
    "oojcu" 'org-jira-update-comment
    "oojtj" 'org-jira-todo-to-jira
    "oorj" 'org-roam-dailies-capture-today)
 )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included
in the dump."
  (load-file "/Users/rodk/.emacs.d/private/local/narrow-indirect.el")
  (with-temp-buffer (org-mode))
  )

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#080808" "#d70000" "#67b11d" "#875f00" "#268bd2" "#af00df" "#00ffff" "#b2b2b2"])
 '(asdf-binary "/opt/homebrew/bin/asdf")
 '(column-number-mode t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(elfeed-goodies/entry-pane-position 'bottom)
 '(emms-mode-line-icon-color "#1ba1a1")
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };"))
 '(evil-want-Y-yank-to-eol nil)
 '(exec-path-from-shell-arguments '("-l"))
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#eee8d5" t)
 '(flycheck-disabled-checkers '(python-pylint))
 '(fringe-mode 4 nil (fringe))
 '(global-display-line-numbers-mode t)
 '(gnus-logo-colors '("#4c8383" "#bababa") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };") t)
 '(helm-completion-style 'emacs)
 '(helm-mode t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-parentheses-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-sexp-background-color "#1c1f26")
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "#dc752f")))
 '(jdee-db-active-breakpoint-face-colors (cons "#1E2029" "#bd93f9"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1E2029" "#50fa7b"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1E2029" "#565761"))
 '(lsp-pyls-configuration-sources ["flake8"])
 '(lsp-pyls-plugins-pycodestyle-enabled nil)
 '(lsp-pyls-plugins-pylint-enabled nil)
 '(lsp-pyright-multi-root nil)
 '(lsp-ui-doc-border "#93a1a1")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(objed-cursor-color "#ff5555")
 '(org-agenda-files
   '("/Users/rodk/Dropbox/org/gtd/inbox.org" "/Users/rodk/Dropbox/org/gtd/gtd.org" "/Users/rodk/Dropbox/org/gtd/tickler.org" "/Users/rodk/Dropbox/org/gtd/someday.org" "/Users/rodk/Dropbox/org/gtd/reference.org"))
 '(org-agenda-sorting-strategy
   '((agenda habit-down time-up priority-down category-keep)
     (todo priority-down category-keep)
     (tags priority-down todo-state-up)
     (search category-keep)))
 '(package-selected-packages
   '(fira-code-mode org-roam compat zenburn-theme zen-and-art-theme yasnippet-snippets yapfify xterm-color xcscope wsd-mode ws-butler winum white-sand-theme which-key web-mode web-beautify wakatime-mode volatile-highlights vi-tilde-fringe uuidgen use-package unfill undo-tree underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme toml-mode toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit swift-mode sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sql-indent spaceline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode slack emojify circe oauth2 sicp shell-pop seti-theme scss-mode sass-mode salt-mode mmm-jinja2 yaml-mode s3ed reverse-theme reveal-in-osx-finder restclient-helm restart-emacs rebecca-theme rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme racer pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme plantuml-mode planet-theme pipenv pyvenv pip-requirements phpunit phpcbf php-extras php-auto-yasnippets phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pbcopy paradox spinner ox-twbs ox-reveal ox-gfm osx-trash osx-dictionary orgit organic-green-theme org-projectile org-category-capture org-present org-pomodoro org-mime org-jira org-download org-bullets open-junk-file omtose-phellack-theme omnisharp oldlace-theme occidental-theme obsidian-theme ob-restclient ob-http ob-async nvm noctilux-theme neotree naquadah-theme mwim mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow magit-popup magit-gh-pulls madhat2r-theme macrostep lush-theme lua-mode lorem-ipsum livid-mode skewer-mode live-py-mode linum-relative link-hint light-soap-theme launchctl js2-refactor multiple-cursors js2-mode js-doc jira-markup-mode jbeans-theme jazz-theme ir-black-theme inkpot-theme indent-guide imenu-list ibuffer-projectile hydra lv hy-mode dash-functional hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation heroku-theme hemisu-theme helm-themes helm-swoop helm-pydoc helm-projectile projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme haml-mode gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gmail-message-mode ham-mode html-to-markdown gitignore-mode github-search github-review a ghub treepy github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh marshal logito pcache ht gh-md geiser gandalf-theme fuzzy fold-this flyspell-popup flyspell-correct-helm flyspell-correct flymd flycheck-rust flycheck-pos-tip pos-tip flycheck pkg-info epl flx-ido flx flatui-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exotica-theme exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit git-commit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-easymotion evil-args evil-anzu anzu evil goto-chg eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help emoji-cheat-sheet-plus emmet-mode elisp-slime-nav elfeed-web simple-httpd elfeed-org elfeed-goodies ace-jump-mode noflet powerline popwin elfeed eink-theme ein with-editor polymode deferred request anaphora editorconfig edit-server dumb-jump drupal-mode php-mode dracula-theme dockerfile-mode docker transient tablist json-mode docker-tramp json-snatcher json-reformat django-theme disaster direnv diminish diff-hl darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csv-mode csharp-mode company-web web-completion-data company-statistics company-restclient restclient know-your-http-well company-emoji company-c-headers company-anaconda company command-log-mode column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode coffee-mode cmake-mode clues-theme clean-aindent-mode clang-format cherry-blossom-theme cargo markdown-mode rust-mode busybee-theme bubbleberry-theme brutalist-theme birds-of-paradise-plus-theme bind-map bind-key badwolf-theme auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed atomic-chrome websocket apropospriate-theme anti-zenburn-theme anaconda-mode pythonic f dash s ample-zen-theme ample-theme alert log4e gntp alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup flatland-theme))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors '("#655370" . "#fbf8ef"))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(projectile-tags-command "find . -name '*.py' | xargs etags")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(transient-mark-mode nil)
 '(treemacs-sorting 'alphabetic-asc)
 '(vc-annotate-background t)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(vterm-max-scrollback 100000)
 '(wakatime-python-bin nil)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(cursor ((t (:background "#b58900"))))
 '(helm-selection ((t (:foreground "white" :background "red" :inverse-video nil))))
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t))
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#080808" "#d70000" "#67b11d" "#875f00" "#268bd2" "#af00df" "#00ffff" "#b2b2b2"])
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   '(zenburn-theme zen-and-art-theme yasnippet-snippets yapfify xterm-color xcscope wsd-mode ws-butler winum white-sand-theme which-key web-mode web-beautify wakatime-mode volatile-highlights vi-tilde-fringe uuidgen use-package unfill undo-tree underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme toml-mode toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit swift-mode sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sql-indent spaceline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode slack emojify circe oauth2 sicp shell-pop seti-theme scss-mode sass-mode salt-mode mmm-jinja2 yaml-mode s3ed reverse-theme reveal-in-osx-finder restclient-helm restart-emacs rebecca-theme rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme racer pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme plantuml-mode planet-theme pipenv pyvenv pip-requirements phpunit phpcbf php-extras php-auto-yasnippets phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pbcopy paradox spinner ox-twbs ox-reveal ox-gfm osx-trash osx-dictionary orgit organic-green-theme org-projectile org-category-capture org-present org-pomodoro org-mime org-jira org-download org-bullets open-junk-file omtose-phellack-theme omnisharp oldlace-theme occidental-theme obsidian-theme ob-restclient ob-http ob-async nvm noctilux-theme neotree naquadah-theme mwim mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow magit-popup magit-gh-pulls madhat2r-theme macrostep lush-theme lua-mode lorem-ipsum livid-mode skewer-mode live-py-mode linum-relative link-hint light-soap-theme launchctl js2-refactor multiple-cursors js2-mode js-doc jira-markup-mode jbeans-theme jazz-theme ir-black-theme inkpot-theme indent-guide imenu-list ibuffer-projectile hydra lv hy-mode dash-functional hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation heroku-theme hemisu-theme helm-themes helm-swoop helm-pydoc helm-projectile projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme haml-mode gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gmail-message-mode ham-mode html-to-markdown gitignore-mode github-search github-review a ghub treepy github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh marshal logito pcache ht gh-md geiser gandalf-theme fuzzy fold-this flyspell-popup flyspell-correct-helm flyspell-correct flymd flycheck-rust flycheck-pos-tip pos-tip flycheck pkg-info epl flx-ido flx flatui-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exotica-theme exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit git-commit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-easymotion evil-args evil-anzu anzu evil goto-chg eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help emoji-cheat-sheet-plus emmet-mode elisp-slime-nav elfeed-web simple-httpd elfeed-org elfeed-goodies ace-jump-mode noflet powerline popwin elfeed eink-theme ein with-editor polymode deferred request anaphora editorconfig edit-server dumb-jump drupal-mode php-mode dracula-theme dockerfile-mode docker transient tablist json-mode docker-tramp json-snatcher json-reformat django-theme disaster direnv diminish diff-hl darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csv-mode csharp-mode company-web web-completion-data company-statistics company-restclient restclient know-your-http-well company-emoji company-c-headers company-anaconda company command-log-mode column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode coffee-mode cmake-mode clues-theme clean-aindent-mode clang-format cherry-blossom-theme cargo markdown-mode rust-mode busybee-theme bubbleberry-theme brutalist-theme birds-of-paradise-plus-theme bind-map bind-key badwolf-theme auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed atomic-chrome websocket apropospriate-theme anti-zenburn-theme anaconda-mode pythonic f dash s ample-zen-theme ample-theme alert log4e gntp alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup flatland-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
