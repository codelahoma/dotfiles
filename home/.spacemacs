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

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     csv
     swift
     (sql :variables
          sql-capitalize-words t)
     html
     javascript ;; includes Coffeescript support
     coffeescript
     lua
     markdown
     (python :variables
             python-fill-column 99
             python-sort-imports-on-save t
             python-auto-set-local-pyenv-version 'on-project-switch
             python-test-runner 'pytest
             python-backend 'lsp)
     ipython-notebook
     emacs-lisp
     ruby
     yaml
     lsp
     command-log
     restclient
     (elfeed :variables
             elfeed-db-directory "~/Dropbox/elfeed/"
             rmh-elfeed-org-files (list "~/Dropbox/org/elfeed.org"))

     vinegar
     ;; (colors :variables colors-colorize-identifiers 'all)
     emoji
     theming
     ibuffer
     imenu-list
     (osx :variables
          osx-command-as nil)
     chrome
     helm
     pdf
     xkcd
     docker
     django
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t)
     better-defaults
     git
     github
     evil-commentary
     (shell :variables
            shell-default-shell 'multi-term
            shell-default-term-shell "/bin/zsh" 
            shell-default-height 50
            shell-default-position 'bottom)

     ranger
     syntax-checking
     (version-control :variables
                      version-control-diff-side 'left)
     themes-megapack
     multiple-cursors
     treemacs

     rk-layout
     rk-org
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      atomic-chrome
                                      direnv
                                      edit-server
                                      editorconfig
                                      helm-dash
                                      jira-markup-mode
                                      magit-lfs
                                      nvm
                                      ob-async
                                      pipenv
                                      s3ed
                                      wsd-mode
                                      yasnippet-snippets
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

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
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

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

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

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

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 10)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         material
                         grandshell
                         doom-dracula
                         light-blue
                         lush
                         soothe
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(all-the-icons :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 16
                               :weight normal
                               :width normal)

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
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
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
   dotspacemacs-auto-generate-layout-names nil

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
   dotspacemacs-enable-paste-transient-state nil

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
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers '(:relative nil
      :disabled-for-modes dired-mode
                          doc-view-mode
                          markdown-mode
                          org-mode
                          pdf-view-mode
                          text-mode
                          xml-mode
                          sgml-mode
      :size-limit-kb 1000)
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
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
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

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
  (add-to-list 'package-pinned-packages '(spaceline . "melpa-stable"))
  (add-to-list 'package-pinned-packages '(spaceline-all-the-icons . "melpa-stable"))
  (add-to-list 'package-pinned-packages '(all-the-icons . "melpa-stable"))

  (with-eval-after-load 'direnv
    (direnv-mode))

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

  (defun codelahoma/switch-node-version ()
    (let (dir (projectile-project-root))
      (nvm-use-for dir)))

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
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (setq max-specpdl-size 6000)
  (setq lsp-message-project-root-warning t)
  ;; (setq javascript-backend 'tern)

  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))

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

  (setq theming-modifications
        '((alect-black
           ;; Provide a sort of "on-off" modeline whereby the current buffer has a nice
           ;; bright blue background, and all the others are in cream.
           ;; TODO: Change to use variables here. However, got error:
           ;; (Spacemacs) Error in dotspacemacs/user-config: Wrong type argument: stringp, pd-blue
           (mode-line :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (mode-line-buffer-id :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (powerline-active1 :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (powerline-active2 :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (mode-line-inactive :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           (mode-line-buffer-id-inactive :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           (powerline-inactive1 :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           (powerline-inactive2 :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           ;; Make a really prominent helm selection line.
           (helm-selection :foreground "white" :background "red" :inverse-video nil)
           ;; See comment above about dotspacemacs-colorize-cursor-according-to-state.
           (cursor :background "#b58900")
           )
          (solarized-dark
           ;; Provide a sort of "on-off" modeline whereby the current buffer has a nice
           ;; bright blue background, and all the others are in cream.
           ;; TODO: Change to use variables here. However, got error:
           ;; (Spacemacs) Error in dotspacemacs/user-config: Wrong type argument: stringp, pd-blue
           (mode-line :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (mode-line-buffer-id :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (powerline-active1 :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (powerline-active2 :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (mode-line-inactive :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           (mode-line-buffer-id-inactive :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           (powerline-inactive1 :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           (powerline-inactive2 :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           ;; Make a really prominent helm selection line.
           (helm-selection :foreground "white" :background "red" :inverse-video nil)
           ;; See comment above about dotspacemacs-colorize-cursor-according-to-state.
           (cursor :background "#b58900")
           )
          (alect-light
           ;; Provide a sort of "on-off" modeline whereby the current buffer has a nice
           ;; bright blue background, and all the others are in cream.
           ;; TODO: Change to use variables here. However, got error:
           ;; (Spacemacs) Error in dotspacemacs/user-config: Wrong type argument: stringp, pd-blue
           (mode-line :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (mode-line-buffer-id :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (powerline-active1 :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (powerline-active2 :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (mode-line-inactive :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           (mode-line-buffer-id-inactive :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           (powerline-inactive1 :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           (powerline-inactive2 :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           ;; Make a really prominent helm selection line.
           (helm-selection :foreground "white" :background "red" :inverse-video nil)
           ;; See comment above about dotspacemacs-colorize-cursor-according-to-state.
           (cursor :background "#b58900")
           )(solarized-light
           ;; Provide a sort of "on-off" modeline whereby the current buffer has a nice
           ;; bright blue background, and all the others are in cream.
           ;; TODO: Change to use variables here. However, got error:
           ;; (Spacemacs) Error in dotspacemacs/user-config: Wrong type argument: stringp, pd-blue
           (mode-line :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (mode-line-buffer-id :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (powerline-active1 :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (powerline-active2 :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
           (mode-line-inactive :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           (mode-line-buffer-id-inactive :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           (powerline-inactive1 :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           (powerline-inactive2 :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
           ;; Make a really prominent helm selection line.
           (helm-selection :foreground "white" :background "red" :inverse-video nil)
           ;; See comment above about dotspacemacs-colorize-cursor-according-to-state.
           (cursor :background "#b58900")
           )))

  (setq-default git-magit-status-fullscreen t)

  (setq helm-ag-base-command "/usr/local/bin/ag --vimgrep")

  (setq multi-term-program "/bin/zsh")

  (setq backup-directory-alist
        `(,(concat user-emacs-directory "backups")))

  (setq create-lockfiles nil)


  ;; (load "~/Dropbox/emacs/.slack_auth.el")

  (setq projectile-enable-caching t)

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

  (with-eval-after-load 'magit
    (require 'magit-lfs))

  ;; (with-eval-after-load 'slack
  ;;   ;; (add-hook 'slack-mode-hook #'emojify-mode)

  ;;   (with-eval-after-load 'tracking
  ;;     (define-key tracking-mode-map [f11]
  ;;       #'tracking-next-buffer))

  ;;   (setq slack-buffer-create-on-notify t)
  ;;   ;; (defun slack-user-status (id team) "")

  ;;   ;; (defun endless/-author-at (pos)
  ;;   ;;   (replace-regexp-in-string
  ;;   ;;    (rx "\n" (* anything)) ""
  ;;   ;;    (or (get-text-property pos 'lui-raw-text) "")))

  ;;   ;; (defun endless/-remove-slack-author ()
  ;;   ;;   "Remove author here if it's the same as above."
  ;;   ;;   (let ((author-here (endless/-author-at (point)))
  ;;   ;;         (author-above (endless/-author-at (1- (point)))))
  ;;   ;;     (when (and (looking-at-p (regexp-quote author-here))
  ;;   ;;                (equal author-here author-above))
  ;;   ;;       (delete-region (1- (point))
  ;;   ;;                      (1+ (line-end-position))))))

  ;;   ;; (defun endless/remove-slack-author-hook ()
  ;;   ;;   "For usage in `lui-pre-output-hook'."
  ;;   ;;   (when (derived-mode-p 'slack-mode)
  ;;   ;;     (save-excursion
  ;;   ;;       (goto-char (point-min))
  ;;   ;;       (save-restriction
  ;;   ;;         (widen)
  ;;   ;;         (endless/-remove-slack-author)))))

  ;;   ;; (add-hook 'lui-pre-output-hook
  ;;   ;;           #'endless/remove-slack-author-hook)
  ;;   )

  (defun lsp-set-cfg ()
    (let ((lsp-cfg '(:pyls (:configurationSources ("flake8")))))
      (lsp--set-configuration lsp-cfg)))

  (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg)

  (add-hook 'rjsx-mode #'lsp-javascript-typescript-enable)

  (setq js2-strict-missing-semi-warning nil)


   ;; (defun fix-flake8 (errors)
   ;;   (let ((errors (flycheck-sanitize-errors errors)))
   ;;     (seq-do #'flycheck-flake8-fix-error-level errors)
   ;;     errors))

   ;; (flycheck-define-checker python-flake8-chain
   ;;   "A Python syntax and style checker using flake8"
   ;;   :command ("flake8"
   ;;             "--format=default"
   ;;             (config-file "--config" flycheck-flake8rc)
   ;;             (option "--max-complexity" flycheck-flake8-maximum-complexity nil
   ;;                     flycheck-option-int)
   ;;             (option "--max-line-length" flycheck-flake8-maximum-line-length nil
   ;;                     flycheck-option-int)
   ;;             "-")
   ;;   :standard-input t
   ;;   :error-filter fix-flake8
   ;;   :error-patterns
   ;;   ((warning line-start
   ;;             "stdin:" line ":" (optional column ":") " "
   ;;             (id (one-or-more (any alpha)) (one-or-more digit)) " "
   ;;             (message (one-or-more not-newline))
   ;;             line-end))
   ;;   ;; :next-checkers ((t . python-pylint))
   ;;   :modes python-mode)

   ;; ;; replace flake8 with new chaining one from above
   ;; (setq flycheck-checkers (cons 'python-flake8-chain (delq 'python-flake8 flycheck-checkers)))
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included
in the dump."
  (load-file "/Users/rodk/.emacs.d/private/local/narrow-indirect.el")
  ;; (load "~/Dropbox/emacs/.slack_auth.el")
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
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
\"#######..#\" };")))
 '(evil-want-Y-yank-to-eol t)
 '(exec-path-from-shell-arguments (quote ("-l")))
 '(fci-rule-color "#eee8d5")
 '(gnus-logo-colors (quote ("#4c8383" "#bababa")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
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
\"###########.######\" };")) t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (yapfify pytest pyenv-mode py-isort live-py-mode helm-pydoc cython-mode pip-requirements hy-mode anaconda-mode pythonic atomic-chrome magit-lfs pipenv csv-mode wsd-mode evil-commentary imenu-list company-anaconda rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby company-quickhelp pyvenv sql-indent ranger jira-markup-mode gmail-message-mode ham-mode html-to-markdown flymd edit-server yasnippet-snippets yaml-mode slack emojify circe oauth2 websocket pony-mode ox-twbs ox-reveal ox-gfm org-jira magit-gh-pulls helm-w3m w3m github-search github-clone github-browse-file gist gh marshal logito pcache ht editorconfig dockerfile-mode docker tablist docker-tramp request-deferred multi clojure-snippets clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider queue clojure-mode winum white-sand-theme powerline rebecca-theme org-category-capture org-mime lua-mode helm-spotify-plus exotica-theme ghub ag avy dash-functional tern iedit f goto-chg skewer-mode simple-httpd markdown-mode deferred haml-mode multiple-cursors dash s xterm-color shell-pop multi-term git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter flycheck-pos-tip pos-tip flycheck eshell-z eshell-prompt-extras esh-help diff-hl packed smartparens highlight evil yasnippet projectile alert magit-popup async vimrc-mode dactyl-mode ws-butler web-mode use-package ujelly-theme tao-theme swift-mode persp-mode orgit org org-download neotree moe-theme jazz-theme info+ hide-comnt helm-projectile helm-flx grandshell-theme gitattributes-mode evil-surround evil-mc evil-matchit evil-escape company-sourcekit apropospriate-theme alect-themes autothemer company helm helm-core magit git-commit with-editor request hydra js2-mode zonokai-theme zenburn-theme zen-and-art-theme xkcd xcode-mode window-numbering which-key web-beautify volatile-highlights vi-tilde-fringe uuidgen underwater-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spotify spacemacs-theme spaceline spacegray-theme sourcekit soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode seti-theme scss-mode sass-mode reverse-theme reveal-in-osx-finder restart-emacs rainbow-delimiters railscasts-theme quelpa purple-haze-theme pug-mode professional-theme popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pcre2el pbcopy pastels-on-dark-theme paradox osx-trash osx-dictionary organic-green-theme org-wunderlist org-projectile org-present org-pomodoro org-plus-contrib org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme move-text monokai-theme monochrome-theme molokai-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow macrostep lush-theme lorem-ipsum livid-mode linum-relative link-hint light-soap-theme less-css-mode launchctl json-mode js2-refactor js-doc jbeans-theme jasminejs-mode ir-black-theme inkpot-theme indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-spotify helm-mode-manager helm-make helm-gitignore helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme gotham-theme google-translate golden-ratio gnuplot gitconfig-mode git-timemachine git-messenger git-link gh-md gandalf-theme flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-args evil-anzu eval-sexp-fu espresso-theme emoji-cheat-sheet-plus emmet-mode elisp-slime-nav dumb-jump dracula-theme django-theme diminish deft dash-at-point darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-web company-tern company-statistics company-emoji column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode clues-theme clean-aindent-mode cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme bind-key badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile anti-zenburn-theme ample-zen-theme ample-theme aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(powerline-default-separator "utf-8")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
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
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((((class color) (min-colors 65535)) (:background "#262B2C" :foreground "#eeeeec")) (((class color) (min-colors 256)) (:background "color-234" :foreground "color-255")) (((class color) (min-colors 88)) (:background "color-80" :foreground "color-87")) (t (:background "black" :foreground "white")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "#b58900"))))
 '(helm-selection ((t (:foreground "white" :background "red" :inverse-video nil))))
 '(mode-line ((t (:foreground "#e9e2cb" :background "#2075c7" :inverse-video nil))))
 '(mode-line-buffer-id ((t (:foreground "#e9e2cb" :background "#2075c7" :inverse-video nil))))
 '(mode-line-buffer-id-inactive ((t (:foreground "#2075c7" :background "#e9e2cb" :inverse-video nil))))
 '(mode-line-inactive ((t (:foreground "#2075c7" :background "#e9e2cb" :inverse-video nil))))
 '(powerline-active1 ((t (:foreground "#e9e2cb" :background "#2075c7" :inverse-video nil))))
 '(powerline-active2 ((t (:foreground "#e9e2cb" :background "#2075c7" :inverse-video nil))))
 '(powerline-inactive1 ((t (:foreground "#2075c7" :background "#e9e2cb" :inverse-video nil))))
 '(powerline-inactive2 ((t (:foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)))))
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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(diary-entry-marker 'font-lock-variable-name-face)
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
 '(evil-want-Y-yank-to-eol t)
 '(exec-path-from-shell-arguments '("-l"))
 '(fci-rule-color "#eee8d5")
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
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
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
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(org-structure-template-alist
   '(("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse")
     ("n" . "notes")))
 '(package-selected-packages
   '(writeroom-mode doom-modeline swiper ivy yapfify pytest pyenv-mode py-isort live-py-mode helm-pydoc cython-mode pip-requirements hy-mode anaconda-mode pythonic atomic-chrome magit-lfs pipenv csv-mode wsd-mode evil-commentary imenu-list company-anaconda rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby company-quickhelp pyvenv sql-indent ranger jira-markup-mode gmail-message-mode ham-mode html-to-markdown flymd edit-server yasnippet-snippets yaml-mode slack emojify circe oauth2 websocket pony-mode ox-twbs ox-reveal ox-gfm org-jira magit-gh-pulls helm-w3m w3m github-search github-clone github-browse-file gist gh marshal logito pcache ht editorconfig dockerfile-mode docker tablist docker-tramp request-deferred multi clojure-snippets clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider queue clojure-mode winum white-sand-theme powerline rebecca-theme org-category-capture org-mime lua-mode helm-spotify-plus exotica-theme ghub ag avy dash-functional tern iedit f goto-chg skewer-mode simple-httpd markdown-mode deferred haml-mode multiple-cursors dash s xterm-color shell-pop multi-term git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter flycheck-pos-tip pos-tip flycheck eshell-z eshell-prompt-extras esh-help diff-hl packed smartparens highlight evil yasnippet projectile alert magit-popup async vimrc-mode dactyl-mode ws-butler web-mode use-package ujelly-theme tao-theme swift-mode persp-mode orgit org org-download neotree moe-theme jazz-theme info+ hide-comnt helm-projectile helm-flx grandshell-theme gitattributes-mode evil-surround evil-mc evil-matchit evil-escape company-sourcekit apropospriate-theme alect-themes autothemer company helm helm-core magit git-commit with-editor request hydra js2-mode zonokai-theme zenburn-theme zen-and-art-theme xkcd xcode-mode window-numbering which-key web-beautify volatile-highlights vi-tilde-fringe uuidgen underwater-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spotify spacemacs-theme spaceline spacegray-theme sourcekit soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode seti-theme scss-mode sass-mode reverse-theme reveal-in-osx-finder restart-emacs rainbow-delimiters railscasts-theme quelpa purple-haze-theme pug-mode professional-theme popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pcre2el pbcopy pastels-on-dark-theme paradox osx-trash osx-dictionary organic-green-theme org-wunderlist org-projectile org-present org-pomodoro org-plus-contrib org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme move-text monokai-theme monochrome-theme molokai-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow macrostep lush-theme lorem-ipsum livid-mode linum-relative link-hint light-soap-theme less-css-mode launchctl json-mode js2-refactor js-doc jbeans-theme jasminejs-mode ir-black-theme inkpot-theme indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-spotify helm-mode-manager helm-make helm-gitignore helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme gotham-theme google-translate golden-ratio gnuplot gitconfig-mode git-timemachine git-messenger git-link gh-md gandalf-theme flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-args evil-anzu eval-sexp-fu espresso-theme emoji-cheat-sheet-plus emmet-mode elisp-slime-nav dumb-jump dracula-theme django-theme diminish deft dash-at-point darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-web company-tern company-statistics company-emoji column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode clues-theme clean-aindent-mode cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme bind-key badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile anti-zenburn-theme ample-zen-theme ample-theme aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))
 '(paradox-github-token t)
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(powerline-default-separator "utf-8")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
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
 '(cursor ((t (:background "#b58900"))))
 '(helm-selection ((t (:foreground "white" :background "red" :inverse-video nil))))
 '(mode-line ((t (:foreground "#e9e2cb" :background "#2075c7" :inverse-video nil))))
 '(mode-line-buffer-id ((t (:foreground "#e9e2cb" :background "#2075c7" :inverse-video nil))))
 '(mode-line-buffer-id-inactive ((t (:foreground "#2075c7" :background "#e9e2cb" :inverse-video nil))))
 '(mode-line-inactive ((t (:foreground "#2075c7" :background "#e9e2cb" :inverse-video nil))))
 '(powerline-active1 ((t (:foreground "#e9e2cb" :background "#2075c7" :inverse-video nil))))
 '(powerline-active2 ((t (:foreground "#e9e2cb" :background "#2075c7" :inverse-video nil))))
 '(powerline-inactive1 ((t (:foreground "#2075c7" :background "#e9e2cb" :inverse-video nil))))
 '(powerline-inactive2 ((t (:foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)))))
)
