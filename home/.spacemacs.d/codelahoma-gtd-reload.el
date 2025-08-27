;;; codelahoma-gtd-reload.el --- Manual reload for GTD system -*- lexical-binding: t; -*-

;; Simple reload function that can be called manually

(defun codelahoma-gtd-manual-reload ()
  "Manually reload and initialize the GTD system."
  (interactive)
  ;; Load the loader
  (load-file "~/.spacemacs.d/codelahoma-gtd-loader.el")
  ;; Initialize
  (codelahoma-gtd-init)
  ;; Force keybindings
  (spacemacs/declare-prefix "o" "user-defined")
  (spacemacs/declare-prefix "o o" "GTD/Zettelkasten")
  (spacemacs/set-leader-keys
    "o o ?" 'codelahoma-gtd-help
    "o o c i" 'codelahoma-gtd-capture-inbox
    "o o n i" 'codelahoma-gtd-open-inbox
    "o o p i" 'codelahoma-gtd-process-inbox
    "o o r w" 'codelahoma-gtd-weekly-review
    "o o w" 'org-save-all-org-buffers)
  (message "GTD system reloaded - try SPC o o ?"))

;; Bind to a temporary key for easy access
(global-set-key (kbd "C-c g r") 'codelahoma-gtd-manual-reload)

(provide 'codelahoma-gtd-reload)
;;; codelahoma-gtd-reload.el ends here