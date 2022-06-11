;;; Code:

(defvar jekyll-packages
  '(
    hyde))

(defun jekyll/init-hyde ()
  (use-package hyde
    :init
    (progn
      (spacemacs/set-leader-keys "aj" 'spacemacs/hyde)
      (defun spacemacs/hyde ()
        "Helper function"
        (interactive)
        (hyde)))
    :config
    (progn
      (evilified-state-evilify hyde/hyde-mode hyde-mode-map
        (kbd "n") 'hyde/new-post
        (kbd "c") 'hyde/hyde-commit-post
        (kbd "g") 'hyde/load-posts
        (kbd "d") 'hyde/delete-post
        (kbd "p") 'hyde/promote-to-post
        (kbd "q") 'hyde/quit
        (kbd "s") 'hyde/serve
        (kbd "x") 'hyde/stop-serve
        (kbd "RET") 'hyde/open-post-maybe))))

;;; packages.el ends here
