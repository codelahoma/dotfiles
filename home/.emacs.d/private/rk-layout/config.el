;; Frame geometry persistence for Emacs daemon mode
;;
;; Saves geometry when frames are deleted, applies to new frames.
;; Works with both daemon (emacsclient -c) and standalone Emacs.

(if (daemonp)
    ;; Daemon mode: hook into frame creation/deletion
    (progn
      (add-hook 'after-make-frame-functions #'rk-layout-apply-geometry)
      (add-hook 'delete-frame-functions #'rk-layout-save-framegeometry))
  ;; Standalone mode: use init/quit hooks (legacy behavior)
  (when window-system
    (add-hook 'after-init-hook (lambda () (rk-layout-apply-geometry (selected-frame))))
    (add-hook 'kill-emacs-hook #'rk-layout-save-framegeometry)))
