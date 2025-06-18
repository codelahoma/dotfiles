;;; init-preload.el --- Preload configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file should be loaded at the very beginning of init.el
;; to prevent built-in org from loading.

;;; Code:

;; Prevent any org-related autoloads from triggering
(setq org-inhibit-startup t)
(setq org-modules nil)

;; Disable Spacemacs org layer until we're ready
(setq-default dotspacemacs-configuration-layers
              (remove 'org dotspacemacs-configuration-layers))

;; Remove ALL org-related paths from load-path
(setq load-path 
      (cl-remove-if (lambda (path)
                      (when (stringp path)
                        (or (string-match-p "lisp/org/?\\'" path)
                            (string-match-p "/share/emacs/.*/lisp/org" path)
                            (string-match-p "/Cellar/.*/share/emacs/.*/lisp/org" path)
                            (string-match-p "/org-mode/" path))))
                    load-path))

;; Prevent autoloading of built-in org
(with-eval-after-load 'autoload
  (setq generated-autoload-file nil))

;; Unload org if it somehow got loaded
(when (featurep 'org)
  (message "WARNING: Built-in org was loaded, attempting to unload...")
  (mapc (lambda (feat)
          (when (featurep feat)
            (unload-feature feat t)))
        '(org org-compat org-macs org-keys org-element org-list 
          org-table org-entities org-faces org-indent)))

;; Define our compatibility shims early
(eval-and-compile
  ;; org-element-with-disabled-cache
  (unless (fboundp 'org-element-with-disabled-cache)
    (defmacro org-element-with-disabled-cache (&rest body)
      "Execute BODY with org-element cache disabled (compatibility shim)."
      `(let ((org-element-use-cache nil))
         ,@body)))
  
  ;; Disable caching
  (setq org-element-use-cache nil))

;; Prevent org-mode from being set through auto-mode-alist
(setq auto-mode-alist 
      (cl-remove-if (lambda (pair) 
                      (and (consp pair)
                           (eq (cdr pair) 'org-mode)))
                    auto-mode-alist))

;; Add advice to prevent built-in org from loading
(defun init-preload--prevent-builtin-org (orig-fun &rest args)
  "Prevent loading of built-in org packages."
  (let ((file (car args)))
    (if (and (stringp file)
             (or (string-match-p "/share/emacs/.*/lisp/org" file)
                 (string-match-p "/Cellar/.*/share/emacs/.*/lisp/org" file)))
        (progn
          (message "Prevented loading of built-in org: %s" file)
          nil)
      (apply orig-fun args))))

(advice-add 'load :around #'init-preload--prevent-builtin-org)
(advice-add 'require :around #'init-preload--prevent-builtin-org)

(provide 'init-preload)
;;; init-preload.el ends here