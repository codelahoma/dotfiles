;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is loaded before init.el and before any packages are loaded.
;; Use it for critical early setup.

;;; Code:

;; Fix for org-element-with-disabled-cache error
(eval-and-compile
  (unless (fboundp 'org-element-with-disabled-cache)
    (defmacro org-element-with-disabled-cache (&rest body)
      "Execute BODY with org-element cache disabled (compatibility shim)."
      `(let ((org-element-use-cache nil))
         ,@body))))

;; Disable org-element caching early
(setq org-element-use-cache nil)

(provide 'early-init)
;;; early-init.el ends here