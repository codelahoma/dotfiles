;;; init-preload.el --- Preload configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file prevents issues with native compilation and built-in org loading.

;;; Code:

;; Prevent native compilation of org-element and related files
;; This is the key fix for the org version loading issue
(setq native-comp-jit-compilation-deny-list '(".*org-element.*" ".*org-macs.*" ".*org-compat.*"))

;; Prevent any org-related autoloads from triggering
(setq org-inhibit-startup t)
(setq org-modules nil)

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

(provide 'init-preload)
;;; init-preload.el ends here