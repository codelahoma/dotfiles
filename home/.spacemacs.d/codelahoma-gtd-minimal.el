;;; codelahoma-gtd-minimal.el --- Minimal safe GTD loader -*- lexical-binding: t; -*-

;; Minimal loader that doesn't cause hangs

(defun codelahoma-gtd-minimal-load ()
  "Load only the essential GTD files without initialization."
  (interactive)
  (message "Loading minimal GTD...")
  (let ((gtd-dir "~/.spacemacs.d/codelahoma-gtd/"))
    ;; Load core files in safe order
    (when (file-exists-p (concat gtd-dir "codelahoma-gtd-config.el"))
      (load-file (concat gtd-dir "codelahoma-gtd-config.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-gtd-core.el"))
      (load-file (concat gtd-dir "codelahoma-gtd-core.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-gtd-capture.el"))
      (load-file (concat gtd-dir "codelahoma-gtd-capture.el")))
    (when (file-exists-p (concat gtd-dir "codelahoma-gtd-process.el"))
      (load-file (concat gtd-dir "codelahoma-gtd-process.el"))))
  
  ;; Set up GTD keybindings
  (spacemacs/declare-prefix "o o c" "capture")
  (spacemacs/declare-prefix "o o n" "navigate")
  (spacemacs/declare-prefix "o o p" "process")
  
  (spacemacs/set-leader-keys
    ;; Help
    "o o ?" 'codelahoma-gtd-minimal-help
    ;; Capture
    "o o c i" 'codelahoma-gtd-capture-inbox
    "o o c t" 'codelahoma-gtd-capture-task
    ;; Navigate
    "o o n i" 'codelahoma-gtd-open-inbox
    "o o n p" 'codelahoma-gtd-open-projects
    ;; Process
    "o o p i" 'codelahoma-gtd-process-inbox
    ;; Test
    "o o m" 'codelahoma-gtd-minimal-test)
  
  (message "GTD system loaded! Press SPC o o ? for help"))

(defun codelahoma-gtd-minimal-test ()
  "Test function that doesn't trigger org-mode."
  (interactive)
  (message "GTD minimal test successful! System is responsive."))

(defun codelahoma-gtd-minimal-help ()
  "Show GTD system help."
  (interactive)
  (message "GTD System Commands:
  SPC o o c i - Capture to inbox
  SPC o o n i - Navigate to inbox  
  SPC o o p i - Process inbox
  SPC o o l   - Load/reload GTD system"))

;; Don't auto-load anything!
;; User must explicitly call codelahoma-gtd-minimal-load

(provide 'codelahoma-gtd-minimal)
;;; codelahoma-gtd-minimal.el ends here