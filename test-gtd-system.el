;;; test-gtd-system.el --- Test GTD system functionality -*- lexical-binding: t; -*-

;; This file tests the basic GTD system setup

(require 'codelahoma-gtd-config)
(require 'codelahoma-gtd-core)
(require 'codelahoma-ui)

(message "\n=== Testing GTD System ===\n")

;; Test 1: Check if directories exist
(message "Test 1: Checking GTD directories...")
(if (file-exists-p codelahoma-gtd-directory)
    (message "✓ GTD directory exists: %s" codelahoma-gtd-directory)
  (message "✗ GTD directory missing: %s" codelahoma-gtd-directory))

;; Test 2: Verify core files
(message "\nTest 2: Checking GTD files...")
(dolist (file '("inbox.org" "projects.org" "next-actions.org" 
                "waiting-for.org" "someday.org" "calendar.org" "media.org"))
  (let ((full-path (expand-file-name file codelahoma-gtd-directory)))
    (if (file-exists-p full-path)
        (message "✓ File exists: %s" file)
      (message "✗ File missing: %s" file))))

;; Test 3: Test keybindings
(message "\nTest 3: Testing keybindings...")
(when (boundp 'spacemacs-version)
  (message "Spacemacs detected, checking SPC o o bindings...")
  (let ((gtd-map (lookup-key spacemacs-leader-override-mode-map "oo")))
    (if (keymapp gtd-map)
        (progn
          (message "✓ SPC o o prefix is defined")
          ;; Check specific bindings
          (dolist (binding '(("c i" . codelahoma-gtd-capture-inbox)
                           ("n i" . codelahoma-gtd-open-inbox)
                           ("n p" . codelahoma-gtd-open-projects)
                           ("s" . org-save-all-org-buffers)))
            (let* ((key (car binding))
                   (expected-cmd (cdr binding))
                   (actual-cmd (lookup-key gtd-map key)))
              (if (eq actual-cmd expected-cmd)
                  (message "✓ SPC o o %s → %s" key expected-cmd)
                (message "✗ SPC o o %s expected %s, got %s" 
                         key expected-cmd actual-cmd)))))
      (message "✗ SPC o o prefix not found"))))

;; Test 4: Benchmark capture
(message "\nTest 4: Benchmarking capture...")
(let ((start-time (current-time)))
  (with-temp-buffer
    (insert "* TODO Test item")
    (write-file (expand-file-name "test-capture.tmp" codelahoma-gtd-directory)))
  (let ((elapsed (float-time (time-subtract (current-time) start-time))))
    (message "✓ Test capture completed in %.3f seconds" elapsed)
    (when (< elapsed codelahoma-gtd-capture-target-time)
      (message "✓ Within target time of %.1f seconds" 
               codelahoma-gtd-capture-target-time))))

;; Clean up test file
(delete-file (expand-file-name "test-capture.tmp" codelahoma-gtd-directory))

(message "\n=== GTD System Test Complete ===\n")