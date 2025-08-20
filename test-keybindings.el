;;; test-keybindings.el --- Test GTD keybinding setup -*- lexical-binding: t; -*-

;; Test script for verifying GTD keybinding configuration

(defun test-gtd-keybindings ()
  "Test GTD keybinding setup and check for conflicts."
  (interactive)
  (let ((results '())
        (conflicts '())
        (missing '()))
    
    ;; Test 1: Check if keybindings are set up
    (push (format "=== GTD Keybinding Test Results ===\n") results)
    
    ;; Test main prefix
    (if (lookup-key spacemacs-default-map (kbd "SPC o o"))
        (push "✓ Main prefix 'SPC o o' is defined\n" results)
      (push "✗ Main prefix 'SPC o o' is NOT defined\n" results))
    
    ;; Test all keybinding groups
    (let ((keybinding-groups
           '(("o o c" . "capture")
             ("o o p" . "process") 
             ("o o n" . "navigate")
             ("o o r" . "review")
             ("o o a" . "agenda")
             ("o o z" . "zettelkasten")
             ("o o d" . "development"))))
      
      (push "\n--- Keybinding Groups ---\n" results)
      (dolist (group keybinding-groups)
        (let* ((key (car group))
               (desc (cdr group))
               (binding (lookup-key spacemacs-default-map (kbd (concat "SPC " key)))))
          (if binding
              (push (format "✓ %s (%s) is defined\n" key desc) results)
            (progn
              (push (format "✗ %s (%s) is NOT defined\n" key desc) results)
              (push key missing))))))
    
    ;; Test specific bindings
    (let ((specific-bindings
           '(("o o c i" . "capture inbox")
             ("o o n i" . "open inbox")
             ("o o n p" . "open projects")
             ("o o r w" . "weekly review")
             ("o o a a" . "org-agenda")
             ("o o z n" . "find note")
             ("o o s" . "save all")
             ("o o d v" . "validate structure"))))
      
      (push "\n--- Specific Bindings ---\n" results)
      (dolist (binding specific-bindings)
        (let* ((key (car binding))
               (desc (cdr binding))
               (full-key (concat "SPC " key))
               (command (lookup-key spacemacs-default-map (kbd full-key))))
          (if command
              (push (format "✓ %s (%s): %s\n" key desc command) results)
            (progn
              (push (format "✗ %s (%s) is NOT bound\n" key desc) results)
              (push key missing))))))
    
    ;; Check for conflicts with existing Spacemacs bindings
    (push "\n--- Checking for Conflicts ---\n" results)
    (let ((gtd-prefixes '("o o")))
      (dolist (prefix gtd-prefixes)
        (let ((full-prefix (concat "SPC " prefix))
              (existing (lookup-key spacemacs-default-map (kbd (concat "SPC " prefix)))))
          (when (and existing 
                     (not (keymapp existing))
                     (not (string-match "GTD" (format "%s" existing))))
            (push (format "⚠ Conflict: %s is already bound to %s\n" prefix existing) results)
            (push prefix conflicts)))))
    
    ;; Summary
    (push "\n=== Summary ===\n" results)
    (push (format "Missing bindings: %d\n" (length missing)) results)
    (push (format "Conflicts found: %d\n" (length conflicts)) results)
    
    ;; Display results
    (with-output-to-temp-buffer "*GTD Keybinding Test*"
      (dolist (result (reverse results))
        (princ result)))
    
    ;; Return status
    (list :missing missing :conflicts conflicts)))

(defun list-gtd-keybindings ()
  "List all GTD keybindings in a readable format."
  (interactive)
  (with-output-to-temp-buffer "*GTD Keybinding Reference*"
    (princ "GTD-Zettelkasten Keybinding Reference\n")
    (princ "=====================================\n\n")
    
    (princ "All commands are under: SPC o o\n\n")
    
    (princ "Capture (SPC o o c)\n")
    (princ "-------------------\n")
    (princ "  c i - Capture to inbox\n")
    (princ "  c c - Generic capture\n\n")
    
    (princ "Navigate (SPC o o n)\n")
    (princ "--------------------\n")
    (princ "  n i - Open inbox\n")
    (princ "  n p - Open projects\n")
    (princ "  n n - Open next actions\n")
    (princ "  n w - Open waiting for\n")
    (princ "  n s - Open someday/maybe\n")
    (princ "  n c - Open calendar\n")
    (princ "  n m - Open media\n\n")
    
    (princ "Process (SPC o o p)\n")
    (princ "-------------------\n")
    (princ "  p i - Process inbox\n\n")
    
    (princ "Review (SPC o o r)\n")
    (princ "------------------\n")
    (princ "  r d - Daily review\n")
    (princ "  r w - Weekly review\n\n")
    
    (princ "Agenda (SPC o o a)\n")
    (princ "------------------\n")
    (princ "  a a - Standard org-agenda\n")
    (princ "  a g - GTD agenda view (coming soon)\n\n")
    
    (princ "Zettelkasten (SPC o o z)\n")
    (princ "------------------------\n")
    (princ "  z n - Find/create note\n")
    (princ "  z i - Insert note link\n")
    (princ "  z c - Capture note\n")
    (princ "  z d - Daily note\n")
    (princ "  z b - Toggle backlinks buffer\n")
    (princ "  z p - Capture permanent note\n")
    (princ "  z l - Capture literature note\n")
    (princ "  z r - Capture reference\n")
    (princ "  z j - Capture project note\n")
    (princ "  z f p - Find permanent notes\n")
    (princ "  z f l - Find literature notes\n")
    (princ "  z f j - Find notes by project\n\n")
    
    (princ "Utilities\n")
    (princ "---------\n")
    (princ "  s   - Save all org buffers\n\n")
    
    (princ "Development (SPC o o d)\n")
    (princ "-----------------------\n")
    (princ "  d r - Reload GTD system\n")
    (princ "  d b - Benchmark capture\n")
    (princ "  d i - Initialize GTD\n")
    (princ "  d v - Validate structure\n")))

;; Run the test
(test-gtd-keybindings)

(provide 'test-keybindings)
;;; test-keybindings.el ends here