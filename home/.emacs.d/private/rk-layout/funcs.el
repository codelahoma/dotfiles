(defvar rk-layout-framegeometry-file
  (expand-file-name "~/.emacs.d/private/framegeometry")
  "File to store frame geometry.")

(defun rk-layout-save-framegeometry (&optional frame)
  "Save FRAME's geometry to `rk-layout-framegeometry-file'.
If FRAME is nil, use selected frame."
  (let* ((frame (or frame (selected-frame)))
         (framegeometry-left (frame-parameter frame 'left))
         (framegeometry-top (frame-parameter frame 'top))
         (framegeometry-width (frame-parameter frame 'width))
         (framegeometry-height (frame-parameter frame 'height)))

    ;; Ensure numeric values
    (when (not (number-or-marker-p framegeometry-left))
      (setq framegeometry-left 0))
    (when (not (number-or-marker-p framegeometry-top))
      (setq framegeometry-top 0))
    (when (not (number-or-marker-p framegeometry-width))
      (setq framegeometry-width 0))
    (when (not (number-or-marker-p framegeometry-height))
      (setq framegeometry-height 0))

    (with-temp-buffer
      (insert
       ";;; Frame geometry for emacsclient frames.\n"
       ";;; Last saved " (current-time-string) ".\n"
       "(setq rk-layout-saved-geometry\n"
       "      '(\n"
       (format "        (top . %d)\n" (max framegeometry-top 0))
       (format "        (left . %d)\n" (max framegeometry-left 0))
       (format "        (width . %d)\n" (max framegeometry-width 0))
       (format "        (height . %d)))\n" (max framegeometry-height 0)))
      (when (file-writable-p rk-layout-framegeometry-file)
        (write-file rk-layout-framegeometry-file)))))

(defun rk-layout-apply-geometry (frame)
  "Apply saved geometry to FRAME."
  (let ((framegeometry-file rk-layout-framegeometry-file))
    (when (file-readable-p framegeometry-file)
      (load-file framegeometry-file)
      (when (and (boundp 'rk-layout-saved-geometry)
                 rk-layout-saved-geometry
                 (display-graphic-p frame))
        (modify-frame-parameters frame rk-layout-saved-geometry)))))

;; Legacy aliases for compatibility
(defalias 'save-framegeometry 'rk-layout-save-framegeometry)
(defalias 'load-framegeometry 'rk-layout-apply-geometry)
