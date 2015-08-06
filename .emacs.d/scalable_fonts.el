(defun fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
        (if (>= (x-display-pixel-width) 1920)
            (set-frame-parameter frame 'font "Monaco 16")
         (set-frame-parameter frame 'font "Monaco 12")))))

;; Fontify current frame
(fontify-frame nil)

;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)
