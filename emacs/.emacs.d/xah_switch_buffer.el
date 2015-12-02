(defvar xah-switch-buffer-ignore-dired t "If t, ignore dired buffer when calling `xah-next-user-buffer' or `xah-previous-user-buffer'")
(setq xah-switch-buffer-ignore-dired t)

(defun xah-next-user-buffer ()
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (or
           (string-equal "*" (substring (buffer-name) 0 1))
           (string-equal "TAGS" (buffer-name))
           (if (string-equal major-mode "dired-mode")
               xah-switch-buffer-ignore-dired
             nil
             ))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (or
           (string-equal "*" (substring (buffer-name) 0 1))
           (string-equal "TAGS" (buffer-name))
           (if (string-equal major-mode "dired-mode")
               xah-switch-buffer-ignore-dired
             nil
             ))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))
