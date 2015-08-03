(powerline-default-theme)

(setq ac-modes '(javascript-mode coffee-mode html-mode emacs-lisp-mode))

(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq flx-ido-use-faces nil)

(setq projectile-require-project-root nil)

(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq scroll-step 1)
(setq-default tab-width 2 indent-tabs-mode nil)
(setq make-backup-files nil)
(setq frame-title-format "%b")
(defadvice helm-display-mode-line (after undisplay-header activate)
  (setq header-line-format nil))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
