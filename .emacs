(load "~/.emacs.d/packages")
(load "~/.emacs.d/requires")
(load "~/.emacs.d/modes")
(load "~/.emacs.d/evil")
(load "~/.emacs.d/misc")
(load "~/.emacs.d/hooks")
(load "~/.emacs.d/xah_switch_buffer")
(load "~/.emacs.d/scalable_fonts")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu t)
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(flycheck-coffeelintrc "~/.coffeelint.json")
 '(helm-M-x-fuzzy-match t)
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-mode-fuzzy-match t)
 '(ido-completion-buffer nil)
 '(linum-relative-current-symbol "")
 '(menu-bar-mode nil)
 '(powerline-display-buffer-size nil)
 '(powerline-display-mule-info nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "unknown" :family "Monaco"))))
 '(helm-M-x-key ((t (:foreground "#FD971F" :underline nil))))
 '(helm-action ((t (:underline nil))))
 '(helm-etags-file ((t (:foreground "Lightgoldenrod4" :underline nil))))
 '(helm-grep-file ((t (:foreground "#A1EFE4" :underline nil))))
 '(helm-selection ((t (:background "dim gray" :underline nil)))))

