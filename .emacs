(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'evil-surround)
(require 'ag)
(require 'dtrt-indent)
(require 'helm-config)
(require 'linum-relative)
(require 'ido)
(require 'git-gutter-fringe)

(require 'projectile)
(projectile-global-mode)
(setq projectile-require-project-root nil)

(require 'evil-leader)
(global-evil-leader-mode)

(require 'auto-complete-config)
(ac-config-default)

(require 'powerline)
(powerline-default-theme)

(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

(require 'evil)
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "<tab>") 'switch-to-next-buffer)
(define-key evil-normal-state-map (kbd "<backtab>") 'switch-to-prev-buffer)
(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "x" 'helm-M-x
  "b" 'helm-buffers-list
  "c" 'kill-this-buffer
  "e" 'eval-buffer
  "a" 'helm-do-ag)

(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq scroll-step 1)
(setq-default tab-width 2 indent-tabs-mode nil)
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(blink-cursor-mode nil)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("3b0a350918ee819dca209cec62d867678d7dac74f6195f5e3799aa206358a983" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(electric-pair-mode t)
 '(evil-jumper-mode t)
 '(evil-mode t)
 '(fci-rule-color "#eee8d5")
 '(global-evil-surround-mode t)
 '(global-evil-visualstar-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(helm-M-x-fuzzy-match t)
 '(helm-autoresize-mode t)
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-mode-fuzzy-match t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(ido-mode t nil (ido))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(linum-relative-current-symbol "")
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "Monaco")))))
