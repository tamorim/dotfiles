(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

; list the packages you want
(setq package-list '(ag auto-complete coffee-mode dtrt-indent emmet-mode evil evil-commentary evil-leader evil-numbers evil-paredit evil-surround evil-visualstar flx-ido flycheck git-gutter-fringe helm helm-ag helm-emmet helm-flycheck helm-projectile ido-vertical-mode jsx-mode linum-relative magit monokai-theme paredit powerline projectile rainbow-delimiters undo-tree delight))

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

