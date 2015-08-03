(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "<tab>") 'xah-next-user-buffer)
(define-key evil-normal-state-map (kbd "<backtab>") 'xah-previous-user-buffer)
(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file-dwim)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "x" 'helm-M-x
  "b" 'helm-buffers-list
  "c" 'kill-this-buffer
  "e" 'eval-buffer
  "a" 'helm-do-ag
  "f" 'helm-flycheck
  "u" 'undo-tree-visualize
  "p" 'package-list-packages)
