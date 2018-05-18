(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dtrt-indent powerline helm-projectile company-tern web-mode helm company dracula-theme projectile evil-surround evil-leader evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq show-paren-delay 0)
(setq backward-delete-char-untabify-method "hungry")
(setq-default indent-tabs-mode nil
              tab-stop-list nil
              tab-width 2)

(show-paren-mode)
(global-hl-line-mode)
(global-linum-mode)
(electric-pair-mode)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "Fira Mono-14"))
(set-face-attribute 'default t :font "Fira Mono-14")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t
        evil-cross-lines t)
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)
  (define-key evil-normal-state-map (kbd "S-C-p") 'helm-M-x)
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-insert-state-map (kbd "<tab>") 'tab-to-tab-stop)
  (evil-set-register ?n [?$ ?%])
  (evil-set-register ?v [?v ?$ ?%])

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "c" 'evil-delete-buffer
      "b" 'helm-buffers-list
      "e" 'eval-buffer
      "a" 'projectile-ag
      "v" (lambda () (interactive) (find-file "~/.emacs"))
      "p" (lambda () (interactive) (evil-paste-after 1 ?+))
      "P" (lambda () (interactive) (evil-paste-before 1 ?+))
      "y" (lambda () (interactive) (evil-execute-macro 1 [?\" ?+ ?y]))
      "Y" (lambda () (interactive) (evil-execute-macro 1 [?\" ?+ ?y ?$]))))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode)))

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

(use-package company
  :init
  (setq company-idle-delay 0
        company-dabbrev-downcase nil
        company-tooltip-align-annotations t)
  :ensure t
  :config
  (add-to-list 'company-frontends 'company-tng-frontend)
  (global-company-mode)
  (define-key company-active-map (kbd "<tab>") 'company-select-next)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "<return>") (lambda ()
                                                    (interactive)
                                                    (if company-selection-changed
                                                        (company-complete-selection)
                                                      (company-cancel))
                                                    (newline-and-indent)))

  (use-package company-tern
    :ensure t
    :config
    (add-to-list 'company-backends 'company-tern)
    (add-hook 'web-mode-hook 'tern-mode)))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

(use-package helm
  :init
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t)
  :ensure t
  :config
  (require 'helm-config)
  (helm-mode 1)

  (use-package helm-projectile
    :init
    (setq helm-projectile-fuzzy-match t)
    :ensure t
    :config
    (helm-projectile-on)))

(use-package web-mode
  :init
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode)))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js" root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(use-package flycheck
  :ensure t
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-hook 'flycheck-mode-hook 'my/use-eslint-from-node-modules)
  (add-hook 'web-mode-hook 'flycheck-mode))

(use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-global-mode))

(use-package undo-tree
  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs/undo")))
  :ensure t
  :config
  (global-undo-tree-mode))
