(let ((backup-dir "~/.emacs.d/backups/")
      (my/custom-file "~/.emacs.d/custom.el"))
  (when (not (file-directory-p backup-dir))
    (make-directory backup-dir t))
  (when (not (file-exists-p my/custom-file))
    (write-region "" nil my/custom-file))
  (setq show-paren-delay 0
        inhibit-startup-screen t
        backward-delete-char-untabify-method "hungry"
        backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t
        backup-directory-alist `(("." . ,backup-dir))
        custom-file my/custom-file))
(setq-default indent-tabs-mode nil
              tab-stop-list nil
              tab-width 2)
(load custom-file)

(show-paren-mode)
(global-hl-line-mode)
(global-linum-mode)
(electric-pair-mode)
(add-to-list 'electric-pair-pairs '(?\' . ?\'))
(add-to-list 'electric-pair-pairs '(?` . ?`))
(add-to-list 'electric-pair-text-pairs '(?\' . ?\'))
(add-to-list 'electric-pair-text-pairs '(?` . ?`))
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-face-font 'default "Fira Mono-14")

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
  (setq-default evil-shift-width tab-width)
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-insert-state-map (kbd "<tab>") 'tab-to-tab-stop)
  (evil-set-register ?n [?$ ?%])
  (evil-set-register ?v [?$ ?v ?%])

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "c" 'evil-delete-buffer
      "e" 'eval-buffer
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
    (add-hook 'rjsx-mode-hook 'tern-mode)))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
  (evil-leader/set-key "a" 'projectile-ag)
  (add-hook 'kill-emacs-hook 'projectile-save-known-projects))

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
  (define-key evil-normal-state-map (kbd "S-C-p") 'helm-M-x)
  (evil-leader/set-key "b" 'helm-buffers-list)

  (use-package helm-projectile
    :init
    (setq helm-projectile-fuzzy-match t
          projectile-completion-system 'helm)
    :ensure t
    :config
    (helm-projectile-on)))

(use-package rjsx-mode
  :init
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . rjsx-mode)))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(defun my/use-eslint-from-node-modules ()
  "Use local eslint if possible."
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
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (add-hook 'flycheck-mode-hook 'my/use-eslint-from-node-modules)
  (add-hook 'rjsx-mode-hook 'flycheck-mode)

  (use-package flycheck-pos-tip
    :ensure t
    :config
    (flycheck-pos-tip-mode)))

(use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-global-mode))

(use-package undo-tree
  :init
  (let ((undo-dir "~/.emacs.d/undo/"))
    (when (not (file-directory-p undo-dir))
      (make-directory undo-dir t))
    (setq undo-tree-auto-save-history t
          undo-tree-history-directory-alist `(("." . ,undo-dir))))
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-global-mode)
  (define-key evil-normal-state-map (kbd "M-k") 'drag-stuff-up)
  (define-key evil-normal-state-map (kbd "M-j") 'drag-stuff-down))

(defun my/neotree-project-dir ()
  "Open NeoTree using the project root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(use-package neotree
  :ensure t
  :config
  (evil-leader/set-key "n" 'my/neotree-project-dir)
  (evil-define-key 'normal neotree-mode-map (kbd "<tab>") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "<return>") 'neotree-enter)

  (use-package all-the-icons
    :ensure t
    :config
    (setq neo-theme 'icons)))

(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))
