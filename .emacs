(require 'package)

(server-start)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)

(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init)

(use-package ido
  :init)

(use-package ivy
  :init)

(use-package hl-todo
  :init)

(use-package move-text
  :init
  :bind
  (("M-p" . 'move-text-up)
   ("M-n" . 'move-text-down)))

(use-package company
  :init)

(use-package ef-themes
  :init)

(use-package which-key
  :init)

(use-package cmake-mode
  :init)

(use-package powerline
  :init)

(use-package glsl-mode
  :init
  :mode (("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode)
         ("\\.tesc\\'" . glsl-mode)
         ("\\.tese\\'" . glsl-mode)
         ("\\.comp\\'" . glsl-mode)))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "C-<tab>") 'projectile-find-other-file)
(global-set-key (kbd "C-'") 'dabbrev-expand)
(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "<f8>") 'compile)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c 2") 'duplicate-line)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c C") 'uncomment-region)
(global-set-key (kbd "C-c q") 'quick-calc)
(global-set-key (kbd "C-c l") 'count-words)
(global-set-key (kbd "C-c g") 'projectile-grep)
(global-set-key (kbd "<f8>") 'projectile-compile-project)
(global-set-key (kbd "C-c <TAB>") 'lsp-format-buffer)

(electric-pair-mode t)
(delete-selection-mode t)
(tool-bar-mode -1)
(global-hl-line-mode 0)
(global-subword-mode t)
(ido-mode t)
(setq-default cursor-type 'box)
(menu-bar-mode 0)
(global-hl-todo-mode)
(which-key-mode 1)

(powerline-default-theme)

(setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("XXX"    . "#FFA500")
        ("KLUDGE" . "#FFFF00")
	("NOTE"   . "#1cc23f")
	))

(with-eval-after-load 'hl-todo
  (define-key hl-todo-mode-map (kbd "C-x t p") 'hl-todo-previous)
  (define-key hl-todo-mode-map (kbd "C-x t n") 'hl-todo-next))

(defconst my-cc-style
  '("my-cc-mode"
    (c-offsets-alist . ((innamespace . [0]))) ; no extra indentation inside namespaces
    (c-basic-offset . 2))) ; set base indentation level to 2 spaces

(c-add-style "my-cc-mode" my-cc-style)

(defun my-c++-setup ()
  (c-set-style "my-cc-mode"))

(add-hook 'c++-mode-hook 'my-c++-setup)

(dolist (pkg '(lsp-mode company helm-lsp flycheck lsp-ui lsp-treemacs))
  (unless (package-installed-p pkg)
    (package-install pkg)))

(defun my-delete-trailing-whitespace ()
  "Delete trailing whitespace if in a programming mode."
  (when (derived-mode-p 'prog-mode)  ; Checks if the current mode is derived from prog-mode
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'my-delete-trailing-whitespace)

(setq mouse-wheel-progressive-speed nil) ; Set to t for progressive speed
(setq mouse-wheel-follow-mouse 't) ; Scroll window under mouse pointer

(require 'lsp-mode)

(use-package lsp-mode
  :ensure t
  :commands lsp-deferred
  :hook ((c++-mode . lsp-deferred)))

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(require 'flycheck)
(global-flycheck-mode)

(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-idle-delay 0.500)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
 '(package-selected-packages
   '(ef-themes powerline which-key hl-todo cmake-mode helm glsl-mode company move-text ivy projectile auto-package-update))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight medium :height 113 :width normal)))))
