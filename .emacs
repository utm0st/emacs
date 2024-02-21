;; Initialize package sources
(require 'package)
(require 'org)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

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

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)

(global-set-key (kbd "C-c o") 'ff-find-other-file)

(global-subword-mode t)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package ido
  :init)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init)

(use-package ivy
  :init)

(use-package markdown-mode
  :init)

(use-package vterm
  :init)

(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)

; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 120)
(defvar efs/default-variable-font-size 120)

(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Fira Code" :height efs/default-variable-font-size :weight 'regular)

(use-package move-text
  :bind
  (("M-p" . 'move-text-up)
   ("M-n" . 'move-text-down))
  )

(ido-mode t)
(electric-pair-mode t)
(global-set-key (kbd "C-'") 'dabbrev-expand)
(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "<f8>") 'compile)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c 2") 'duplicate-line)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c C") 'uncomment-region)

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(use-package projectile)
(use-package flycheck)
(use-package yasnippet :config (yas-global-mode))
(use-package hydra)
(use-package company)
(use-package lsp-ui)
(use-package which-key :config (which-key-mode))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package helm-lsp)
(use-package helm
  :config (helm-mode))
(use-package lsp-treemacs)
(use-package cmake-mode)
(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil) ; Use lsp-ui and flycheck
  (setq lsp-clients-clangd-args '("--compile-commands-dir=output")) ; Specify the path to your compile_commands.json file if needed
  )
(use-package glsl-mode)

(use-package doom-themes
  :init)

(setq make-backup-files nil)

(setq treemacs-text-scale -2)

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq-default fill-column 120)

;; Hide asterisks for bold words and stuff.
(with-eval-after-load 'org
  (setq org-highlight-latex-and-related '(latex script entities))
  (setq org-hide-emphasis-markers t))

(add-hook 'org-mode-hook 'org-indent-mode)

(add-hook 'org-mode-hook 'turn-on-auto-fill)

(use-package ef-themes
  :ensure t
  :init)

(use-package flycheck-languagetool
  :ensure t
  :hook (text-mode . flycheck-languagetool-setup)
  :init
  (setq flycheck-languagetool-server-jar "/home/euclid/Source/LanguageTool-6.3/languagetool-server.jar"))

(defun my-c++-mode-hook ()
  (setq c-default-style "stroustrup"))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(ef-winter))
 '(custom-safe-themes
   '("9ed206ff6874db89cb4a588c6cdc75a7b056fecbc9880e9758881bdef6d9d79a"
     "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882"
     "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a"
     "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33"
     "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "ccdc42b444da0b62c25850da75f59186319ee22ddfd153ffc9f7eb4e59652fc9"
     "46586a9fe1e34359c55c43536aa362c15237a29dbbd69a225ddcbcce97301959" default))
 '(delete-selection-mode t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(ef-themes doom-themes flycheck-languagetool org-bullets glsl-mode cmake-mode green-is-the-new-black-theme nasm-mode
               yasnippet which-key vterm projectile move-text lsp-ui lsp-java ivy helm-lsp flycheck company
               auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
