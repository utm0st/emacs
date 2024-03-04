;; Initialize package sources
(require 'package)
(require 'org)

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

;; Bindings
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key (kbd "C-'") 'dabbrev-expand)
(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "<f8>") 'compile)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c 2") 'duplicate-line)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c C") 'uncomment-region)

;; Packages
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

(use-package move-text
  :init
  :bind
  (("M-p" . 'move-text-up)
   ("M-n" . 'move-text-down))
  )

(use-package company
  :init)

(use-package glsl-mode
  :init
  :mode (("\\.vs\\'" . glsl-mode)
         ("\\.fs\\'" . glsl-mode)))


(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ef-themes
  :init)

;; Options
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
(setq visible-bell t)
(column-number-mode)
(global-display-line-numbers-mode t)
(global-subword-mode t)
(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(ido-mode t)
(electric-pair-mode t)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(setq make-backup-files nil)
(setq-default fill-column 120)
(delete-selection-mode t)

;; Hide asterisks for bold words and stuff.
(with-eval-after-load 'org
  (setq org-highlight-latex-and-related '(latex script entities))
  (setq org-hide-emphasis-markers t))

(add-hook 'org-mode-hook 'org-indent-mode)

(add-hook 'org-mode-hook 'turn-on-auto-fill)

(defvar efs/default-font-size 120)
(defvar efs/default-variable-font-size 120)

(set-face-attribute 'default nil :font "Comic Mono" :height efs/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Comic Mono" :height efs/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Comic Mono" :height efs/default-variable-font-size :weight 'regular)

(setq revert-without-query '("TAGS"))

(setq xref-show-xrefs-function #'xref-show-definitions-buffer)

(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4
 c-file-style nil)

(defun my-glsl-mode-hook ()
  "Custom hook for GLSL mode to set the tab width."
  (setq-default c-basic-offset 4)   ; Set the basic indentation offset for GLSL (which is based on C mode).
  (setq-default tab-width 4)        ; Set visual display of tabs to 4 spaces
  (setq-default indent-tabs-mode nil)) ; Use spaces instead of tabs

(add-hook 'glsl-mode-hook 'my-glsl-mode-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(ef-autumn))
 '(custom-safe-themes
   '("79a8c85692a05a0ce0502168bb0e00d25f021a75d8b0136b46978bddf25e3b72" "6f780ba22a933a57ee1b3aea989dc527c068a31d31513d2f1956955f2a697e6e" "3cf1845f34a1180b390a94e7cef30912f60c152c520e03d08868cc7b70aaa9c8" default))
 '(inhibit-startup-screen t)
 '(initial-scratch-message
   ";; There's no tomorrow. It's now or never. Be true to yourself or die.\12\12")
 '(package-selected-packages
   '(ef-themes vterm projectile org-bullets move-text ivy glsl-mode company auto-package-update))
 '(xref-auto-jump-to-first-definition t)
 '(xref-auto-jump-to-first-xref t)
 '(xref-prompt-for-identifier
   '(not xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame))
 '(xref-show-definitions-function 'xref-show-definitions-completing-read))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
