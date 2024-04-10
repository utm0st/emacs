;; Initialize package sources
(require 'package)
(require 'org)

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
  :mode (("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode)
         ("\\.tesc\\'" . glsl-mode)
         ("\\.tese\\'" . glsl-mode)
         ("\\.comp\\'" . glsl-mode)
         ))

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package cmake-mode
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

(setq pixel-scroll-precision-mode t)

(setq special-display-buffer-names
      '("*compilation*"))

(setq special-display-frame-alist
      '((name . "*compilation*")
        (width . 120)
        (height . 64)
        (left . 100)
        (top . 100)))

;; Define a custom face for highlighting
(defface my-hl-line-face
  '((t (:background "firebrick" :foreground nil))) ; Adjust the color as needed
  "Face for highlighting the current line.")

;; Set hl-line-face to use the custom face
(setq hl-line-face 'my-hl-line-face)

;; Enable hl-line-mode globally
(global-hl-line-mode 1)

(setq fixme-modes '(c++-mode c-ts-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
	    (font-lock-add-keywords
	     mode
	     '(
           ("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
           ("\\<\\(TEMP\\)" 1 'font-lock-fixme-face t)
           )))
	  fixme-modes)

(modify-face 'font-lock-fixme-face "red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "lime green" nil nil t nil t nil nil)

(global-auto-revert-mode t)

(blink-cursor-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes '(borland-blue))
 '(custom-safe-themes
   '("dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "7af2a6fcd1e743d165c58fd95d20b46c2d96d9873ab67fc9371bdc8fda463de7" "0c5d7ffa7cdf8b889342d2cd38f6ddd2cc716561048feca89b17fda56677d6b8" "9f27d5afd6d78b40bf1290c10722818e0b90f141fc3758d3c2d284ccb565de15" "296da7c17c698e963c79b985c6822db0b627f51474c161d82853d2cb1b90afb0" "f74a9377a5e4829fcef7a403a2f6497251b2ac7228e2bf630034a0d3f0065c8a" "95e934b092694a2624adb653043d1dc016a6712fa27b788f9ff4dffb8ee08290" "7776ba149258df15039b1f0aba4b180d95069b2589bc7d6570a833f05fdf7b6d" "702d0136433ca65a7aaf7cc8366bd75e983fe02f6e572233230a528f25516f7e" "5669dc3dfc150278fdcc2fb6d87f176484b17751c066ccc829a631ec281c4ee3" "84b04a13facae7bf10f6f1e7b8526a83ca7ada36913d1a2d14902e35de4d146f" "b3737f86b86d52c7d06820c10dc2609e9192627fc02dd654f4f9eb336f24f511" "f02f8c8efe52c25819eb4341bbadb483d366f637aea0e092e62629126db3545d" "d05d7ef5ef2c6d63f382e5e2fcdf2f4c7a9f6f6aad9433f04dbddecf71d46f1c" "3b5bac2bef0c51a169be7e9b5f80414e662e5eb2e3e3cf126873325e9344d26e" "b5367e48da33f76c5423869034124db4ec68ab712b2dd3b908afa3fe080f0da6" "9ed206ff6874db89cb4a588c6cdc75a7b056fecbc9880e9758881bdef6d9d79a" "6631f884f5f43e9d8eee42f5bcf8522a7f791688d2d2667ec135c129066be243" "2ca3da7d36b0d326f984530a07be54b272b5c313b1361989acf747d8b5616162" "79a8c85692a05a0ce0502168bb0e00d25f021a75d8b0136b46978bddf25e3b72" "6f780ba22a933a57ee1b3aea989dc527c068a31d31513d2f1956955f2a697e6e" "3cf1845f34a1180b390a94e7cef30912f60c152c520e03d08868cc7b70aaa9c8" default))
 '(global-display-line-numbers-mode t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message
   ";; For people like us, chances only come around once in a blue moon. We're the trash of society and if we don't risk our lives for every chance we get, we'll become nothing but sore losers.\12\12")
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(tao-theme borland-blue-theme bordland-theme bordland-blue raspopovic-theme cmake-mode ef-themes vterm projectile org-bullets move-text ivy glsl-mode company auto-package-update))
 '(tool-bar-mode nil)
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
 '(default ((t (:family "FreeMono" :foundry "GNU " :slant normal :weight bold :height 128 :width normal)))))
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(set-cursor-color "yellow")
