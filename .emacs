;;;
;; Für Menschen wie mich kommen Chancen nur einmal im Blauen Mond vorbei.
;; Ich bin der Abfall der Gesellschaft und wenn ich mein Leben nicht für jede
;; Chance rischiere, die ich bekomme, werde ich zu nichts.
;; -------------------------------------
;; Daher ist Mut meine einzige Hoffnung.
;;
;;;

(setq initial-scratch-message ";; Ihre einzige Hoffnung beginnt hier.\n")

(require 'package)

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

(setq inhibit-startup-message t)
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

(setq-default
 c-file-style nil
 coffee-tab-width 2
 css-indent-offset 2
 fill-column 72
 indent-tabs-mode nil
 save-place t
 tab-width 2
 truncate-lines t)

(c-add-style "my-cpp-style" '((c-basic-offset . 2)
                              (c-offsets-alist (access-label . 0)
                                               (label . +)
                                               (innamespace . 0)
                                               )))

(add-hook 'c++-mode-hook 'my-c++-style)

(dolist (pkg '(lsp-mode company helm-lsp flycheck lsp-ui lsp-treemacs))
  (unless (package-installed-p pkg)
    (package-install pkg)))

(defun my-delete-trailing-whitespace ()
  "Delete trailing whitespace if in a programming mode."
  (when (derived-mode-p 'prog-mode)  ; Checks if the current mode is derived from prog-mode
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'my-delete-trailing-whitespace)

(setq mouse-wheel-progressive-speed nil) ; set to t for progressive speed
(setq mouse-wheel-follow-mouse 't) ; scroll window under mouse pointer

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

(setq read-process-output-max (* 1024 1024))
(setq lsp-idle-delay 0.500)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(ef-duo-dark))
 '(custom-safe-themes
   '("263e3a9286c7ab0c4f57f5d537033c8a5943e69d142e747723181ab9b12a5855" "546862540e7b7d758a64b328bf3ceec7ae98dd87d80551496b45485ec26e05e5" "063095cf0fe6ed3990546ec77e5d3798a1e2ad5043350063467a71c69518bb24" "515ebca406da3e759f073bf2e4c8a88f8e8979ad0fdaba65ebde2edafc3f928c" "c42587b19ee1c9aa1a9dd1d8ace37ece24ca2a322243035cd6ba07f44fb466db" "6b839977baf10a65d9d7aed6076712aa2c97145f45abfa3bad1de9d85bc62a0e" default))
 '(package-selected-packages
   '(ef-themes powerline which-key hl-todo cmake-mode helm glsl-mode company move-text ivy projectile auto-package-update))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight medium :height 113 :width normal)))))
