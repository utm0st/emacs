;; basic cfg
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
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))
(global-set-key (kbd "C-h") 'delete-backward-char)
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
(electric-pair-mode t)
(delete-selection-mode t)
;; (setq tree-sitter nil)
;; (setq treesit-language-source-alist
;;    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;      (cmake "https://github.com/uyha/tree-sitter-cmake")
;;      (css "https://github.com/tree-sitter/tree-sitter-css")
;;      (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;      (go "https://github.com/tree-sitter/tree-sitter-go")
;;      (html "https://github.com/tree-sitter/tree-sitter-html")
;;      (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;      (json "https://github.com/tree-sitter/tree-sitter-json")
;;      (make "https://github.com/alemuller/tree-sitter-make")
;;      (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;      (python "https://github.com/tree-sitter/tree-sitter-python")
;;      (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;      (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;      (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;      (yaml "https://github.com/ikatyang/tree-sitter-yaml")
;;      (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;      (c "https://github.com/tree-sitter/tree-sitter-c")))
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
(setq major-mode-remap-alist
      '((c++-mode . c++-ts-mode)
	(c-mode . c-ts-mode)
	(json-mode . json-ts-mode)
	(bash-mode . bash-ts-mode)))
