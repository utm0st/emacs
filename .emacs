;; Font-size and stuff.
(defvar efs/default-font-size 140)
(defvar efs/default-variable-font-size 140)
(defvar efs/frame-transparency '(90 . 90))

(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-face-attribute 'default nil :font "Iosevka" :height efs/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height efs/default-font-size)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Iosevka" :height efs/default-variable-font-size :weight 'regular)

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Initialize package sources
(require 'package)

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

;; Update packages.
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; Completion framework.
(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
              ("TAB" . ivy-alt-done)
              ("C-l" . ivy-alt-done)
              ("C-n" . ivy-next-line)
              ("C-p" . ivy-previous-line)
              :map ivy-switch-buffer-map
              ("C-p" . ivy-previous-line)
              ("C-l" . ivy-done)
              ("C-d" . ivy-switch-buffer-kill)
              :map ivy-reverse-i-search-map
              ("C-p" . ivy-previous-line)
              ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package go-mode
  :init)

(use-package clang-format
  :init)

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package yasnippet
  :init)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l
  :hook (lsp-mode . efs/lsp-mode-setup)
  (lsp-enable-which-key-integration t))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/repos")
    (setq projectile-project-search-path '("~/repos")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package eglot
;;   :init
;;   :config
;;   (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")))

(use-package tree-sitter
  :init)

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package ef-themes
  :init)

(defun jart-sudo (&optional path)
  "Reopen PATH (or current file) with root privileges."
  (interactive)
  (find-alternate-file
   (concat "/sudo:root@localhost:" (or path buffer-file-name))))

(defun jart-unfill-paragraph ()
  "Take a multi-line paragraph and make it into a single line.

Thanks: Stefan Monnier <foo@acm.org>"
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun jart-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

(defun jart-isearch-show-all-matches ()
  "Shows grep results during a C-s search."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string
             (regexp-quote isearch-string)))))

(defun jart-paredit-close-parenthesis ()
  "Reliably insert closing parenthesis."
  (interactive)
  (let ((p (point)))
    (condition-case nil
        (paredit-close-parenthesis)
      ('error
       (insert ")")
       (jart-show-note "unbalanced")))
    (goto-char (+ p 1))))

(defun jart-note ()
  "Open a new note entry in my notes file."
  (interactive)
  (find-file "~/notes.org")
  (goto-char (point-min))
  (org-insert-heading)
  (insert (concat "<" (format-time-string "%Y-%m-%dT%H:%M:%S%z") "> ")))

(defun jart-sort-at-point ()
  "Sort lines under cursor."
  (interactive)
  (or (jart-sort-list-at-point)
      (jart-sort-block-at-point)))

(defun jart-sane-forward-paragraph ()
  "Move to next blank line."
  (interactive)
  (jart-normal-paragraphs
   (forward-paragraph)))

(defun jart-sane-backward-paragraph ()
  "Move to previous blank line."
  (interactive)
  (jart-normal-paragraphs
   (backward-paragraph)))

(defun jart-sane-} ()
  "Insert } with correct indentation."
  (interactive)
  (insert "}")
  (indent-for-tab-command))

(defvar dotfiles-dir
  (file-name-directory
   (or (buffer-file-name) load-file-name))
  "Location of Emacs configuration")

(defun jart-save-word ()
  "Adds word under cursor to personal dictionary."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (word (buffer-substring-no-properties (car bounds)
                                               (cdr bounds))))
    (ispell-send-string (concat "*" word "\n"))
    (add-to-list 'ispell-buffer-session-localwords word)
    (when (fboundp 'flyspell-unhighlight-at)
      (flyspell-unhighlight-at (car bounds)))
    (setq ispell-pdict-modified-p '(t))
    (ispell-pdict-save t t)))

(defun jart-pretty-lambdas ()
  "Make lambda render with the unicode symbol."
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun jart-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . lsp-deferred))

(use-package disaster
  :init)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-Q") 'jart-unfill-paragraph)
(global-set-key (kbd "C-x M-m") 'shell)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-r") 'replace-string)
(global-set-key (kbd "C-x C-l") 'replace-regexp)
(global-set-key (kbd "<f1>") 'man)
(global-set-key (kbd "<f3>") 'jart-sudo)
(global-set-key (kbd "<f5>") 'toggle-truncate-lines)
(global-set-key (kbd "<f6>") 'gud-next)
(global-set-key (kbd "C-<f6>") 'gud-nexti)
(global-set-key (kbd "<f7>") 'gud-step)
(global-set-key (kbd "C-<f7>") 'gud-stepi)
(global-set-key (kbd "<f8>") 'gud-finish)
(global-set-key (kbd "C-<f8>") 'gud-cont)
(global-set-key (kbd "<f9>") 'gud-up)
(global-set-key (kbd "C-<f9>") 'gud-down)
(global-set-key (kbd "<f10>") 'compile)
(global-set-key (kbd "C-<f10>") 'gdb)
(global-set-key (kbd "C-c n") 'jart-note)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-M-<tab>") 'clang-format-region)
(global-set-key (kbd "C-c <tab>") 'clang-format-buffer)
(global-set-key (kbd "C-c f") 'flyspell-buffer)
(global-set-key (kbd "C-c a") 'jart-save-word)
(global-set-key (kbd "C-c d") 'disaster)
(global-set-key (kbd "C-'") 'dabbrev-expand)
(global-set-key (kbd "C-c o") 'ff-find-other-file)
;; Move thru' windows.
(global-set-key (kbd "M-S-<left>") 'windmove-left)
(global-set-key (kbd "M-S-<right>") 'windmove-right)
(global-set-key (kbd "M-S-<up>") 'windmove-up)
(global-set-key (kbd "M-S-<down>") 'windmove-down)
;; Show lines of code.
(global-set-key (kbd "<f12>") 'global-display-line-numbers-mode)

;; Nasty trick to handle namespace indent.
(defconst my-namespaces
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "my-namespaces" my-namespaces)

;; Misc.
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t) ; Disable splash screen.
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar
(auto-compression-mode t)
(auto-fill-mode 1)
(delete-selection-mode 1)
(global-font-lock-mode t)
(ido-mode t)
(recentf-mode 1)
(show-paren-mode 1)
(global-subword-mode t)
(setq make-backup-files nil)


(setq-default
 c-basic-offset 2
 c-file-style nil
 fill-column 72
 indent-tabs-mode nil
 tab-width 2
 truncate-lines t)

;; UTF-8
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Performance Enhancements

;; Stop recursively searching parents when I open a file!
(setq vc-handled-backends nil)
(require 'files)
(defun dir-locals-find-file (file)
  "Do nothing with FILE."
  nil)

(require 'server)
(when (not (server-running-p))
  (server-start))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(column-number-mode t)
 '(custom-enabled-themes '(ef-autumn))
 '(custom-safe-themes
   '("ff1607d931035f2496e58566ee567b44a0f10be2f3f55d8e2956af16a2431d94" "6fad1050fbe7ee0b4bca65d33078d279a96f64c0df3286b36ce45fe4803847f2" "f1a116f53d9e685023ebf435c80a2fecf11a1ecc54bb0d540bda1f5e2ae0ae58" "c0e8a59eb7d603ca4af5616b2c61b8be2fee031760af4d8c80fd2f21229ce462" default))
 '(fill-column 72)
 '(gdb-enable-debug t)
 '(gdb-many-windows t)
 '(gdb-restore-window-configuration-after-quit t)
 '(global-display-line-numbers-mode t)
 '(lsp-enable-snippet nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(yasnippet ef-themes disaster go-mode clang-format company-box company magit counsel-projectile projectile hydra counsel ivy-rich which-key rainbow-delimiters doom-themes all-the-icons doom-modeline swiper auto-package-update ivy use-package))
 '(tab-width 2)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Comfy Fixed" :foundry "UKWN" :slant normal :weight normal :height 143 :width normal)))))
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
