;; Move thru windows.
(global-set-key (kbd "M-S-<left>") 'windmove-left)
(global-set-key (kbd "M-S-<right>") 'windmove-right)
(global-set-key (kbd "M-S-<up>") 'windmove-up)
(global-set-key (kbd "M-S-<down>") 'windmove-down)

;; Show lines of code.
(global-set-key (kbd "<f12>") 'global-display-line-numbers-mode)

;; Compilation.
(global-set-key (kbd "<f8>") 'compile)

;; Ido-mode: completion in mini-buffer.
(ido-mode t)

;; Spelling.
(global-set-key (kbd "<f7>") 'ispell-region)

;; Flash current location of point.
(beacon-mode 1)

;; Startup emacs loading my config.
(find-file "~/.emacs")

;; Enable this to navigate by words that follow the camelCase notation.
(global-subword-mode t)

;; Display time in the mode line.
(display-time-mode t)

;; Don't ask again when converting a word to uppercase.
(put 'upcase-region 'disabled nil)

;; Don't ask for narrow to region again.
(put 'narrow-to-region 'disabled nil)

;; Refresh buffer.
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)

;; We pretty much want this always.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Disable back-ups.
(setq make-backup-files nil)

;; Start-up.
(setq inhibit-startup-screen t) ; Disable splash screen.
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

;; Make frame transparency overridable
(defvar efs/frame-transparency '(90 . 90))

(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Packages.
(require 'org)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Treemacs.
(global-set-key (kbd "<f1>") 'treemacs)

;; Comments.
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;; Completion for key bindings.
(which-key-mode 1)

;; Styles.
(setq c-default-style
      '((c-mode . "k&r")
        (c++-mode . "stroustrup")
        (other . "linux")))

;; Expand.
(global-set-key (kbd "C-'") 'dabbrev-expand)

;; Switch between .h and .cpp.
(global-set-key (kbd "C-c o") 'ff-find-other-file)

;; Emacs server.
(server-start)

;; Caret colour.
(set-cursor-color "#00ff00")

(require 'tree-sitter)
(require 'tree-sitter-langs)

(add-hook 'c++-mode-hook #'tree-sitter-mode)
(add-hook 'c-mode-hook #'tree-sitter-mode)

(add-hook 'c++-mode-hook #'tree-sitter-hl-mode)
(add-hook 'c-mode-hook #'tree-sitter-hl-mode)

(global-tree-sitter-mode)

;; LSP stuff.
(setq lsp-keymap-prefix "s-l")

(require 'lsp-mode)
(add-hook 'c-mode #'lsp)
(add-hook 'c++-mode #'lsp)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(leuven-dark))
 '(display-time-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(dap-mode helm-lsp flycheck lsp-ivy lsp-treemacs lsp-ui lsp-mode tree-sitter-langs tree-sitter projectile counsel ivy company eglot treemacs which-key beacon))
 '(tool-bar-mode nil)
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight regular :height 120 :width normal)))))
(put 'downcase-region 'disabled nil)
