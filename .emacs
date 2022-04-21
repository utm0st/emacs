(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(cyberpunk))
 '(custom-safe-themes
   '("b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" default))
 '(display-time-mode t)
 '(package-selected-packages '(cyberpunk-theme treemacs beacon))
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Fixed" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))

;; Disable the back-up feature.
(setq make-backup-files nil)

;; Disable visible scrollbar.
(scroll-bar-mode -1)

;; Disable the toolbar
(tool-bar-mode -1)

;; Disable tooltips
(tooltip-mode -1)

;; Enable the menu bar. The only reason why I have this on is because I sometimes
;; use Emacs in MSYS2.
(menu-bar-mode 1)

;; Delete the trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Disable beep.
(setq visible-bell 1)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;
;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
;; Pretty much what F5 does in other editors.
;;
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

;; Refresh buffer.
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)

;; Remove the file definitely!
(setq delete-by-moving-to-trash t)

;; Don't ask again when converting a word to uppercase.
(put 'upcase-region 'disabled nil)

;; Don't ask for narrow to region again.
(put 'narrow-to-region 'disabled nil)

;; Display time in the mode line.
(display-time-mode t)

;; Completion.
(global-set-key (kbd "C-'") 'dabbrev-expand)

;; Enable this to navigate by words that follow the camelCase notation.
(global-subword-mode t)

;; Startup emacs loading my config.
(find-file "~/.emacs")

;; Don't show startup screen.
(setq inhibit-startup-screen t)

;; Highlight current line.
(setq global-hl-line-mode t)

;; Show parenthesis.
(setq show-paren-mode t)

;; Show lines of code. Sometimes this is useful.
(global-set-key (kbd "<f12>") 'global-display-line-numbers-mode)

;; Comment and uncomment region.
(global-set-key (kbd "C-S-c") 'comment-region)
(global-set-key (kbd "C-M-Z") 'uncomment-region)

;; Move thru windows.
(global-set-key (kbd "M-S-<left>") 'windmove-left)
(global-set-key (kbd "M-S-<right>") 'windmove-right)
(global-set-key (kbd "M-S-<up>") 'windmove-up)
(global-set-key (kbd "M-S-<down>") 'windmove-down)

;; Compilation shortcuts.
(global-set-key (kbd "<f8>") 'compile)

;; Move between declaration and implementation file.
(global-set-key (kbd "C-c o") 'ff-find-other-file)

;; Highlight specific keywords.
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(make-face 'font-lock-temp-face)
(mapc (lambda (mode)
	(font-lock-add-keywords
	 mode
	 '(("\\<\\(TEMP\\)" 1 'font-lock-temp-face t)
	   ("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
	   ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-temp-face "Orange" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Green" nil nil t nil t nil nil)

;; Ido-mode (autocompletion, moving thru minibuffer with arrows and enter).
(ido-mode t)

;; Code styles.
(setq c-default-style
      '((java-mode . "java")
	(c++-mode . "stroustrup")
	(c-mode . "bsd")
        (awk-mode . "awk")
        (other . "linux")))

;; Little function to rename the file and its buffer.
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it's visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!!!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(global-set-key (kbd "C-c r")  'rename-file-and-buffer)

;; For checking my poor English.
(global-set-key (kbd "<f7>") 'ispell-region)

;; Highlight line when we swap buffers and so on.
(beacon-mode 1)

;; Visualize directory.
(global-set-key (kbd "<f1>") 'treemacs)

;; Useful to keep track of long lines and reduce its length.
(global-set-key (kbd "<f9>") 'display-fill-column-indicator-mode)

;; Transparency.

;; Make frame transparency overridable
(defvar efs/frame-transparency '(90 . 90))

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Width of fringe.
(set-fringe-mode 5)

;;
;; gdb.
;;
;; For GDB I typically have these options enabled.
;;
;; - Many windows.
;; - Show main.
;;
;; M-x customize > search for gdb.
;;
