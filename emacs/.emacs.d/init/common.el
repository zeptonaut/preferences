;; Disable annoying things
(setq visible-bell t)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Use the clipboard for the kill ring
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

;; When you have to do a split (switch-file-other-buffer), always split vertically
;; and use the split window that you already have
(setq split-width-threshold 1000)
(setq split-height-threshold nil)

;; Set font
(set-face-attribute 'default nil :family "Anonymous Pro" :height 120)

;; Automatically set the window width and height
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    (add-to-list 'default-frame-alist
         (cons 'width (/ (- (x-display-pixel-width) 50)
                             (frame-char-width))))

    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 50)
                             (frame-char-height)))))))
(set-frame-size-according-to-resolution)

;; Change backups
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/backups/" t)))

;; Bind C-c C-v (next to C-c C-c) to uncomment region
(global-set-key "\C-c\C-v" 'uncomment-region)

;; Bind F9 to switch to the terminal in the other window
(defun open-ansi-term-other-window ()
  (switch-to-buffer-other-window (get-buffer "*ansi-term*")))
(global-set-key [f9] 'open-ansi-term-other-window)

;; Bind F10 to toggle between header/source files
(global-set-key [f10] 'ff-find-other-file)

;; Bind F11 to compile
(global-set-key [f11] 'recompile)

;; Changes enter to move to new line and indent
(global-set-key "\C-m" 'newline-and-indent)

;; Changes parameter list to indent by four indents
(setq c-offsets-alist '((arglist-intro . ++)))
(setq c-offsets-alist '((arglist-cont-nonempty . ++)))
;; Changes multiple line boolean statements to indent by two indents
(setq c-offsets-alist '((statement . ++)))
(setq c-offsets-alist '((statement-cont . ++)))

;; Load additional modes
(add-to-list 'load-path "~/.emacs.d/modes")
(require 'markdown-mode)
(require 'javascript-mode)
(require 'php-mode)

;; IDO
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Desktop mode
(defun save-emacs-state ()
  "Saves the desktop as of right now, so if it dies it'll come back in
  the right place."
  (interactive)
  (desktop-save desktop-dirname)
  (message "Saved desktop %s" desktop-dirname))

;; Save the desktop state after a bunch of idle time
(setq save-emacs-timer
      (run-with-idle-timer 300 t 'save-emacs-state))

'(desktop-path (quote ("~/.emacs.d/backups")))
'(desktop-restore-eager 10)
'(desktop-save t)
'(desktop-save-mode t)

;; color-theme
(add-to-list 'load-path "~/.emacs.d/plugins/color-theme-6.6.0")
(require 'color-theme)
(color-theme-initialize)
(color-theme-vim-colors)

;; yasnippet
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet-0.6.1c")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")

;; auto-install
(add-to-list 'load-path "~/.emacs.d/plugins/auto-install")
(require 'auto-install)

;; popup
(add-to-list 'load-path "~/.emacs.d/plugins/popup")
(require 'popup)

;; autocomplete
(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete/")
(require 'auto-complete-config)
(ac-config-default)

;; Slime
(add-to-list 'load-path "~/.emacs.d/plugins/slime")
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))