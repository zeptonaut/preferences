;; Charlie Andrews
;; emacs configuration file

;; Disable annoying things
(tool-bar-mode -1)
(setq visible-bell t)

;; Use the clipboard for the kill ring
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

;; Set the font
(set-face-attribute 'default nil :family "Anonymous Pro" :height 160)

;; SLIME
;; Set up slime
(add-to-list 'load-path ".emacs.d/plugins/slime")
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))

;; Bind C-c C-v (next to C-c C-c) to uncomment region
(global-set-key "\C-c\C-v" 'uncomment-region)

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

;; Load additional modes
(add-to-list 'load-path "~/.emacs.d/modes")
(require 'markdown-mode)
(autoload 'javascript-mode "javascript" nil t)

;; color-theme
(add-to-list 'load-path "~/.emacs.d/plugins/color-theme-6.6.0")
(require 'color-theme)
(color-theme-initialize)
(color-theme-tm)

;; yasnippet
(add-to-list 'load-path	"~/.emacs.d/plugins/yasnippet-0.6.1c")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")

;; System specific loads
(if (string= system-name "charliea-desktop.chi.corp.google.com")
    ;; Work computer
    (load-file ".emacs.d/init/google.el")
  ;; Other
  (load-file ".emacs.d/init/other.el"))