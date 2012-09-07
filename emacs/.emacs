;; Charlie Andrews
;; emacs configuration file

;; Disable annoying things
(tool-bar-mode -1)
(setq visible-bell t)

;; Set the font
(set-face-attribute 'default nil :family "Anonymous Pro" :height 175)

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

;; Changes enter to move to new line and indent
(global-set-key "\C-m" 'newline-and-indent)

;; Sets the tab style
(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)
(setq c-default-style "linux" c-basic-offset 2)

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

;; Set how to handle different file extensions
(setq auto-mode-alist
  (append
    '(("\\.C$" . c++-mode)
      ("\\.H$" . c++-mode)
      ("\\.cc$" . c++-mode)
      ("\\.hh$" . c++-mode)
      ("\\.c$" . c++-mode)
      ("\\.h$" . c++-mode)
      ("\\.cpp$" . c++-mode)
      ("\\.t$" . c++-mode)
      ("\\.js.erb" . javascript-mode)
      ("\\.l" . flex-mode)
      ("\\.y" . flex-mode)
      ("\\.html" . nxml-mode)
      ("\\.markdown" . markdown-mode)
      ("\\.js" . javascript-mode)
      ) auto-mode-alist))

;; Change where backup files are saved
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)

;; Copies the PATH variable from the terminal
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell 
      (replace-regexp-in-string "[[:space:]\n]*$" "" 
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))
