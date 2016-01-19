;; Load this file when emacs starts
(find-file "~/.emacs.d/init/chromium.el")

;; Load the base C/C++ style for Google
(load-file "~/.emacs.d/vendor/google-c-style.el")
(add-hook 'c-mode-common-hook 'google-set-c-style);
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Create a modified Google style for Chromium
(c-add-style "Chromium" '("Google"))

;; Create a modified Google style for WebKit
(c-add-style "WebKit" '("Google"
                        (c-basic-offset . 4)
                        (fill-column . 120)
                        (c-offsets-alist . ((innamespace . 0)
                                            (access-label . -)
                                            (case-label . 0)
                                            (member-init-intro . +)
                                            (topmost-intro . 0)
                                            (arglist-cont-nonempty . +)))))

(add-to-list 'load-path "~/chromium/src/tools/gn/misc/emacs/")
(require 'gn-mode)

;; Provide a wrapper function to always run Chromium compile commands from
;; ~/chromium/src
(defun chromium-compile ()
  (interactive)
  (let ((default-directory "~/chromium/src/"))
    (call-interactively 'compile)))

(defun chromium-gdb ()
  (interactive)
  (let ((default-directory "~/chromium/src/"))
    (call-interactively 'gdb)))

;; Make ycmd shut up about its errors
(setq url-show-status nil)
(setq ycmd-request-message-level -1)

;; Load clang format to make C++ files pretty
;; (load-file "~/chromium/src/buildtools/clang_format/script/clang-format.el")

;; Define a minor mode to override the default compile keybinding so that it
;; runs chromium-compile instead
(defvar chromium-c++-minor-mode-map (make-keymap) "chromium-c++-minor-mode-keymap")
(define-key chromium-c++-minor-mode-map (kbd "C-c c") 'chromium-compile)
(define-key chromium-c++-minor-mode-map (kbd "C-c d") 'chromium-gdb)
(define-minor-mode chromium-c++-minor-mode
  t " chromium-c++" 'chromium-c++-minor-mode-map)
;; (add-hook 'chromium-c++-minor-mode-hook
;; 	  (lambda ()
;; 	    (add-hook 'before-save-hook 'clang-format-buffer nil t)))

(set-variable 'ycmd-extra-conf-whitelist '("~/chromium/*"))
