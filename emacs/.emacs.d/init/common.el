;; Load this file when emacs starts
(find-file "~/.emacs.d/init/common.el")

;; package allows for easy package management
;; Required here because this is what allows requiring of other packages.
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Change the default font
(set-face-attribute 'default nil :height 110)

;; Disable the useless start screen
(setq inhibit-splash-screen t)

;; Create a thin scroll bar for every window
(scroll-bar-mode 1)
(set-scroll-bar-mode 'right)
;;(setq-default scroll-bar-width 10)

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq-default indent-tabs-mode nil)

;; Disable annoying things
(setq visible-bell t)
(setq enable-recursive-minibuffers t)
(setq vc-follow-symlinks t)
(setq create-lockfiles nil)

;; Makes path be the same as in my shell
(exec-path-from-shell-initialize)

;; Always use filesystem versions of files
(global-auto-revert-mode 1)

;; Load the solarized theme
;; Don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)
;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)
;; Don't change size of org-mode headlines (but keep other size-changes)
(setq solarized-scale-org-headlines nil)
;; Avoid all font-size changes
(setq solarized-height-minus-1 1)
(setq solarized-height-plus-1 1)
(setq solarized-height-plus-2 1)
(setq solarized-height-plus-3 1)
(setq solarized-height-plus-4 1)
(load-theme 'solarized-dark t)

;; Set the default fill column to 80.
(setq-default fill-column 80)

;; Required here to allow chord bindings throughout the file
(require 'key-chord)
(key-chord-mode 1)

(global-set-key "\C-m" 'newline-and-indent)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-c e") 'eval-region)
(global-set-key (kbd "C-c E") 'eval-buffer)
(global-set-key (kbd "C-c C-r") 'replace-string)
(global-set-key (kbd "C-c C-/") 'replace-regexp)
(global-set-key (kbd "C-c [") (lambda () (interactive) (profiler-start 'cpu)))
(global-set-key (kbd "C-c ]") 'profiler-stop)
(global-set-key (kbd "C-c l") 'profiler-report)
(global-set-key [f5] 'revert-buffer)

;; Always scroll compilation output to the bottom.
(setq compilation-scroll-output 'first-error)
;; Always kill any existing compilations with successive ones.
(setq compilation-always-kill t)

;; When you have to do a split (switch-file-other-buffer), always
;; split vertically and use the split window that you already have
(setq split-width-threshold 1000)
(setq split-height-threshold nil)

;; swiper
(global-set-key "\C-s" 'swiper)

(setq profiler-max-stack-depth 30)

;; Default to 80 column fill for C++
(add-hook 'c-mode-common-hook (lambda ()
				;; Set the fill column to 80
				(customize-set-variable 'fill-column 80)
				;; Enable auto newline mode and hungry deletions
				(c-toggle-hungry-state 1)
				(c-toggle-electric-state)))

;; diminish keeps the modeline tidy.
;; Required here to let other modes diminish themselves.
(require 'diminish)

;; auto-insert provides skeletons for new buffers
(setq auto-insert-directory "~/.emacs.d/templates")

;; flycheck shows errors as you go
(require 'flycheck)
(add-hook 'coffee-mode-hook (lambda ()
                              (flycheck-mode)))
(add-hook 'python-mode-hook (lambda ()
                              (flycheck-mode)))
(setq flycheck-display-errors-delay 0.01)

;; go-autocomplete provides autocomplete for go
;(require 'go-autocomplete)
;(require 'auto-complete-config)
;(add-hook 'before-save-hook 'gofmt-before-save) ;; automatically format before saving

;; go-flymake provides syntax checking for go
;(add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
					;(require 'go-flymake)

;; Make gdb run in many-windows move, similar to how Eclipse works
(setq gdb-many-windows 1)

(require 'go-mode)
(setq gofmt-command "goimports")
(add-hook 'go-mode-hook (lambda()p
                          (add-hook 'before-psave-hook 'gofmt-before-save)
                          (setq tab-width 2f
                                whitespace-style '())))

;; hl-line+ highlights the current line when emacs is idle
(require 'hl-line+)
(toggle-hl-line-when-idle 1)

;; ivy-mode provides better minibuffer completion
(ivy-mode 1)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-display-style 'fancy)

;; js2-mode fixes javascript in emacs
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-idle-timer-delay 1))

;; markdown-mode gives an emacs mode for markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README.md\\'" . gfm-mode))

;; projectile provides an easy way to manage projects
(require 'projectile)
(diminish 'projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode 1)
(setq projectile-git-command "fd . -0")
(setq projectile-generic-command "fd . -0")
(setq projectile-enable-caching t)
(setq projectile-completion-system 'ivy)
(key-chord-define-global "qf" 'projectile-find-file)

;; python-mode
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

;; show-paren-mode highlights matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0.001)

(require 're-builder)
(setq reb-re-syntax 'string)

;; subword-mode makes it so that camelcase is treated properly
(global-subword-mode 1)

;; tramp allows you to locally edit remote files
(require 'tramp)
(setq tramp-default-method "ssh")

;; whitespace highlights lines that are too long
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(set-face-attribute 'whitespace-line nil
                    :background "red1"
                    :foreground "white")
(add-hook 'python-mode-hook (lambda()
                              (setq whitespace-line-column 80
                                    whitespace-style '(face tabs trailing lines-tail))))
(add-hook 'prog-mode-hook 'whitespace-mode)
(diminish 'whitespace-mode)

;; Make shift-arrow keys move between buffers
(windmove-default-keybindings)

;; ws-butler cleans up whitespace, but only on lines that you touch
(require 'ws-butler)
(ws-butler-global-mode 1)
(diminish 'ws-butler-mode)

;; yasnippet provides template for frequently-used idioms
(require 'yasnippet)
(yas-global-mode)
;; Don't use the default snippets
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(key-chord-define-global "qo" 'yas-expand-from-trigger-key)
;; Tell yasnippet not to mess with the spacing
(setq yas-indent-line 'none)

(diminish 'yas-minor-mode)


(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(setq async-shell-command-display-buffer nil)
(defun git-webdiff-master ()
  (interactive)
  (shell-command "((git webdiff master &) >& /dev/null &)"))
(key-chord-define-global "qw" 'git-webdiff-master)


;; ;; C++ tools for coding in Chromium
(load-file "~/.emacs.d/init/chromium.el")

;; Playground
(add-to-list 'load-path "~/.emacs.d/playground")
(add-to-list 'load-path "~/.emacs.d/vendor")
