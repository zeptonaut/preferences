;; package allows for easy package management
;; Required here because this is what allows requiring of other packages.
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; automatically re-byte-compile any out of date files
(require 'auto-compile)
(auto-compile-on-load-mode 1)

;; Required here to allow chord bindings throughout the file
(require 'key-chord)

;; Disable annoying things
(setq visible-bell t)
(setq-default scroll-bar-width 10)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq enable-recursive-minibuffers t)

(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/themes/"))

(load-theme 'blue-gnus t)

;; highlight the current line
(global-hl-line-mode 1)
(custom-set-faces
 '(hl-line ((((class grayscale color) (background light)) (:background "gainsboro"))))
 '(hl-line ((((class grayscale color) (background dark)) (:background "color-234")))))

;; Show line numbers
(global-linum-mode t)

;; Always use filesystem versions of files
(global-auto-revert-mode 1)

;; Write all backups to the backup directory
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/backups/" t)))

;; Key bindings
(global-set-key (kbd "C-w") 'clipboard-kill-region)
(global-set-key (kbd "M-w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-y") 'clipboard-yank)
(global-set-key (kbd "C-c e") 'eval-region)
(global-set-key (kbd "C-c C-r") 'replace-string)
(global-set-key (kbd "C-c C-/") 'replace-regexp)
(global-set-key (kbd "C-c C-t i") 'timeclock-in)
(global-set-key (kbd "C-c C-t o") 'timeclock-out)
(global-set-key (kbd "C-c b") 'compile)
(global-set-key (kbd "C-c r") 'recompile)

;; When you have to do a split (switch-file-other-buffer), always
;; split vertically and use the split window that you already have
(setq split-width-threshold 1000)
(setq split-height-threshold nil)

(global-set-key (kbd "C-<") 'uncomment-region)
(global-set-key (kbd "C->") 'comment-region)

;; Bind F5 to revert-buffer (like refresh for a browser)
(global-set-key [f5] 'revert-buffer)

;; Changes enter to move to new line and indent
(global-set-key "\C-m" 'newline-and-indent)
;; Changes parameter list to indent by four indents
(setq c-offsets-alist '((arglist-intro . ++)))
(setq c-offsets-alist '((arglist-cont-nonempty . ++)))
;; Changes multiple line boolean statements to indent by two indents
(setq c-offsets-alist '((statement . ++)))
(setq c-offsets-alist '((statement-cont . ++)))

;; diminish keeps the modeline tidy.
;; Required here to let other modes diminish themselves.
(require 'diminish)

;; ace-jump allows you to jump around your current buffer
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(key-chord-define-global "jj" 'ace-jump-mode)

;; ace-jump-buffer allows you to jump around between buffers
(require 'ace-jump-buffer)
(key-chord-define-global "kk" 'ace-jump-buffer)

;; anzu shows total search results in the mode line while searching
(require 'anzu)
(global-anzu-mode +1)
(diminish 'anzu-mode)

;; autocomplete provides auto-completion based on other open buffers
(require 'auto-complete-config)
(ac-config-default)
(diminish 'auto-complete-mode)

;; command-frequency helps you find which commands you use most frequently
(add-to-list 'load-path "~/.emacs.d/plugins/command-frequency")
(require 'command-frequency)
(command-frequency-table-load)
(command-frequency-mode 1)
(command-frequency-autosave-mode 1)

;; find-things-fast allows you to easily find files within a project
(require 'find-file-in-project)
(key-chord-define-global "qq" 'find-file-in-project)
(add-to-list 'ffip-patterns "*.java")
(add-to-list 'ffip-patterns "*.h")
(add-to-list 'ffip-patterns "*.cc")
(add-to-list 'ffip-patterns "*.go")

;; flx-ido provides better flex matching for IDO
(require 'flx-ido)

;; ido gives you options in the minibuffer
(require 'ido)
(ido-mode 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)
(setq ido-auto-merge-work-directories-length -1) ; allow me to create files, dammit
(setq ido-enable-tramp-completion nil) ; ido over tramp = slow

;; ido-ubiquitous gives you ido-mode everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; js2-mode fixes javascript emacs
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; ac-js2-mode provides contextual autocomplete for javascript
;; NOTE: included here because of js2-mode dependency
;; (require 'ac-js2)
;; (add-hook 'js2-mode-hook 'ac-js2-mode)

;; key-chord allows you to use chords of keys as a binding option
(key-chord-mode 1)

;; magit is an emacs interface to git
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; nrepl
(add-to-list 'load-path "~/.emacs.d/plugins/nrepl")
(require 'nrepl)

;; There's currently a problem where smartparens doesn't pull in the
;; correct version of 'cl. This should fix the problem.
(unless (fboundp 'cl-flet)
  (defalias 'cl-flet 'flet))

;; smartparens provides various useful methods for handling balanced tags
(require 'smartparens-config)
(setq sp-autoskip-closing-pair 'always)
(smartparens-global-mode 1)
(diminish 'smartparens-mode)

;; subword-mode makes it so that camelcase is treated properyl
(global-subword-mode 1)

;; tramp allows you to locally edit remote files
(require 'tramp)
(setq tramp-default-method "ssh")

;; uniquify adds useful postfixes to uniquily identify buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; web-mode allows you to edit HTML files with other languages inline
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; windmove allows you to better navigate between frames
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; yasnippet provides template for frequently-used idioms
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet-0.6.1c")
(require 'yasnippet) ;; not yasnippet-bundle
(setq yas/trigger-key (kbd "C-c C-f"))
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")
(diminish 'yas/minor-mode)

;; Disable tabs in nxml mode
(add-hook 'nxml-mode-hook (lambda()
                            (setq indent-tabs-mode nil)))

;; language-specific modes
(require 'go-mode)
(require 'clojure-mode)
; (load "~/.emacs.d/plugins/nxhtml/autostart")
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;; Load additional modes
(add-to-list 'load-path "~/.emacs.d/modes")
(require 'markdown-mode)
(require 'javascript-mode)
(require 'php-mode)

;; Easily resize the frame with (C-c -) and (C-c +)
(setq my-frame-size 120.0)
(defun increase-frame-size-by-one-frame-size ()
  (interactive)
  (change-frame-size-by-number-of-frame-sizes 1))

(defun decrease-frame-size-by-one-frame-size ()
  (interactive)
  (change-frame-size-by-number-of-frame-sizes -1))

(defun change-frame-size-by-number-of-frame-sizes (vertical-split-delta)
  (set-vertical-split-count (+ (get-vertical-split-count) vertical-split-delta)))

(defun get-vertical-split-count ()
  (floor (/ (frame-width (selected-frame)) my-frame-size)))

(defun set-vertical-split-count (vertical-split-count)
  (let ((new-width (round (* my-frame-size vertical-split-count))))
    (set-frame-width (selected-frame) new-width)))

(global-set-key (kbd "C-c -") '(lambda ()
                                 (interactive)
                                 (decrease-frame-size-by-one-frame-size)
                                 (balance-windows)))
(global-set-key (kbd "C-c =") '(lambda ()
                                 (interactive)
                                 (increase-frame-size-by-one-frame-size)
                                 (balance-windows)))
