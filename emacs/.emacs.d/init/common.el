;; package allows for easy package management
;; Required here because this is what allows requiring of other packages.
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Required here to allow chord bindings throughout the file
(require 'key-chord)

;; Disable annoying things
(setq visible-bell t)
(setq enable-recursive-minibuffers t)

(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/themes/"))

(setq vc-follow-symlinks t)

;; (load-theme 'zenburn t)

;; Always use filesystem versions of files
(global-auto-revert-mode 1)

;; Don't create lockfiles
(setq create-lockfiles nil)

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

;; language-specific modes
;; (require 'go-mode)
(require 'clojure-mode)
(require 'clojure-test-mode)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

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
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ac-delay 0.050)
;; (setq ac-menu-height 20)
;; (diminish 'auto-complete-mode)

;; better-defaults fixes some of emacs's bad defaults
(require 'better-defaults)

;; cider provides an IDE and REPL for clojure
(require 'cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; clean-buffer-list cleans out all buffers that haven't been used in a while
(setq clean-buffer-list-delay-general 1)
(setq clean-buffer-list-kill-never-buffer-names
      '("common.el"
        "google.el"))

;; command-frequency helps you find which commands you use most frequently
(add-to-list 'load-path "~/.emacs.d/plugins/command-frequency")
(require 'command-frequency)
(command-frequency-table-load)
(command-frequency-mode 1)
(command-frequency-autosave-mode 1)

;; company mode provides an auto-complete framework
(require 'company)
(global-set-key (kbd "M-/") 'company-complete)
(setq company-idle-delay 0.010)
(setq company-minimum-prefix-length 3)
(setq company-show-numbers t)

(custom-set-faces
 '(company-preview
   ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common
   ((t (:inherit company-preview))))
 '(company-tooltip
   ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-selection
   ((t (:background "black" :foreground "lightgray"))))
 '(company-tooltip-common
   ((((type x)) (:inherit company-tooltip :weight bold))
    (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection
   ((((type x)) (:inherit company-tooltip-selection :weight bold))
    (t (:inherit company-tooltip-selection))))
 `(company-scrollbar-bg ((t (:background "white"))))
 `(company-scrollbar-fg ((t (:background "black")))))

;; desktop-save-mode automatically saves emacs when closing it
(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil)
(desktop-save-mode 1)

(require 'emmet-mode)

;; find-file-in-project allows you to easily find files within a project
(require 'find-file-in-project)
(key-chord-define-global "qq" 'ffip)
(add-to-list 'ffip-patterns "*.java")
(add-to-list 'ffip-patterns "*.h")
(add-to-list 'ffip-patterns "*.cc")
(add-to-list 'ffip-patterns "*.go")
(setq ffip-limit 20000)

;; flx-ido provides better flex matching for IDO
(require 'flx-ido)

(require 'flymake)
(setq flymake-start-syntax-check-on-newline nil)
;; Make the flymake errors more obvious
(custom-set-faces
 '(flymake-errline ((t (:background "firebrick" :foreground "color-231")))))
;; Show the flymake error in the minibuffer
(custom-set-variables
 '(help-at-pt-timer-delay 0.1)
 '(help-at-pt-display-when-idle '(flymake-overlay)))
(key-chord-define-global ".," 'flymake-goto-prev-error)
(key-chord-define-global ",." 'flymake-goto-next-error)

;; go-autocomplete provides autocomplete for go
;(require 'go-autocomplete)
;(require 'auto-complete-config)
;(add-hook 'before-save-hook 'gofmt-before-save) ;; automatically format before saving

;; go-flymake provides syntax checking for go
;(add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
;(require 'go-flymake)

;; hl-line+ highlights the current line when emacs is idle
(require 'hl-line+)
(toggle-hl-line-when-idle 1)

;; ido gives you options in the minibuffer
(flx-ido-mode 1)
(setq ido-use-faces nil)
(setq ido-auto-merge-work-directories-length -1) ; allow me to create files, dammit
(setq ido-enable-tramp-completion nil) ; ido over tramp = slow

;; ido-ubiquitous gives you ido-mode everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; js2-mode fixes javascript in emacs
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; ac-js2-mode provides contextual autocomplete for javascript
;; NOTE: included here because of js2-mode dependency
;; (require 'ac-js2)
;; (add-hook 'js2-mode-hook 'ac-js2-mode)
;; (setq ac-js2-evaluate-calls t)
;; (setq ac-js2-external-libraries '(concat user-emacs-directory "lib/d3.min.js"))

;; key-chord allows you to use chords of keys as a binding option
(key-chord-mode 1)

;; magit is an emacs interface to git
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; markdown-mode gives an emacs mode for markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README.md\\'" . gfm-mode))

;; org-mode helps you organize your life
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/.emacs.d/org"))
(require 'ox-md)
(eval-after-load "org"
  '(require 'ox-md nil t))

;; Rainbow delimiters changes the color of () and {} so that it's
;; easier to see when they're matched
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; scss-mode allows you to work with Sass
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; smartparens provides various useful methods for handling balanced tags
(require 'smartparens-config)
(require 'smartparens-html)
(setq sp-highlight-wrap-overlay nil
      sp-autoescape-string-quote nil
      sp-autoskip-closing-pair 'always)
(add-to-list 'sp--html-modes '(nxml-mode))
(smartparens-global-mode 1)
(diminish 'smartparens-mode)

;; Reduce the amount of time it takes the matching parenthesis to show up
(setq show-paren-delay 0)

;; subword-mode makes it so that camelcase is treated properyl
(global-subword-mode 1)

;; tramp allows you to locally edit remote files
(require 'tramp)
(setq tramp-default-method "ssh")

;; web-mode allows you to edit HTML files with other languages inline
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; windmove allows you to better navigate between frames
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; ws-butler cleans up whitespace, but only on lines that you touch
(require 'ws-butler)
(add-hook 'c++-mode-hook 'ws-butler-mode)

;; yasnippet provides template for frequently-used idioms
(require 'yasnippet)
(setq yas/trigger-key (kbd "C-c C-f"))
(yas-global-mode 1)
(diminish 'yas-minor-mode)

;; Disable tabs in nxml mode
(add-hook 'nxml-mode-hook (lambda()
                            (setq indent-tabs-mode nil)))

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
