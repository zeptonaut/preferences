;; package allows for easy package management
;; Required here because this is what allows requiring of other packages.
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Load this file by default
(find-file "~/.emacs.d/init/common.el")

;; Makes path be the same as in my shell
(exec-path-from-shell-initialize)

;; Required here to allow chord bindings throughout the file
(require 'key-chord)
(key-chord-mode 1)

;; Disable annoying things
(setq visible-bell t)
(setq enable-recursive-minibuffers t)

(setq vc-follow-symlinks t)

;; Always use filesystem versions of files
(global-auto-revert-mode 1)

;; Don't create lockfiles
(setq create-lockfiles nil)

;; Key bindings
(global-set-key (kbd "C-w") 'clipboard-kill-region)
(global-set-key (kbd "M-w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-y") 'clipboard-yank)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key (kbd "C-c e") 'eval-region)
(global-set-key (kbd "C-c E") 'eval-buffer)
(global-set-key (kbd "C-c C-r") 'replace-string)
(global-set-key (kbd "C-c C-/") 'replace-regexp)
(global-set-key (kbd "C-c b") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key [f5] 'revert-buffer)

;; When you have to do a split (switch-file-other-buffer), always
;; split vertically and use the split window that you already have
(setq split-width-threshold 1000)
(setq split-height-threshold nil)

;; diminish keeps the modeline tidy.
;; Required here to let other modes diminish themselves.
(require 'diminish)

;; ace-jump allows you to jump around your current buffer
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(key-chord-define-global "jj" 'ace-jump-mode)

;; anzu shows total search results in the mode line while searching
(require 'anzu)
(global-anzu-mode +1)
(diminish 'anzu-mode)

;; autocomplete provides auto-completion based on other open buffers
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ac-delay 0.010)
;; (setq ac-menu-height 20)
;; (diminish 'auto-complete-mode)

;; circe is an IRC client within emacs
(require 'circe)

;; coffee-mode provides support for coffeescript
(require 'coffee-mode)
(custom-set-variables '(coffee-tab-width 2))

;; company mode provides an auto-complete framework
(require 'company)
(global-set-key (kbd "M-/") 'company-complete)
(setq company-minimum-prefix-length 1)
(setq company-show-numbers t)
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .1)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
;; (add-hook 'after-init-hook 'global-company-mode)

;; company-go provides auto-completion for go code
(require 'company-go)
(add-hook 'go-mode-hook (lambda ()
			  (set (make-local-variable 'company-backends) '(company-go))
			  (company-mode)))

(require 'emmet-mode)

;; find-file-in-project finds files in projects
(require 'find-file-in-project)
(key-chord-define-global "jf" 'find-file-in-project)
(custom-set-variables '(ffip-limit 2056)
                      '(ffip-fullpaths 1))

;; flx-ido provides better flex matching for IDO
(require 'flx-ido)

;; flycheck shows errors as you go
(require 'flycheck)
(add-hook 'coffee-mode-hook (lambda ()
                              (flycheck-mode)))
(add-hook 'python-mode-hook (lambda ()
                              (flycheck-mode)))
(setq flycheck-display-errors-delay 0.01)

;; (require 'flymake)
;; (setq flymake-start-syntax-check-on-newline nil)
;; ;; Make the flymake errors more obvious
;; (custom-set-faces
;;  '(flymake-errline ((t (:background "firebrick" :foreground "color-231")))))
;; ;; Show the flymake error in the minibuffer
;; (custom-set-variables
;;  '(help-at-pt-timer-delay 0.1)
;;  '(help-at-pt-display-when-idle '(flymake-overlay)))
;; (key-chord-define-global ".," 'flymake-goto-prev-error)
;; (key-chord-define-global ",." 'flymake-goto-next-error)

;; go-autocomplete provides autocomplete for go
;(require 'go-autocomplete)
;(require 'auto-complete-config)
;(add-hook 'before-save-hook 'gofmt-before-save) ;; automatically format before saving

;; go-flymake provides syntax checking for go
;(add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
;(require 'go-flymake)

(require 'go-mode)
(setq gofmt-command "goimports")
(add-hook 'go-mode-hook (lambda()
                          (add-hook 'before-save-hook 'gofmt-before-save)
                          (setq tab-width 2
                                whitespace-style '())))

;; hl-line+ highlights the current line when emacs is idle
(require 'hl-line+)
(toggle-hl-line-when-idle 1)

;; ido gives you options in the minibuffer
(flx-ido-mode 1)
(setq ido-use-faces nil)
(setq ido-auto-merge-work-directories-length -1) ; allow me to create files, dammit
(setq ido-enable-tramp-completion nil) ; ido over tramp = slow

;; Don't use ido's history, because it gets in the way more than it helps
(custom-set-variables
 '(ido-enable-last-directory-history nil)
 '(ido-record-commands nil)
 '(ido-max-work-directory-list 0)
 '(ido-max-work-file-list 0))

;; ido-ubiquitous gives you ido-mode everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations '("\n-> " "" "\n   " "\n   ..."
                        "[" "]" " [No match]" " [Matched]"
                        " [Not readable]" " [Too big]" " [Confirm]"))
(defun ido-disable-line-trucation () 
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; Returns true if the current buffer is part of Chromium
(defun is-chromium ()
  (string-match "chrome/src/" buffer-file-name))

(add-hook 'c++-mode-hook (lambda() (if (is-chromium)
                                       (irony-mode))))

;; jedi understands python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; js2-mode fixes javascript in emacs
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))

;; ac-js2-mode provides contextual autocomplete for javascript
;; NOTE: included here because of js2-mode dependency
;; (require 'ac-js2)
;; (add-hook 'js2-mode-hook 'ac-js2-mode)
;; (setq ac-js2-evaluate-calls t)
;; (setq ac-js2-external-libraries '(concat user-emacs-directory "lib/d3.min.js"))

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
(setq org-directory "~/Dropbox/org/projects")
(setq org-agenda-files (list "~/Dropbox/org/projects"))
(setq org-mobile-directory "~/Dropbox/mobileorg")
(add-hook 'org-mode-hook (lambda() (visual-line-mode t)))

(add-hook 'org-mode-hook (lambda()
                           (visual-line-mode t)))
(custom-set-variables `(org-startup-folded (quote children)))
(setq org-tag-persistent-alist '(("waiting" . ?w)))

;; Files count as part of the tree
(setq org-refile-use-outline-path 'file)
;; Give us the whole tree, because we're using IDO
(setq org-outline-path-complete-in-steps nil)
;; Only list the files, don't list any subtrees within the file
(setq org-refile-targets '((org-agenda-files :level . 0)))
;; q-"capture"
(key-chord-define-global "qc" 'org-capture)
;; q-"switch"
(key-chord-define-global "qs" 'org-iswitchb)
(setq org-completion-use-ido t)
;; q-"waiting"
(key-chord-define-global "qw" (lambda ()
                                (interactive)
                                (org-toggle-tag "waiting")))

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/inbox.org") "* TODO %?\n"))))

(require 'ox-md)
(eval-after-load "org"
  '(require 'ox-md nil t))

;; TODO(charliea): Use this in google.el.
;; (require 'org-mobile-sync)
;; (org-mobile-sync-mode 1)

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
(setq show-paren-delay 0.01)

;; subword-mode makes it so that camelcase is treated properyl
(global-subword-mode 1)

;; tramp allows you to locally edit remote files
(require 'tramp)
(setq tramp-default-method "ssh")

;; web-mode allows you to edit HTML files with other languages inline
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(setq web-mode-code-indent-offset 2)

;; web-beautify prettifies HTML/CSS/JS
(require 'web-beautify)
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c C-b") 'web-beautify-js))

;; whitespace highlights lines that are too long
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(set-face-attribute 'whitespace-line nil
                    :background "red1"
                    :foreground "white")
(add-hook 'python-mode-hook 'whitespace-mode)
(add-hook 'python-mode-hook (lambda()
                              (setq whitespace-line-column 80
                                    whitespace-style '(face tabs trailing lines-tail))))

(add-hook 'prog-mode-hook 'whitespace-mode)

;; ws-butler cleans up whitespace, but only on lines that you touch
(require 'ws-butler)
(add-hook 'c++-mode-hook 'ws-butler-mode)

;; yasnippet provides template for frequently-used idioms
(require 'yasnippet)
(key-chord-define-global "kj" 'yas-expand-from-trigger-key)
(yas-global-mode 1)
(diminish 'yas-minor-mode)

;; Disable tabs in nxml mode
(add-hook 'nxml-mode-hook (lambda()
                            (setq indent-tabs-mode nil)))

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

;; Playground
(add-to-list 'load-path "~/.emacs.d/playground")
