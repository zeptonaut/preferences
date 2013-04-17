;; Main google load file
(load-file "/google/src/files/head/depot/eng/elisp/google.el")

;; Make font smaller
(set-face-attribute 'default nil :height 115)

;; Default browser to Chrome
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(require 'google-imports)
(require 'p4-google)
(require 'compilation-colorization)
(require 'rotate-among-files)
(require 'p4-files)

;; Support for blaze builds
(require 'google3)
(require 'google3-build)
(setq google-build-system "blaze")

;; Set up grok
(setq grok-api-bns "/bns/global/ns/aggregator/grok/grokservice.us_east/2")
(grok-init)

(global-set-key "\C-c\C-r" 'google-imports-organize-imports)
(global-set-key (kbd "C-x <C-f11>") 'google3-build)
(global-set-key [f2] 'google-rotate-among-files)
(global-set-key (kbd "<C-f8>") 'google-imports-add-import-from-tag)

;; Highlight tabs and lines over 100 characters long
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq whitespace-line-column 100)
(set-face-attribute 'whitespace-tab nil
                    :foreground "#EEE"
                    :background "#D44"
                    :weight 'bold)
(set-face-attribute 'whitespace-line nil
                    :foreground "#EEE"
                    :background "#D44"
                    :weight 'bold)
(add-hook 'nxml-mode-hook (lambda () (whitespace-mode t)))
(add-hook 'java-mode-hook (lambda () (whitespace-mode t)))

;; Organize java imports before saving
(add-hook 'java-mode-hook (lambda ()
                            (add-hook 'before-save-hook 'google-imports-organize-imports nil t)))

(global-set-key (kbd "C-c l") (lambda () (interactive) (compile "git5 --no-pager lint -v")))
(global-set-key (kbd "C-c p") (lambda () (interactive) (compile "git5 export -p all")))
(global-set-key (kbd "C-c b") 'google3-build)
(global-set-key (kbd "C-c t") 'google3-test)
