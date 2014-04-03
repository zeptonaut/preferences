;; Make font smaller
(set-face-attribute 'default nil :family "Inconsolata" :height 120)

;; ;; ;; Open up this file as a buffer as soon as emacs opens
;; (find-file "~/.emacs.d/init/google.el")

;; ;; Main google load file
;; (load-file "/google/src/files/head/depot/eng/elisp/google.el")
;; (load-file "/google/src/files/head/depot/eng/elisp/google-cc-extras.el")
;; (load-file "/google/src/files/head/depot/eng/elisp/google-autogen.el")

;; ;; Default browser to Chrome
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "google-chrome")

;; (require 'google)
;; (require 'google-imports)
;; (require 'p4-google)
;; (require 'compilation-colorization)
;; (require 'rotate-among-files)
;; (require 'p4-files)
;; (require 'google-autogen)
;; (require 'google-jswat)
;; ;; Support for blaze builds
;; (require 'google3)
;; (require 'google3-build)
;; (setq google-build-system "blaze")

;; ;; (require 'google-flymake)

;; ;; NOTE: These are still a bit flaky. Fix them.
;; ;; C++ and java autocomplete
;; ;; (require 'gcomplete)
;; ;; (gcomplete-setup-flymake)
;; ;; (gcomplete-setup-for-auto-complete)


;; ;; Set up grok
;; (setq grok-api-bns "/bns/global/ns/aggregator/grok/grokservice.us_east/2")
;; (grok-init)

;; (global-set-key (kbd "C-c b") 'google3-build)
;; (add-hook 'c++-mode-hook
;;           (lambda () (local-set-key (kbd "C-c f") 'google-clang-format-file)))
;; (global-set-key (kbd "C-c i") 'google-imports-add-import-from-tag)
;; (global-set-key (kbd "C-c l") (lambda () (interactive) (compile "git5 --no-pager lint -v")))
;; (global-set-key (kbd "C-c p") (lambda () (interactive) (compile "git5 export -p all")))
;; (global-set-key (kbd "C-c t") 'google3-test)
;; (global-set-key (kbd "C-c x") 'google-rotate-among-files)
;; (global-set-key (kbd "C-c s") 'cs)
;; (global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
;; (global-set-key (kbd "C-c o") 'google-imports-organize-imports)

;; ;; Highlight tabs and lines over 100 characters long
;; (require 'whitespace)
;; ;(setq whitespace-style '(face empty tabs lines-tail trailing))
;; (set-face-attribute 'whitespace-tab nil
;;                     :foreground "#EEE"
;;                     :background "#D44"
;;                     :weight 'bold)
;; (set-face-attribute 'whitespace-line nil
;;                     :foreground "#EEE"
;;                     :background "#D44"
;;                     :weight 'bold)
;; (add-hook 'nxml-mode-hook (lambda ()
;;                             (setq whitespace-line-column 100)
;;                             (whitespace-mode t)))
;; (add-hook 'java-mode-hook (lambda ()
;;                             (setq whitespace-line-column 100)
;;                             (setq-default fill-column 100)
;;                             (whitespace-mode t)))
;; (add-hook 'c++-mode-hook (lambda ()
;;                            (setq whitespace-line-column 80)
;;                            (setq-default fill-column 80)
;;                            (whitespace-mode t)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(hl-line ((t (:background "gainsboro"))))
;;  '(whitespace-line ((t (:underline "firebrick")))))

;; (add-hook 'c-mode-common-hook
;; 	  (lambda()
;; 	    (add-hook 'write-contents-functions
;; 		      (lambda()
;; 			(save-excursion
;; 			  (google-imports-organize-imports))))))

;; ;; autogen new files
;; (setq google-autogen-on-file-not-found 'cc-generator)

;; ;; increase gc threshold
;; (setq gc-cons-threshold 20000000)

;; ;; Fix jswat path
;; (defun jswat ()
;;   (interactive)
;;   (setq google-jdb-jswat-command "~/bin/jswat-launcher")
;;   (google-jswat))

;; ;; Fix java indentation
;; (add-to-list 'load-path "~/.emacs.d/plugins/java-mode-indent-annotations")
;; (require 'java-mode-indent-annotations)

;; (add-hook 'java-mode-hook (lambda ()
;; 	    (c-set-offset 'statement-cont
;; 			  '(c-no-indent-after-java-annotations ++))))
