;; Main google load file
(load-file "/google/src/files/head/depot/eng/elisp/google.el")

;; Default browser to Chrome
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; Set up grok
(setq grok-api-bns "/bns/global/ns/aggregator/grok/grokservice.us_east/2")
(grok-init)

(require 'google-imports)
(global-set-key (kbd "s-i") 'google-imports-add-import-from-tag)
(require 'p4-google)                ;; g4-annotate, improves find-file-at-point
(require 'compilation-colorization) ;; colorizes output of (i)grep
(require 'rotate-clients)           ;; google-rotate-client
(require 'rotate-among-files)       ;; google-rotate-among-files
(require 'googlemenu)               ;; handy Google menu bar
(require 'p4-files)                 ;; transparent support for Perforce filesystem

;; Support for blaze builds
(require 'google3)
(require 'google3-build)
(setq google-build-system "blaze")

;; Easy searching of code
(require 'csearch)

;; Set up grok
(setq grok-api-bns "/bns/global/ns/aggregator/grok/grokservice.us_east/2")
(grok-init)