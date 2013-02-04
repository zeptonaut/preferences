;; Set font
(set-face-attribute 'default nil :family "Anonymous Pro" :height 160)

;; Main google load file
(load-file "/google/src/files/head/depot/eng/elisp/google.el")

;; Make font smaller
(set-face-attribute 'default nil :height 120)

;; Default browser to Chrome
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; Set up grok
;;(setq grok-api-bns "/bns/global/ns/aggregator/grok/grokservice.us_east/2")
;;(grok-init)

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

;; Bind C-c C-r to reorder imports
(global-set-key "\C-c\C-r" 'google-imports-organize-imports)
(global-set-key (kbd "C-x <C-f11>") 'google3-build)


;; Set up grok
(setq grok-api-bns "/bns/global/ns/aggregator/grok/grokservice.us_east/2")
(grok-init)

;; Toggle between source and test file
(defun get-source-file (path)
  (replace-regexp-in-string "google3/javatests" "google3/java"
                            (replace-regexp-in-string "Test[\.]java" ".java" path)))

(defun get-test-file (path)
  (replace-regexp-in-string "google3/java" "google3/javatests"
                            (replace-regexp-in-string "[\.]java" "Test.java" path)))

(defun toggle-source-test-file ()
  (interactive)
  (if (string-match ".*Test\.java" buffer-file-name)
      (find-file (get-source-file buffer-file-name))
    (find-file (get-test-file buffer-file-name))))

(global-set-key (kbd "C-x <C-f10>") 'toggle-source-test-file)

;; MACROS
(fset 'insert-indirect-dependency
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 111 19 73 78 70 79 58 32 34 2 67108896 19 34 19 134217847 24 111 return 25 44 16 24 19] 0 "%d")) arg)))
