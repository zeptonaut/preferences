(set-face-attribute 'default nil :family "Anonymous Pro" :height 200)

(require 'markdown-mode)
(require 'javascript-mode)
(require 'php-mode)

(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
