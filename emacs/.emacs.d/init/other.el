(set-face-attribute 'default nil :height 110)

;; gocode doesn't work within google3, so only load this on personal computers
(require 'company-go)
;; only use company-go when completing in go-mode
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))
