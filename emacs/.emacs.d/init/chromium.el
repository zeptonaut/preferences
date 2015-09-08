;; Load this file when emacs starts
(find-file "~/.emacs.d/init/common.el")

;; Load the base C/C++ style for Google
(load-file "~/.emacs.d/vendor/google-c-style.el")

;; Create a modified Google style for WebKit
(c-add-style "WebKit" '("Google"
                        (c-basic-offset . 4)
                        (fill-column . 120)
                        (c-offsets-alist . ((innamespace . 0)
                                            (access-label . -)
                                            (case-label . 0)
                                            (member-init-intro . +)
                                            (topmost-intro . 0)
                                            (arglist-cont-nonempty . +)))))
;; (require 'ycmd)
;; (require 'company-ycmd)
;; (require 'flycheck-ycmd)

;; (company-ycmd-setup)
;; (flycheck-ycmd-setup)

;; (add-hook 'c++-mode-hook 'ycmd-mode)
;; (add-hook 'c++-mode-hook 'company-mode)
;; ;; (add-hook 'c++-mode-hook 'flycheck-mode)
;; (set-variable 'ycmd-server-command (list "python" (substitute-in-file-name "$HOME/YouCompleteMe/ycmd/ycmd/__main__.py")))
;; (add-to-list 'ycmd-extra-conf-whitelist (substitute-in-file-name "$HOME/chromium/.ycm_extra_conf.py"))
;; ;; Show flycheck errors in idle mode as well
;; (setq ycmd-parse-conditions '(save new-line mode-enabled idle-change))
