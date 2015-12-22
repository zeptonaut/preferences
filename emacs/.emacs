(load-file "~/.emacs.d/init/common.el")

;; System specific loads
(if (string= system-name "charliea.arb.corp.google.com")
    (load-file "~/.emacs.d/init/google.el")
    (load-file "~/.emacs.d/init/other.el"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(ido-enable-last-directory-history nil)
 '(ido-max-work-directory-list 0)
 '(ido-max-work-file-list 0)
 '(ido-record-commands nil)
 '(org-agenda-custom-commands
   (quote
    (("w" "Today at work"
      ((agenda ""
	       ((org-agenda-ndays 1)
		(org-agenda-tag-filter-preset
		 (quote
		  ("+WORK")))))))
     ("u" "Unscheduled" tags "-SCHEDULED={.+}-DEADLINE={.+}/!+TODO|+STARTED|+WAITING"))))
 '(org-agenda-dim-blocked-tasks t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy (quote (time-up priority-down)))
 '(org-capture-templates
   (quote
    (("t" "todo" entry
      (file+headline "~/Dropbox/org/todo.org" "Inbox")
      "* TODO %?
")
     ("w" "waiting" entry
      (file+headline "~/Dropbox/org/todo.org" "Inbox")
      "* WAITING %?
"))))
 '(org-directory "~/Dropbox/org")
 '(org-log-done t)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-targets
   (quote
    (("~/Dropbox/org/todo.org" :maxlevel . 3)
     ("~/Dropbox/org/someday.org" :level . 3))))
 '(org-refile-use-outline-path (quote file))
 '(safe-local-variable-values
   (quote
    ((define-auto-insert ".cc" "chromium_header.tmpl")
     (define-auto-insert ".h" "chromium_header.tmpl")
     (define-auto-insert "\\.\\([Cc]\\|cc\\|cpp\\|h\\)\\'"
       (quote chromium-file))
     (chromium-c++minor-mode)
     (local-set-key
      (kbd "C-c C-c")
      (quote chromium-compile))
     (local-set-key
      (kbd "C-t C-w")
      (quote chromium-compile))
     (local-set-key
      (kbd "C-c c")
      (quote chromium-compile))
     (flycheck-mode t)
     (company-mode t)
     (ycmd-mode t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
