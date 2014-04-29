(load-file "~/.emacs.d/init/common.el")

;; System specific loads
(if (string= system-name "charliea-desktop.chi.corp.google.com")
    (load-file "~/.emacs.d/init/google.el")
    (load-file "~/.emacs.d/init/other.el"))
