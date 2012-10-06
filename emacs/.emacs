;; Charlie Andrews
;; emacs configuration file

(load-file "~/.emacs.d/init/common.el")

;; System specific loads
(if (string= system-name "charliea-desktop.chi.corp.google.com")
    ;; Work computer
    (load-file "~/.emacs.d/init/google.el")
  ;; Other
  (load-file "~/.emacs.d/init/other.el"))
