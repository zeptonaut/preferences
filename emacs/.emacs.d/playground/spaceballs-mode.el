;; ;; Major mode for generating random spaceballs quotes


;; (defvar spaceballs-mode-hook nil
;;   "Hooks to be run when entering spaceballs-mode.")
;; (defvar spaceballs-mode-map
;;   (let ((map (make-keymap)))
;;     (define-key map (kbd "RET") 'insert-random-spaceballs-quote)
;;     map)
;;   "Keymap for spaceballs-mode")




;; (defun insert-random-spaceballs-quote (arg)
;;   (interactive "P")
;;   (dotimes (number (or arg 0))
;;     (insert (get-random-element spaceballs-lines) "\n")))



;; (provide 'spaceballs-mode)

;; ~/.emacs.d/playground/spaceballs-mode.el

(defun get-lines-from-file (file-path)
  "Returns the lines of the file at file-path as a list."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun get-random-element (lst)
  "Returns a random element of a list."
  (elt lst (random (length lst))))


(defvar spaceballs-mode-hook nil
  "Hooks to be run when entering spaceballs-mode.")
(defvar spaceballs-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "RET") 'insert-random-spaceballs-quote)
    map)
  "Keymap for spaceballs-mode")
(defvar spaceballs-lines
  (get-lines-from-file "~/.emacs.d/playground/spaceballs-quotes.txt"))

(defun insert-random-spaceballs-quote ()
  (interactive)
  (insert (get-random-element spaceballs-lines) "\n"))

(define-derived-mode spaceballs-mode fundamental-mode "spaceballs"
  "Major mode for generating random Spaceballs quotes.")

(provide 'spaceballs-mode)
