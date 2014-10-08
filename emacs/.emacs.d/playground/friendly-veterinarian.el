;; A search for "ask for user input" returns something similar online...
(defun query-favorite-animal (name)
  "Queries the user for their favorite animal."
  (interactive "sEnter the name of your favorite animal: ")
  (message "Your favorite animal is: %s. I guess those are alright." name))

(defun query-favorite-animal-autocomplete (name)
  "Queries the user for their favorite animal with autocomplete."
  (interactive
   (list
    (completing-read "sEnter the name of your favorite animals: "
                     (get-lines-from-file "~/.emacs.d/playground/animals.txt"))))
  (message "Your favorite animal is: %s. I guess those are alright." name))

(defun get-lines-from-file (file-path)
  "Returns the lines of the file at file-path."
  ;; Insert the contents of the file into a temporary buffer
  (with-temp-buffer
    (insert-file-contents file-path)
    ;; Split the contents of the current buffer on the new line
    (split-string (buffer-string) "\n" t)))

(provide 'friendly-veterinarian)
