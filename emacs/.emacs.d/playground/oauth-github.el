(require 'oauth2)
(require 'json)

(defvar token
  (make-oauth2-token :access-token "8825c8ddef45990182cf9c644ed294c2f1c3734a"))

(defvar user-data
  (with-current-buffer
      (oauth2-url-retrieve-synchronously token "https://api.github.com/user")
    (goto-char url-http-end-of-headers)
    (json-read)))
