(flycheck-define-checker error-on-q
  "A syntax checker that always says that line 1 has an error."

  :command ("grep --color=never -n -i \"q\""
            source
            "| awk -F \":\" '{print $1\":Ick! Q! I hate that letter!\"}'")
  :error-patterns
  ((error line-start line ":" (message) line-end))
  :modes text-mode)

(provide 'flycheck-error-on-q)
