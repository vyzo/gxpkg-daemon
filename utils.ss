(export string->sexp)

(def (string->sexp s)
  (read (open-input-string s)))
