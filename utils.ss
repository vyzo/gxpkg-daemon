(import :std/pregexp
        :std/error)
(export read-string string->sexp)

(def (read-string s)
  (read (open-input-string s)))

(def (string->sexp s)
  (read-string s))
