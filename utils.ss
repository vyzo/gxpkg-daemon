;;; -*- Gerbil -*-
;;; © vyzo belmarca
;;; github.com provider

(import :std/sugar)

(export string->sexp
        unless*)

(def (string->sexp s)
  (read (open-input-string s)))

;; like unless but for a's that throw exceptions.
(defrules unless* ()
  ((_ a b)
   (try a (catch (e) b))))
