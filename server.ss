;;; -*- Gerbil -*-
;;; © vyzo
;;; The Gerbil Package Metadata Server

(import :std/net/httpd
        :std/sugar)
(export make-server)

(defstruct server ()
  final: #t)

(defmethod {get-handler server}
  (lambda (srv host path)
    default-handler))

(def (default-handler req res)
  (http-response-write res 404 '(("Content-Type" . "text/plain"))
    "these aren't the droids you are looking for.\n"))