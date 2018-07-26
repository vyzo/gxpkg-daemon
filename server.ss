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
    (cond
     ((equal? path "/")
      (cut / srv <> <>))
     ((equal? path "/packages")
      (cut /packages srv <> <>))
     ((string-prefix? path "/package/")
      (cut /package srv <> <>))
     ((string-prefix? path "/search/")
      (cut /search srv <> <>))
     (else
      default-handler))))

(defmethod {put-handler! server}
  void)

;; handlers
(def root-page #<<END
  This is the gerbil package metadata server

  JSON API:
  - /packages: list packages
  - /package/<canonical-package-name>:
    GET returns the package metadata
    POST publishes a new package to the server
  - /search/<query>: searches package metadata

END
)

(def (/ srv req res)
  (http-response-write res 200 '(("Content-type" . "text/plain"))
    root-page))

(def (/packages srv req res)
  (http-response-write res 500 '(("Content-Type" . "text/plain"))
    "/packages: Implement me!\n"))

(def (/package srv req res)
  (http-response-write res 500 '(("Content-Type" . "text/plain"))
    "/package: Implement me!\n"))

(def (/search srv req res)
  (http-response-write res 500 '(("Content-Type" . "text/plain"))
    "/search: Implement me!\n"))

(def (default-handler req res)
  (http-response-write res 404 '(("Content-Type" . "text/plain"))
    "these aren't the droids you are looking for.\n"))
