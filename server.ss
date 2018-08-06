;;; -*- Gerbil -*-
;;; © vyzo
;;; server implementation

(import :std/net/httpd
        :std/sugar
        :std/misc/text
        (only-in :gerbil/gambit/ports object->string)
        :vyzo/gxpkgd/db
        :vyzo/gxpkgd/providers/github)
(export make-server)

(defstruct server (db)
  final: #t)

(defmethod {get-handler server}
  (lambda (srv host path)
    (cond
     ((equal? path "/")
      (cut / srv <> <>))
     ((equal? path "/packages")
      (cut /packages srv <> <>))
     ((string-prefix? path "/package/github/")
      (cut /package/github srv <> <>))
     ((string-prefix? path "/search/")
      (cut /search srv <> <>))
     (else
      default-handler))))

(defmethod {put-handler! server}
  void)

;; handlers
(def root-page (include-text "html/index.html"))

(def (/ srv req res)
  (http-response-write res 200 '(("Content-type" . "text/html"))
    root-page))

(def (/packages srv req res)
  (http-response-write res 500 '(("Content-Type" . "text/plain"))
    "/packages: Implement me!\n"))

;; GET /package/github/<author>/<package-name>
(def (/package/github srv req res)
  (try
   (github-handler (http-request-url req) (server-db srv) res)
   (catch (e)
     (raise e))))

(def (/search srv req res)
  (http-response-write res 500 '(("Content-Type" . "text/plain"))
    "/search: Implement me!\n"))

(def (default-handler req res)
  (http-response-write res 404 '(("Content-Type" . "text/plain"))
    "these aren't the droids you are looking for.\n"))
